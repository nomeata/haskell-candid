{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This (internal) module contains the core stuff; in particularly
-- everything that can be done without UndecidableInstances
--
-- Everything of interest is re-exported by "Codec.Candid".
module Codec.Candid.Core
    ( Type(..)
    , OtherT
    , Other(Other)
    , Fields
    , FieldName(N, H)
    , Candid(..)
    , CandidSeq(..)
    , CandidArg
    , Unary(..)
    , encodeT
    , encodeBuilderT
    , decodeT
    , encode
    , encodeBuilder
    , decode
    , Val
    , Record
    , Variant
    , Seq
    , KnownType
    , KnownArgs
    , KnownFields
    , typeVal
    , typesVal
    ) where

import Numeric.Natural
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.Map as M
import qualified Data.Row.Records as R
import qualified Data.Row.Internal as R
import qualified Data.Row.Variants as V
import Data.Proxy
import Data.Typeable
import Data.Bifunctor
import Data.Word
import Data.Int
import Data.List
import Data.Void
import Control.Monad.RWS.Lazy
import GHC.TypeLits
import Data.Serialize.LEB128
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.IEEE754 as G
import Data.Text.Prettyprint.Doc

import Codec.Candid.Tuples

-- |
-- Candid types
--
data Type
    -- prim types
    = NatT | Nat8T | Nat16T | Nat32T | Nat64T
    | IntT | Int8T | Int16T | Int32T | Int64T
    | Float32T | Float64T
    | BoolT
    | TextT
    | NullT
    | ReservedT
    | EmptyT
    -- constructors
    | OptT Type
    | VecT Type
    | RecT Fields
    | VariantT Fields
    -- reference
    | PrincipalT
    -- short-hands
    | BlobT
      -- ^ a short-hand for 'VecT' 'Nat8T'
    -- for recursive types
    | OtherT_ Other
      -- ^ internal, use 'OtherT' instead

-- | This allows embedding any type with a 'Candid' instance; this way you can express recursive types.
type OtherT (t :: *) = 'OtherT_ ('Other t)

-- | A wrapper for arbitrary other types with a Candid instance.
--
-- This allows recursive types
data Other where
    Other :: a -> Other
    Other_ :: Type -> Other -- ^ only on the value level

instance Pretty Type where
    pretty NatT = "nat"
    pretty Nat8T = "nat8"
    pretty Nat16T = "nat16"
    pretty Nat32T = "nat32"
    pretty Nat64T = "nat64"
    pretty IntT = "int"
    pretty Int8T = "int8"
    pretty Int16T = "int16"
    pretty Int32T = "int32"
    pretty Int64T = "int64"
    pretty Float32T = "float"
    pretty Float64T = "float"
    pretty BoolT = "bool"
    pretty TextT = "text"
    pretty NullT = "null"
    pretty ReservedT = "reserved"
    pretty EmptyT = "empty"
    pretty (OptT t) = "opt" <+> pretty t
    pretty (VecT t) = "vec" <+> pretty t
    pretty (RecT fs) = "record" <+> prettyFields False fs
    pretty (VariantT fs) = "variant" <+> prettyFields True fs
    pretty (OtherT_ (Other _)) = error "Other on term level"
    pretty BlobT = "blob"
    pretty PrincipalT = "principal"
    pretty (OtherT_ (Other_ t)) = pretty t

    prettyList = encloseSep lparen rparen (comma <> space) . map pretty

prettyFields :: Bool -> Fields -> Doc ann
prettyFields in_variant fs = braces $ hsep $ punctuate semi $ map (prettyField in_variant) fs

prettyField :: Bool -> (FieldName, Type) -> Doc ann
prettyField _ (N _, _) = error "N on term level"
prettyField True (N' n, NullT) = pretty n
prettyField _ (N' n, t) = pretty n <+> colon <+> pretty t -- TODO: encode field names
prettyField _ (H _, _) = error "H on term level"
prettyField True (H' n, NullT) = pretty n
prettyField _ (H' n, t) = pretty n <+> colon <+> pretty t

-- | The list of fields of a 'RecT' or 'VariantT'
type Fields = [(FieldName, Type)]

-- | A Candid fieldname
data FieldName
   = N Symbol -- ^ a properly named field
   | N' T.Text -- ^ Use this in terms (usually not needed)
   | H Nat -- ^ a field hash. Should fit in 32 bit. Also used for tuples 
   | H' Word32 -- ^ Use this in terms (mostly internal)

instance Show FieldName where show = prettyFieldName

prettyFieldName :: FieldName -> String
prettyFieldName (N _) = error "Named in term"
prettyFieldName (N' t) = T.unpack t
prettyFieldName (H _) = error "Nat in term"
prettyFieldName (H' n) = show n

type Args = [Type]

-- The values
type family Val (t :: Type) where
    Val 'NatT = Natural
    Val 'Nat8T = Word8
    Val 'Nat16T = Word16
    Val 'Nat32T = Word32
    Val 'Nat64T = Word64
    Val 'IntT = Integer
    Val 'Int8T = Int8
    Val 'Int16T = Int16
    Val 'Int32T = Int32
    Val 'Int64T = Int64
    Val 'Float32T = Float
    Val 'Float64T = Double
    Val 'BoolT = Bool
    Val 'TextT = T.Text
    Val 'NullT = ()
    Val 'ReservedT = ()
    Val 'EmptyT = Void
    Val ('OptT t) = Maybe (Val t)
    Val ('VecT t) = V.Vector (Val t)
    Val ('RecT fs) = Record fs
    Val ('VariantT fs) = Variant fs
    Val 'PrincipalT = BS.ByteString
    Val 'BlobT = BS.ByteString
    Val ('OtherT_ ('Other t)) = t

type family Seq (ts :: [Type]) where
    Seq '[] = ()
    Seq (t ': ts) = (Val t, Seq ts)

type family Record (fs :: Fields) where
    Record '[] = ()
    Record ('(f,t)':fs) = (Val t, Record fs)

type family Variant (fs :: [(FieldName, Type)]) where
    Variant '[] = Void
    Variant ('(f,t) ': fs) = Either (Val t) (Variant fs)

-- | Encode, given a Candid 'Type' (best used with @TypeApplications@)
encodeT :: forall ts. KnownArgs ts => Seq ts -> BS.ByteString
encodeT = BSL.toStrict . B.toLazyByteString . encodeBuilderT @ts

-- | Encode based on Haskell type
encode :: CandidArg a => a -> BS.ByteString
encode = BSL.toStrict . B.toLazyByteString . encodeBuilder

-- | Encode to a 'B.Builder', given a Candid 'Type' (best used with @TypeApplications@)
encodeBuilderT :: forall ts. KnownArgs ts => Seq ts -> B.Builder
encodeBuilderT x = mconcat
    [ B.stringUtf8 "DIDL"
    , typTable (args @ts)
    , encodeSeq (args @ts) x
    ]

-- | Encode to a 'B.Builder' based on Haskell type
encodeBuilder :: forall a. CandidArg a => a -> B.Builder
encodeBuilder x = encodeBuilderT @(ArgRep (AsTuple a)) (toSeq (asTuple x))

encodeSeq :: SArgs ts -> Seq ts -> B.Builder
encodeSeq SArgsNil () = mempty
encodeSeq (SArgsCons t ts) (x, xs) = encodeVal t x <> encodeSeq ts xs

encodeVal :: SType t -> Val t -> B.Builder
encodeVal SEmptyT v = case v of {}
encodeVal SBoolT False = B.word8 0
encodeVal SBoolT True = B.word8 1
encodeVal SNatT n = buildLEB128 n
encodeVal SNat8T n = B.word8 n
encodeVal SNat16T n = B.word16LE n
encodeVal SNat32T n = B.word32LE n
encodeVal SNat64T n = B.word64LE n
encodeVal SIntT n = buildSLEB128 n
encodeVal SInt8T n = B.int8 n
encodeVal SInt16T n = B.int16LE n
encodeVal SInt32T n = B.int32LE n
encodeVal SInt64T n = B.int64LE n
encodeVal SFloat32T n = B.floatLE n
encodeVal SFloat64T n = B.doubleLE n
encodeVal STextT t = encodeBytes (T.encodeUtf8 t)
encodeVal SNullT () = mempty
encodeVal SReservedT () = mempty
encodeVal (SOptT _) Nothing = B.word8 0
encodeVal (SOptT t) (Just x) = B.word8 1 <> encodeVal t x
encodeVal (SVecT t) xs =
    buildLEB128Int (V.length xs) <>
    foldMap (encodeVal t) xs
encodeVal (SRecT fs) r =
    foldMap snd $ sortOn fst $ encodeRec fs r
encodeVal (SVariantT fs) x =
    buildLEB128Int i <> b
  where
    (pos, b) = encodeVariant fs x
    m = map fst $ sortOn snd $ zip [0..] (map (hashFieldName . fst) (fromSFields fs))
    Just i = elemIndex pos m
encodeVal SPrincipalT s = B.int8 1 <> encodeBytes s
encodeVal SBlobT b = encodeBytes b
encodeVal (SOtherT st) x = encodeVal st (toCandid x)

encodeBytes :: BS.ByteString -> B.Builder
encodeBytes bytes = buildLEB128Int (BS.length bytes) <> B.byteString bytes

-- Encodes the fields, sorting happens later
encodeRec :: SFields fs -> Record fs -> [(Word32, B.Builder)]
encodeRec SFieldsNil () = []
encodeRec (SFieldsCons n t fs) (x, xs) =
    (hashFieldName (fromSFieldName n), encodeVal t x) : encodeRec fs xs

-- Encodes the value, returns fields and index
encodeVariant :: SFields fs -> Variant fs -> (Natural, B.Builder)
encodeVariant SFieldsNil x = case x of {}
encodeVariant (SFieldsCons _ t _) (Left x) = (0, encodeVal t x)
encodeVariant (SFieldsCons _ _ fs) (Right v) = first succ (encodeVariant fs v)

type TypTableBuilder = RWS () B.Builder (M.Map TypeRep Integer, Natural)
typTable :: Typeable fs => SArgs fs -> B.Builder
typTable ts = mconcat
    [ buildLEB128 typ_tbl_len
    , typ_tbl
    , buildLEB128 (lenSArgs ts)
    , foldMap buildSLEB128 typ_idxs
    ]
  where
    (typ_idxs, (_, typ_tbl_len), typ_tbl) = runRWS (goArgs ts) () (M.empty, 0)

    addCon :: forall (t :: Type). Typeable t => TypTableBuilder B.Builder -> TypTableBuilder Integer
    addCon body = gets (M.lookup n . fst) >>= \case
        Just i -> return i
        Nothing -> mdo
            i <- gets snd
            modify' (first (M.insert n (fromIntegral i)))
            modify' (second succ)
            tell b
            b <- body
            return $ fromIntegral i
      where n = typeRep (Proxy @t)

    goArgs :: SArgs ts -> TypTableBuilder [Integer]
    goArgs SArgsNil = return []
    goArgs (SArgsCons t ts) = (:) <$> go t <*> goArgs ts

    go :: forall t. Typeable t => SType t -> TypTableBuilder Integer

    go SNullT     = return $ -1
    go SBoolT     = return $ -2
    go SNatT      = return $ -3
    go SIntT      = return $ -4
    go SNat8T     = return $ -5
    go SNat16T    = return $ -6
    go SNat32T    = return $ -7
    go SNat64T    = return $ -8
    go SInt8T     = return $ -9
    go SInt16T    = return $ -10
    go SInt32T    = return $ -11
    go SInt64T    = return $ -12
    go SFloat32T  = return $ -13
    go SFloat64T  = return $ -14
    go STextT     = return $ -15
    go SReservedT = return $ -16
    go SEmptyT    = return $ -17

    -- Constructors
    go (SOptT t) = addCon @t $ do
        ti <- go t
        return $ buildSLEB128 @Integer (-18) <> buildSLEB128 ti
    go (SVecT t) = addCon @t $ do
        ti <- go t
        return $ buildSLEB128 @Integer (-19) <> buildSLEB128 ti
    go (SRecT fs) = addCon @t $ recordLike (-20) fs
    go (SVariantT fs) = addCon @t $ recordLike (-21) fs

    -- References
    go SPrincipalT = return $ -24

    -- Short-hands
    go SBlobT = addCon @t $
        -- blob = vec nat8
        return $ buildSLEB128 @Integer (-19) <> buildSLEB128 @Integer (-5)

    -- Other types
    go (SOtherT st) = go st

    goFields :: SFields fs -> TypTableBuilder [(Word32, Integer)]
    goFields SFieldsNil = return []
    goFields (SFieldsCons fn t fs) = do
        ti <- go t
        tis <- goFields fs
        return $ (hashFieldName (fromSFieldName fn), ti) : tis

    recordLike :: Integer -> SFields fs -> TypTableBuilder B.Builder
    recordLike n fs = do
        tis <- goFields fs
        return $ mconcat
            [ buildSLEB128 n
            , leb128Len tis
            , foldMap (\(n,ti) -> buildLEB128 n <> buildSLEB128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]


-- | Decode, given a Candid 'Type' (best used with @TypeApplications@)
decodeT :: forall ts. KnownArgs ts => BS.ByteString -> Either String (Seq ts)
decodeT = G.runGet $ do
    decodeMagic
    arg_tys <- decodeTypTable
    decodeParams (args @ts) arg_tys

-- | Decode based on Haskell type
decode :: forall a. CandidArg a => BS.ByteString -> Either String a
decode bytes = fromTuple . fromSeq <$> decodeT @(ArgRep (AsTuple a)) bytes

decodeParams :: SArgs ts -> [Type] -> G.Get (Seq ts)
decodeParams SArgsNil _ = return () -- NB: This is where we ignore extra arguments
decodeParams SArgsCons{} [] = fail "Missing argument"
decodeParams (SArgsCons st sts) (t' : ts) = do
    v <- decodeVal st t'
    vs <- decodeParams sts ts
    return (v, vs)

decodeVal :: SType t -> Type -> G.Get (Val t)
decodeVal SBoolT BoolT = G.getWord8 >>= \case
    0 -> return False
    1 -> return True
    _ -> fail "Invalid boolean value"
decodeVal SNatT NatT = getLEB128
decodeVal SNat8T Nat8T = G.getWord8
decodeVal SNat16T Nat16T = G.getWord16le
decodeVal SNat32T Nat32T = G.getWord32le
decodeVal SNat64T Nat64T = G.getWord64le
decodeVal SIntT NatT = fromIntegral <$> getLEB128 @Natural
decodeVal SIntT IntT = getSLEB128
decodeVal SInt8T Int8T = G.getInt8
decodeVal SInt16T Int16T = G.getInt16le
decodeVal SInt32T Int32T = G.getInt32le
decodeVal SInt64T Int64T = G.getInt64le
decodeVal SFloat32T Float32T = G.getFloat32le
decodeVal SFloat64T Float64T = G.getFloat64le
decodeVal STextT TextT = do
    bs <- decodeBytes
    case T.decodeUtf8' bs of
        Left err -> fail (show err)
        Right t -> return t
decodeVal SNullT NullT = return ()
decodeVal SReservedT t = skipVal t
decodeVal (SOptT _) NullT = return Nothing
decodeVal (SOptT st) (OptT t) = G.getWord8 >>= \case
    0 -> return Nothing
    1 -> Just <$> decodeVal st t
    _ -> fail "Invalid optional value"
decodeVal (SVecT st) (VecT t) = do
    n <- getLEB128Int
    V.fromList <$> replicateM n (decodeVal st t)
decodeVal (SRecT sfs) (RecT fs) = decodeRec sfs fs
decodeVal (SVariantT sfs) (VariantT fs) = do
        i <- getLEB128Int
        unless (i <= length fs) $ fail "variant index out of bound"
        let (fn, t) = fs !! i
        decodeVariant sfs (hashFieldName fn) t
decodeVal (SOtherT st) t = fromCandid <$> decodeVal st t
decodeVal SPrincipalT PrincipalT = G.getWord8 >>= \case
    0 -> fail "reference encountered"
    1 -> decodeBytes
    _ -> fail "Invalid principal value"
decodeVal SBlobT (VecT Nat8T) = decodeBytes
decodeVal s t = fail $ "unexpected type " ++ take 20 (show (pretty t)) ++  " when decoding " ++ take 20 (show s)

decodeRec :: SFields fs -> Fields -> G.Get (Record fs)
decodeRec SFieldsNil [] = return ()
decodeRec (SFieldsCons fn _ _) [] = fail $ "missing field " <> prettyFieldName (fromSFieldName fn)
decodeRec sfs ((h,t):dfs) =
    findField sfs (hashFieldName h)
        (skipVal t >> decodeRec sfs dfs)
        (\st' sfs' sortIn -> do
            x <- decodeVal st' t
            xs <- decodeRec sfs' dfs
            return (x `sortIn` xs))

decodeBytes :: G.Get BS.ByteString
decodeBytes = getLEB128Int >>= G.getByteString

-- findField, in CPS style, produces a function with a type
-- that inserts the value in the right slot in the nested pairs
findField :: SFields fs ->
        Word32 ->
        a ->
        (forall t' fs'.
            SType t' -> SFields fs' ->
            (Val t' -> Record fs' -> Record fs) -> a
        ) ->
        a
findField SFieldsNil _ k1 _ = k1
findField (SFieldsCons fn st sfs) h k1 k2
    | h == hashFieldName (fromSFieldName fn) = k2 st sfs (,)
    | otherwise = findField sfs h k1 $ \st' sfs' sortIn ->
        k2 st' (SFieldsCons fn st sfs') (\x (y, ys) -> (y, x `sortIn` ys))

decodeVariant :: SFields fs -> Word32 -> Type -> G.Get (Variant fs)
decodeVariant SFieldsNil _ _ = fail "unexpected variant tag"
decodeVariant (SFieldsCons fn st sfs) h t
        | h == hashFieldName (fromSFieldName fn) = Left <$> decodeVal st t
        | otherwise = Right <$> decodeVariant sfs h t

skipVal :: Type -> G.Get ()
skipVal (OtherT_ (Other _ )) = error "Other on term level"
skipVal (OtherT_ (Other_ t)) = skipVal t
skipVal BoolT = G.skip 1
skipVal NatT = void (getLEB128 @Natural)
skipVal Nat8T = G.skip 1
skipVal Nat16T = G.skip 2
skipVal Nat32T = G.skip 4
skipVal Nat64T = G.skip 8
skipVal IntT = void (getSLEB128 @Integer)
skipVal Int8T = G.skip 1
skipVal Int16T = G.skip 2
skipVal Int32T = G.skip 4
skipVal Int64T = G.skip 8
skipVal Float32T = G.skip 4
skipVal Float64T = G.skip 8
skipVal TextT = skipBytes
skipVal NullT = return ()
skipVal ReservedT = return ()
skipVal EmptyT = fail "skipping empty value"
skipVal (OptT t) = G.getWord8 >>= \case
    0 -> return ()
    1 -> skipVal t
    _ -> fail "Invalid optional value"
skipVal (VecT t) = do
    n <- getLEB128Int
    replicateM_ n (skipVal t)
skipVal (RecT fs) = mapM_ (skipVal . snd) fs
skipVal (VariantT fs) = do
    i <- getLEB128Int
    unless (i <= length fs) $ fail "variant index out of bound"
    let (_fn, t) = fs !! i
    skipVal t
skipVal PrincipalT = G.getWord8 >>= \case
    0 -> fail "reference encountered"
    1 -> skipBytes
    _ -> fail "Invalid principal value"
skipVal BlobT = skipBytes

skipBytes :: G.Get ()
skipBytes = getLEB128Int >>= G.skip


decodeMagic :: G.Get ()
decodeMagic = do
    magic <- G.getBytes 4
    unless (magic == T.encodeUtf8 (T.pack "DIDL")) $ fail "Expected magic bytes \"DIDL\""

getLEB128Int :: G.Get Int
getLEB128Int = fromIntegral <$> getLEB128 @Natural

decodeSeq :: G.Get a -> G.Get [a]
decodeSeq act = do
    len <- getLEB128Int
    replicateM len act

decodeTypTable :: G.Get [Type]
decodeTypTable = do
    -- typ table
    len <- getLEB128
    pre_table <- V.fromList <$> replicateM (fromIntegral len) (decodeTypTableEntry len)
    -- tie the know
    let table = fmap ($ table) pre_table
    -- argument list
    map ($ table) <$> decodeSeq (decodeTypRef len)

decodeTypTableEntry :: Natural -> G.Get (V.Vector Type -> Type)
decodeTypTableEntry max = getSLEB128 @Integer >>= \case
    -18 -> (OptT <$>) <$> decodeTypRef max
    -19 -> (VecT <$>) <$> decodeTypRef max
    -20 -> (RecT <$>) <$> decodeTypFields max
    -21 -> (VariantT <$>) <$> decodeTypFields max
    _ -> fail "Unknown structural type"

decodeTypRef :: Natural -> G.Get (V.Vector Type -> Type)
decodeTypRef max = do
    i <- getSLEB128
    when (i > fromIntegral max) $ fail "Type reference out of range"
    if i < 0
    then case primTyp i of
        Just t -> return $ const t
        Nothing -> fail  $ "Unknown prim typ " ++ show i
    else return $ \v ->v V.! fromIntegral i

decodeTypFields :: Natural -> G.Get (V.Vector Type -> Fields)
decodeTypFields max = sequence <$> decodeSeq (decodeTypField max)

decodeTypField :: Natural -> G.Get (V.Vector Type -> (FieldName, Type))
decodeTypField max = do
    h <- getLEB128
    t <- decodeTypRef max
    return $ (H' h,) <$> t

primTyp :: Integer -> Maybe Type
primTyp (-1)  = Just NullT
primTyp (-2)  = Just BoolT
primTyp (-3)  = Just NatT
primTyp (-4)  = Just IntT
primTyp (-5)  = Just Nat8T
primTyp (-6)  = Just Nat16T
primTyp (-7)  = Just Nat32T
primTyp (-8)  = Just Nat64T
primTyp (-9)  = Just Int8T
primTyp (-10) = Just Int16T
primTyp (-11) = Just Int32T
primTyp (-12) = Just Int64T
primTyp (-13) = Just Float32T
primTyp (-14) = Just Float64T
primTyp (-15) = Just TextT
primTyp (-16) = Just ReservedT
primTyp (-17) = Just EmptyT
primTyp (-24) = Just PrincipalT
primTyp _     = Nothing


hashFieldName :: FieldName -> Word32
hashFieldName (H _) = error "Nat on value level"
hashFieldName (H' n) = n
hashFieldName (N _) = error "Symbol in value level computation"
hashFieldName (N' s) =
    BS.foldl (\h c -> (h * 223 + fromIntegral c)) 0 $ T.encodeUtf8 s

buildLEB128Int :: Int -> B.Builder
buildLEB128Int = buildLEB128 @Natural . fromIntegral

leb128Len :: [a] -> B.Builder
leb128Len = buildLEB128Int . length

-- Using normal Haskell values

-- | The class of types that can be used as Candid argument sequences.
-- Essentially all types that are in 'Candid', but tuples need to be treated specially.
type CandidArg a = (CandidSeq (AsTuple a), Tuplable a)

class KnownArgs (ArgRep a) => CandidSeq a where
    type ArgRep a :: [Type]
    toSeq :: a -> Seq (ArgRep a)
    fromSeq :: Seq (ArgRep a) -> a

    default toSeq :: Seq (ArgRep a) ~ a => a -> Seq (ArgRep a)
    toSeq = id
    default fromSeq :: Seq (ArgRep a) ~ a => Seq (ArgRep a) -> a
    fromSeq = id

instance CandidSeq () where
    type ArgRep () = '[]

instance Candid a => CandidSeq (Unary a) where
    type ArgRep (Unary a) = '[Rep a]
    toSeq (Unary x) = (toCandid x, ())
    fromSeq (x, ()) = Unary $ fromCandid x

instance (Candid a, Candid b) => CandidSeq (a, b) where
    type ArgRep (a, b) = '[Rep a, Rep b]
    toSeq (x,y) = (toCandid x, (toCandid y, ()))
    fromSeq (x, (y, ())) = (fromCandid x, fromCandid y)

instance (Candid a, Candid b, Candid c, Candid d, Candid e, Candid f) => CandidSeq (a, b, c, d, e, f) where
    type ArgRep (a, b, c, d, e, f) = '[ Rep a, Rep b, Rep c, Rep d, Rep e, Rep f]
    toSeq (x1,x2,x3,x4,x5,x6) =
        x1 & x2 & x3 & x4 & x5 & x6 & ()
      where
        infixr &
        x & r = (toCandid x, r)
    fromSeq (x1, (x2, (x3, (x4, (x5, (x6, ())))))) =
        (fromCandid x1, fromCandid x2, fromCandid x3, fromCandid x4, fromCandid x5, fromCandid x6)


-- | The class of Haskell types that can be converted to Candid.
--
-- You can create intances of this class for your own types; see the overview above for examples. The default instance is mostly for internal use.
class (Typeable a, KnownType (Rep a)) => Candid a where
    type Rep a :: Type
    toCandid :: a -> Val (Rep a)
    fromCandid :: Val (Rep a) -> a

    default toCandid :: Val (Rep a) ~ a => a -> Val (Rep a)
    toCandid = id
    default fromCandid :: Val (Rep a) ~ a => Val (Rep a) -> a
    fromCandid = id

instance Candid Bool where type Rep Bool = 'BoolT
instance Candid Natural where type Rep Natural = 'NatT
instance Candid Word8 where type Rep Word8 = 'Nat8T
instance Candid Word16 where type Rep Word16 = 'Nat16T
instance Candid Word32 where type Rep Word32 = 'Nat32T
instance Candid Word64 where type Rep Word64 = 'Nat64T
instance Candid Integer where type Rep Integer = 'IntT
instance Candid Int8 where type Rep Int8 = 'Int8T
instance Candid Int16 where type Rep Int16 = 'Int16T
instance Candid Int32 where type Rep Int32 = 'Int32T
instance Candid Int64 where type Rep Int64 = 'Int64T
instance Candid Float where type Rep Float = 'Float32T
instance Candid Double where type Rep Double = 'Float64T
instance Candid Void where type Rep Void = 'EmptyT
instance Candid T.Text where type Rep T.Text = 'TextT
instance Candid BS.ByteString where type Rep BS.ByteString = 'BlobT

instance Candid BSL.ByteString where
    type Rep BSL.ByteString = 'BlobT
    toCandid = BSL.toStrict
    fromCandid = BSL.fromStrict

{-
instance Candid String where
    type Rep String = 'TextT
    toCandid = toCandid . T.pack
    fromCandid = T.unpack . fromCandid
-}

instance Candid a => Candid (Maybe a) where
    type Rep (Maybe a) = 'OptT (Rep a)
    toCandid = fmap toCandid
    fromCandid = fmap fromCandid

instance Candid a => Candid (V.Vector a) where
    type Rep (V.Vector a) = 'VecT (Rep a)
    toCandid = fmap toCandid
    fromCandid = fmap fromCandid

instance Candid a => Candid [a] where
    type Rep [a] = 'VecT (Rep a)
    toCandid = fmap toCandid . V.fromList
    fromCandid = V.toList <$> fmap fromCandid

-- | Maybe a bit opinionated, but 'null' seems to be the unit of Candid
instance Candid () where
    type Rep () = 'NullT

instance (Candid a, Candid b) => Candid (a, b) where
    type Rep (a, b) = 'RecT '[ TupField 0 a, TupField 1 b]
    toCandid (x,y) = (toCandid x, (toCandid y, ()))
    fromCandid (x, (y, ())) = (fromCandid x, fromCandid y)

instance (Candid a, Candid b, Candid c) => Candid (a, b, c) where
    type Rep (a, b, c) = 'RecT '[ TupField 0 a, TupField 1 b, TupField 2 c]
    toCandid (x,y,z) = (toCandid x, (toCandid y, (toCandid z, ())))
    fromCandid (x, (y, (z, ()))) = (fromCandid x, fromCandid y, fromCandid z)

instance (Candid a, Candid b, Candid c, Candid d, Candid e, Candid f) => Candid (a, b, c, d, e, f) where
    type Rep (a, b, c, d, e, f) = 'RecT '[ TupField 0 a, TupField 1 b, TupField 2 c, TupField 3 d, TupField 4 e, TupField 5 f]
    toCandid (x1,x2,x3,x4,x5,x6) =
        x1 & x2 & x3 & x4 & x5 & x6 & ()
      where
        infixr &
        x & r = (toCandid x, r)
    fromCandid (x1, (x2, (x3, (x4, (x5, (x6, ())))))) =
        (fromCandid x1, fromCandid x2, fromCandid x3, fromCandid x4, fromCandid x5, fromCandid x6)

type TupField n a = '( 'H n, Rep a)

instance (Candid a, Candid b) => Candid (Either a b) where
    type Rep (Either a b) = 'VariantT '[ '( 'N "Left", Rep a), '( 'N "Right", Rep b) ]
    toCandid (Left x) = Left (toCandid x)
    toCandid (Right x) = Right (Left (toCandid x))
    fromCandid (Left x) = Left (fromCandid x)
    fromCandid (Right (Left x)) = Right (fromCandid x)
    fromCandid (Right (Right x)) = case x of {}

-- row-types integration

class KnownFields (RecRep r) => FromRowRec r where
    type RecRep r :: Fields
    fromRowRec :: R.Rec r -> Val ('RecT (RecRep r))
    toRowRec :: Val ('RecT (RecRep r)) -> R.Rec r

instance FromRowRec ('R.R '[]) where
    type RecRep ('R.R '[]) = '[]
    fromRowRec _ = ()
    toRowRec _ = R.empty

instance (Candid t, R.KnownSymbol f, FromRowRec ('R.R xs)) => FromRowRec ('R.R (f 'R.:-> t ': xs)) where
    type RecRep ('R.R (f 'R.:-> t ': xs)) = '( 'N f, Rep t) ': RecRep ('R.R xs)
    fromRowRec r = (toCandid (r R..! l), fromRowRec (r R..- l))
      where l = R.Label @f
    toRowRec (x, xs) = R.unsafeInjectFront l (fromCandid @t x) (toRowRec @('R.R xs) xs)
      where l = R.Label @f

instance (FromRowRec r, Typeable r) => Candid (R.Rec r) where
    type Rep (R.Rec r) = 'RecT (RecRep r)
    toCandid = fromRowRec
    fromCandid = toRowRec

class KnownFields (VarRep r) => FromRowVar r where
    type VarRep r :: Fields
    fromRowVar :: V.Var r -> Val ('VariantT (VarRep r))
    toRowVar :: Val ('VariantT (VarRep r)) -> V.Var r

instance FromRowVar ('R.R '[]) where
    type VarRep ('R.R '[]) = '[]
    fromRowVar = V.impossible
    toRowVar = absurd

instance (Candid t, R.KnownSymbol f, FromRowVar ('R.R xs)) => FromRowVar ('R.R (f 'R.:-> t ': xs)) where
    type VarRep ('R.R (f 'R.:-> t ': xs)) = '( 'N f, Rep t) ': VarRep ('R.R xs)
    fromRowVar r = case V.trial r l of
        Left x -> Left (toCandid x)
        Right r' -> Right (fromRowVar @('R.R xs) r')
      where l = R.Label @f
    toRowVar (Left x) = V.unsafeMakeVar l (fromCandid x)
      where l = R.Label @f
    toRowVar (Right xs) = V.unsafeInjectFront (toRowVar @('R.R xs) xs)

instance (FromRowVar r, Typeable r) => Candid (V.Var r) where
    type Rep (V.Var r) = 'VariantT (VarRep r)
    toCandid = fromRowVar
    fromCandid = toRowVar



-- Repetitive stuff for dependently typed programming

-- | Corresponding singleton family

data SType (t :: Type) where
    SNatT :: SType 'NatT
    SNat8T :: SType 'Nat8T
    SNat16T :: SType 'Nat16T
    SNat32T :: SType 'Nat32T
    SNat64T :: SType 'Nat64T
    SIntT :: SType 'IntT
    SInt8T :: SType 'Int8T
    SInt16T :: SType 'Int16T
    SInt32T :: SType 'Int32T
    SInt64T :: SType 'Int64T
    SFloat32T :: SType 'Float32T
    SFloat64T :: SType 'Float64T
    SBoolT :: SType 'BoolT
    STextT :: SType 'TextT
    SNullT :: SType 'NullT
    SReservedT :: SType 'ReservedT
    SEmptyT :: SType 'EmptyT
    SOptT :: Typeable t => SType t -> SType ('OptT t)
    SVecT :: Typeable t => SType t -> SType ('VecT t)
    SRecT :: SFields fs -> SType ('RecT fs)
    SVariantT :: SFields fs -> SType ('VariantT fs)
    SPrincipalT :: SType 'PrincipalT
    SBlobT :: SType 'BlobT
    SOtherT :: forall a. Candid a => SType (Rep a) -> SType ('OtherT_ ('Other a))

deriving instance Show (SType t)

data SFields (fs :: Fields) where
    SFieldsNil :: SFields '[]
    SFieldsCons :: Typeable t => SFieldName n -> SType t -> SFields fs -> SFields ('(n, t) ': fs)

deriving instance Show (SFields fs)

data SArgs (t :: Args) where
    SArgsNil :: SArgs '[]
    SArgsCons :: Typeable t => SType t -> SArgs fs -> SArgs (t ': fs)
deriving instance Show (SArgs t)

data SFieldName (n :: FieldName) where
    SN :: KnownSymbol s => Proxy s -> SFieldName ('N s)
    SH :: KnownNat n => Proxy n -> SFieldName ('H n)
deriving instance Show (SFieldName n)

fromSType :: SType t -> Type
fromSType SNatT = NatT
fromSType SNat8T = Nat8T
fromSType SNat16T = Nat16T
fromSType SNat32T = Nat32T
fromSType SNat64T = Nat64T
fromSType SIntT = IntT
fromSType SInt8T = Int8T
fromSType SInt16T = Int16T
fromSType SInt32T = Int32T
fromSType SInt64T = Int64T
fromSType SFloat32T = Float32T
fromSType SFloat64T = Float64T
fromSType SBoolT = BoolT
fromSType STextT = TextT
fromSType SNullT = NullT
fromSType SReservedT = ReservedT
fromSType SEmptyT = EmptyT
fromSType (SOptT t) = OptT (fromSType t)
fromSType (SVecT t) = VecT (fromSType t)
fromSType (SRecT fs) = RecT (fromSFields fs)
fromSType (SVariantT fs) = VariantT (fromSFields fs)
fromSType SPrincipalT = PrincipalT
fromSType SBlobT = BlobT
fromSType (SOtherT st) = OtherT_ (Other_ (fromSType st))

fromSFields :: SFields fs -> Fields
fromSFields SFieldsNil = []
fromSFields (SFieldsCons n t fs) = (fromSFieldName n, fromSType t) : fromSFields fs

fromSArgs :: SArgs fs -> Args
fromSArgs SArgsNil = []
fromSArgs (SArgsCons t fs) = fromSType t : fromSArgs fs

lenSArgs :: SArgs fs -> Natural
lenSArgs SArgsNil = 0
lenSArgs (SArgsCons _ fs) = succ (lenSArgs fs)

fromSFieldName :: SFieldName n -> FieldName
fromSFieldName (SN p) = N' (T.pack (symbolVal p))
fromSFieldName (SH p) = H' (fromIntegral (natVal p))


class Typeable t => KnownType (t :: Type) where typ :: SType t

instance KnownType 'NatT where typ = SNatT
instance KnownType 'Nat8T where typ = SNat8T
instance KnownType 'Nat16T where typ = SNat16T
instance KnownType 'Nat32T where typ = SNat32T
instance KnownType 'Nat64T where typ = SNat64T
instance KnownType 'IntT where typ = SIntT
instance KnownType 'Int8T where typ = SInt8T
instance KnownType 'Int16T where typ = SInt16T
instance KnownType 'Int32T where typ = SInt32T
instance KnownType 'Int64T where typ = SInt64T
instance KnownType 'Float32T where typ = SFloat32T
instance KnownType 'Float64T where typ = SFloat64T
instance KnownType 'TextT where typ = STextT
instance KnownType 'BoolT where typ = SBoolT
instance KnownType 'NullT where typ = SNullT
instance KnownType 'ReservedT where typ = SReservedT
instance KnownType 'EmptyT where typ = SEmptyT
instance KnownType t => KnownType ('OptT t) where typ = SOptT (typ @t)
instance KnownType t => KnownType ('VecT t) where typ = SVecT (typ @t)
instance KnownFields fs => KnownType ('RecT fs) where typ = SRecT (fields @fs)
instance KnownFields fs => KnownType ('VariantT fs) where typ = SVariantT (fields @fs)
instance KnownType 'PrincipalT where typ = SPrincipalT
instance KnownType 'BlobT where typ = SBlobT
instance Candid a => KnownType ('OtherT_ ('Other a)) where typ = SOtherT (typ @(Rep a))

class Typeable fs => KnownFields (fs :: [(FieldName, Type)]) where fields :: SFields fs
instance KnownFields '[] where fields = SFieldsNil
instance (KnownFieldName n, KnownType t, KnownFields fs) => KnownFields ('(n,t) ': fs) where
    fields = SFieldsCons (fieldName @n) (typ @t) (fields @fs)

class Typeable t => KnownArgs (t :: [Type]) where args :: SArgs t
instance KnownArgs '[] where args = SArgsNil
instance (KnownType t, KnownArgs ts) => KnownArgs (t ': ts) where
    args = SArgsCons (typ @t) (args @ts)

-- | Takes a Candid type from the type level to the value level, e.g. for pretty-printing with 'pretty'
typeVal :: forall t. KnownType t => Type
typeVal = fromSType (typ @t)

-- | Takes Candid types from the type level to the value level, e.g. for pretty-printing with 'pretty'
typesVal :: forall ts. KnownArgs ts => [Type]
typesVal = fromSArgs (args @ts)

class Typeable fn => KnownFieldName (fn :: FieldName) where fieldName :: SFieldName fn
instance KnownSymbol s => KnownFieldName ('N s) where
    fieldName = SN (Proxy @s)
instance KnownNat s => KnownFieldName ('H s) where
    fieldName = SH (Proxy @s)

