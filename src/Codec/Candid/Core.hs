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
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-orphans #-}
-- | This (internal) module contains the core stuff; in particularly
-- everything that can be done without UndecidableInstances
--
-- Everything of interest is re-exported by "Codec.Candid".
module Codec.Candid.Core
    ( Candid(..)
    , asType'
    , CandidSeq(..)
    , CandidArg
    , typeDesc
    , TypeDesc
    , tieKnot
    , Unary(..)
    , encode
    , encodeBuilder
    , decode
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
import Control.Monad.State.Lazy
import Data.Proxy
import Data.Typeable
import Data.Bifunctor
import Data.Word
import Data.Int
import Data.List
import Data.Void
import Control.Monad.RWS.Lazy
import Data.Serialize.LEB128
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.IEEE754 as G
import Data.Text.Prettyprint.Doc

import Codec.Candid.Tuples
import Codec.Candid.Data
import Codec.Candid.Types

-- | Encode based on Haskell type
encode :: CandidArg a => a -> BS.ByteString
encode = BSL.toStrict . B.toLazyByteString . encodeBuilder

-- | Encode to a 'B.Builder' based on Haskell type
encodeBuilder :: forall a. CandidArg a => a -> B.Builder
encodeBuilder x = mconcat
    [ B.stringUtf8 "DIDL"
    , typTable t
    , encodeSeq (tieKnot t) (seqVal (asTuple x))
    ]
  where
    t = typeDesc @a

encodeSeq :: [Type Void] -> [Value] -> B.Builder
encodeSeq [] _ = mempty -- NB: Subtyping
encodeSeq (t:ts) (x:xs) = encodeVal t x <> encodeSeq ts xs
encodeSeq _ [] = error "encodeSeq: Not enough values"

encodeVal :: Type Void -> Value -> B.Builder
encodeVal BoolT (BoolV False) = B.word8 0
encodeVal BoolT (BoolV True) = B.word8 1
encodeVal NatT (NatV n) = buildLEB128 n
encodeVal Nat8T (Nat8V n) = B.word8 n
encodeVal Nat16T (Nat16V n) = B.word16LE n
encodeVal Nat32T (Nat32V n) = B.word32LE n
encodeVal Nat64T (Nat64V n) = B.word64LE n
encodeVal IntT (NatV n) = buildSLEB128 (fromIntegral n :: Integer) -- NB Subtyping
encodeVal IntT (IntV n) = buildSLEB128 n
encodeVal Int8T (Int8V n) = B.int8 n
encodeVal Int16T (Int16V n) = B.int16LE n
encodeVal Int32T (Int32V n) = B.int32LE n
encodeVal Int64T (Int64V n) = B.int64LE n
encodeVal Float32T (Float32V n) = B.floatLE n
encodeVal Float64T (Float64V n) = B.doubleLE n
encodeVal TextT (TextV t) = encodeBytes (T.encodeUtf8 t)
encodeVal NullT NullV = mempty
encodeVal ReservedT _ = mempty -- NB Subtyping
encodeVal (OptT _) (OptV Nothing) = B.word8 0
encodeVal (OptT t) (OptV (Just x)) = B.word8 1 <> encodeVal t x
encodeVal (VecT t) (VecV xs) =
    buildLEB128Int (V.length xs) <>
    foldMap (encodeVal t) xs
encodeVal (RecT fs) (RecV vs) = encodeRec fs' vs'
  where
    fs' = sortOn fst $ map (first hashFieldName) fs
    vs' = map (first hashFieldName) vs
encodeVal (VariantT fs) (VariantV f x) =
    case lookup (hashFieldName f) fs' of
        Just (i, t) -> buildLEB128Int i <> encodeVal t x
        Nothing -> error $ "encodeVal: Variant field " ++ show (pretty f) ++ " not found"
  where
    fs' = sortOn fst [ (hashFieldName n, (i,t)) | ((n,t), i) <- zip fs [0..] ]
encodeVal PrincipalT (PrincipalV (Principal s)) = B.int8 1 <> encodeBytes s
encodeVal BlobT (BlobV b) = encodeBytes b
encodeVal (VecT Nat8T) (BlobV b) = encodeBytes b
encodeVal (RefT x) _ = absurd x
encodeVal t v = error $ "Unexpected value at type " ++ show (pretty t) ++ ": " ++ show (pretty v)

encodeBytes :: BS.ByteString -> B.Builder
encodeBytes bytes = buildLEB128Int (BS.length bytes) <> B.byteString bytes

-- Encodes the fields in order specified by the type
encodeRec :: [(Word32, Type Void)] -> [(Word32, Value)] -> B.Builder
encodeRec [] _ = mempty -- NB: Subtyping
encodeRec ((n,t):fs) vs
    | Just v <- lookup n vs = encodeVal t v <> encodeRec fs vs
    | otherwise = error $ "Missing record field " ++ show (pretty n)

type TypTableBuilder k = RWS () B.Builder (M.Map (Type k) Integer, Natural)

typTable :: TypeDesc -> B.Builder
typTable (TypeDesc m (ts :: [Type k])) = mconcat
    [ buildLEB128 typ_tbl_len
    , typ_tbl
    , leb128Len ts
    , foldMap buildSLEB128 typ_idxs
    ]
  where
    (typ_idxs, (_, typ_tbl_len), typ_tbl) = runRWS (mapM go ts) () (M.empty, 0)

    addCon :: Type k -> TypTableBuilder k B.Builder -> TypTableBuilder k Integer
    addCon t body = gets (M.lookup t . fst) >>= \case
        Just i -> return i
        Nothing -> mdo
            i <- gets snd
            modify' (first (M.insert t (fromIntegral i)))
            modify' (second succ)
            tell b
            b <- body
            return $ fromIntegral i

    go :: Type k -> TypTableBuilder k Integer
    go t = case t of
      NullT     -> return $ -1
      BoolT     -> return $ -2
      NatT      -> return $ -3
      IntT      -> return $ -4
      Nat8T     -> return $ -5
      Nat16T    -> return $ -6
      Nat32T    -> return $ -7
      Nat64T    -> return $ -8
      Int8T     -> return $ -9
      Int16T    -> return $ -10
      Int32T    -> return $ -11
      Int64T    -> return $ -12
      Float32T  -> return $ -13
      Float64T  -> return $ -14
      TextT     -> return $ -15
      ReservedT -> return $ -16
      EmptyT    -> return $ -17

      -- Constructors
      OptT t' -> addCon t $ do
        ti <- go t'
        return $ buildSLEB128 @Integer (-18) <> buildSLEB128 ti
      VecT t' -> addCon t $ do
        ti <- go t'
        return $ buildSLEB128 @Integer (-19) <> buildSLEB128 ti
      RecT fs -> addCon t $ recordLike (-20) fs
      VariantT fs -> addCon t $ recordLike (-21) fs

      -- References
      PrincipalT -> return $ -24

      -- Short-hands
      BlobT -> addCon t $
        -- blob = vec nat8
        return $ buildSLEB128 @Integer (-19) <> buildSLEB128 @Integer (-5)

      RefT t -> go (m M.! t)

    goFields :: Fields k -> TypTableBuilder k [(Word32, Integer)]
    goFields [] = return []
    goFields ((fn, t):fs) = do
        ti <- go t
        tis <- goFields fs
        return $ (hashFieldName fn, ti) : tis

    recordLike :: Integer -> Fields k -> TypTableBuilder k B.Builder
    recordLike n fs = do
        tis <- goFields fs
        return $ mconcat
            [ buildSLEB128 n
            , leb128Len tis
            , foldMap (\(n,ti) -> buildLEB128 n <> buildSLEB128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]

-- | Decode to value representation
decodeVals :: BS.ByteString -> Either String [Value]
decodeVals = G.runGet $ do
    decodeMagic
    arg_tys <- decodeTypTable
    mapM decodeVal (tieKnot arg_tys)

data TypeDesc where
    TypeDesc :: forall k. (Pretty k, Ord k) => M.Map k (Type k) -> [Type k] -> TypeDesc

instance Pretty TypeDesc where
    pretty (TypeDesc m ts) = pretty (M.toList m, ts)

data Ref k f  = Ref k (f (Ref k f))

buildTypeDesc :: forall k. (Pretty k, Ord k) => [Type (Ref k Type)] -> TypeDesc
buildTypeDesc ts = TypeDesc m ts'
  where
    (ts', m) = runState (mapM (mapM go) ts) mempty

    go :: Ref k Type -> State (M.Map k (Type k)) k
    go (Ref k t) = do
        seen <- gets (M.member k)
        unless seen $ mdo
            modify (M.insert k t')
            t' <- mapM go t
            return ()
        return k


tieKnot :: TypeDesc -> [Type Void]
tieKnot (TypeDesc m (ts :: [Type k])) = ts'
  where
    f :: k -> Type Void
    f k = m' M.! k
    m' :: M.Map k (Type Void)
    m' = (>>= f) <$> m
    ts' :: [Type Void]
    ts' = (>>= f) <$> ts


-- | Decode to Haskell type
decode :: forall a. CandidArg a => BS.ByteString -> Either String a
decode = decodeVals >=> fromVals >=> return . fromTuple

decodeVal :: Type Void -> G.Get Value
decodeVal BoolT = G.getWord8 >>= \case
    0 -> return $ BoolV False
    1 -> return $ BoolV True
    _ -> fail "Invalid boolean value"
decodeVal NatT = NatV <$> getLEB128
decodeVal Nat8T = Nat8V <$> G.getWord8
decodeVal Nat16T = Nat16V <$> G.getWord16le
decodeVal Nat32T = Nat32V <$> G.getWord32le
decodeVal Nat64T = Nat64V <$> G.getWord64le
decodeVal IntT = IntV <$> getSLEB128
decodeVal Int8T = Int8V <$> G.getInt8
decodeVal Int16T = Int16V <$> G.getInt16le
decodeVal Int32T = Int32V <$> G.getInt32le
decodeVal Int64T = Int64V <$> G.getInt64le
decodeVal Float32T = Float32V <$> G.getFloat32le
decodeVal Float64T = Float64V <$> G.getFloat64le
decodeVal TextT = TextV <$> do
    bs <- decodeBytes
    case T.decodeUtf8' bs of
        Left err -> fail (show err)
        Right t -> return t
decodeVal NullT = return NullV
decodeVal ReservedT = return ReservedV
decodeVal (OptT t) = G.getWord8 >>= \case
    0 -> return $ OptV Nothing
    1 -> OptV . Just <$> decodeVal t
    _ -> fail "Invalid optional value"
decodeVal (VecT t) = do
    n <- getLEB128Int
    VecV . V.fromList <$> replicateM n (decodeVal t)
decodeVal (RecT fs) = RecV <$> mapM (\(_,(fn, t)) -> (fn,) <$> decodeVal t) fs'
  where
    fs' = sortOn fst [ (hashFieldName n, (n,t)) | (n,t) <- fs ]
decodeVal (VariantT fs) = do
        i <- getLEB128Int
        unless (i <= length fs) $ fail "variant index out of bound"
        let (fn, t) = fs !! i
        VariantV fn <$> decodeVal t
decodeVal PrincipalT = G.getWord8 >>= \case
    0 -> fail "reference encountered"
    1 -> PrincipalV . Principal <$> decodeBytes
    _ -> fail "Invalid principal value"
decodeVal BlobT = error "shorthand encountered while decoding"
decodeVal EmptyT = fail "Empty value"
decodeVal (RefT v) = absurd v

decodeBytes :: G.Get BS.ByteString
decodeBytes = getLEB128Int >>= G.getByteString

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

decodeTypTable :: G.Get TypeDesc
decodeTypTable = do
    len <- getLEB128
    table <- replicateM (fromIntegral len) (decodeTypTableEntry len)
    ts <- decodeSeq (decodeTypRef len)
    return $ TypeDesc (M.fromList (zip [0..] table)) ts

decodeTypTableEntry :: Natural -> G.Get (Type Int)
decodeTypTableEntry max = getSLEB128 @Integer >>= \case
    -18 -> OptT <$> decodeTypRef max
    -19 -> VecT <$> decodeTypRef max
    -20 -> RecT <$> decodeTypFields max
    -21 -> VariantT <$> decodeTypFields max
    _ -> fail "Unknown structural type"

decodeTypRef :: Natural -> G.Get (Type Int)
decodeTypRef max = do
    i <- getSLEB128
    when (i > fromIntegral max) $ fail "Type reference out of range"
    if i < 0
    then case primTyp i of
        Just t -> return t
        Nothing -> fail  $ "Unknown prim typ " ++ show i
    else return $ RefT (fromIntegral i)

decodeTypFields :: Natural -> G.Get (Fields Int)
decodeTypFields max = decodeSeq (decodeTypField max)

decodeTypField :: Natural -> G.Get (FieldName, Type Int)
decodeTypField max = do
    h <- getLEB128
    t <- decodeTypRef max
    return (escapeFieldHash h, t)

primTyp :: Integer -> Maybe (Type a)
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


buildLEB128Int :: Int -> B.Builder
buildLEB128Int = buildLEB128 @Natural . fromIntegral

leb128Len :: [a] -> B.Builder
leb128Len = buildLEB128Int . length

-- Using normal Haskell values

-- | The class of types that can be used as Candid argument sequences.
-- Essentially all types that are in 'Candid', but tuples need to be treated specially.
type CandidArg a = (CandidSeq (AsTuple a), Tuplable a)


class CandidSeq a where
    asTypes :: [Type (Ref TypeRep Type)]
    seqVal :: a -> [Value]
    fromVals :: [Value] -> Either String a

typeDesc :: forall a. CandidArg a => TypeDesc
typeDesc = buildTypeDesc (asTypes @(AsTuple a))

instance Pretty TypeRep where
    pretty = pretty . show

instance CandidSeq () where
    asTypes = []
    seqVal () = []
    fromVals _ = return () -- Subtyping

instance Candid a => CandidSeq (Unary a) where
    asTypes = [asType' @a]
    seqVal (Unary x) = [ toCandid x ]
    fromVals (x:_) = Unary <$> fromCandid x -- Subtyping
    fromVals _ = error "Not enough arguments"

instance (Candid a, Candid b) => CandidSeq (a, b) where
    asTypes = [asType' @a, asType' @b]
    seqVal (x, y) = [ toCandid x, toCandid y ]
    fromVals (x:y:_) = (,) <$> fromCandid x <*> fromCandid y
    fromVals _ = error "Not enough arguments"

-- | The class of Haskell types that can be converted to Candid.
--
-- You can create intances of this class for your own types; see the overview above for examples. The default instance is mostly for internal use.
class Typeable a => Candid a where
    asType :: Type (Ref TypeRep Type)
    toCandid :: a -> Value
    fromCandid :: Value -> Either String a

asType' :: forall a.  Candid a => Type (Ref TypeRep Type)
asType' = RefT (Ref (typeRep (Proxy @a)) (asType @a))

instance Candid Bool where
    asType = BoolT
    toCandid = BoolV
    fromCandid (BoolV b) = Right b
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Natural where
    asType = NatT
    toCandid = NatV
    fromCandid (NatV n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word8 where
    asType = Nat8T
    toCandid = Nat8V
    fromCandid (Nat8V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word16 where
    asType = Nat16T
    toCandid = Nat16V
    fromCandid (Nat16V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word32 where
    asType = Nat32T
    toCandid = Nat32V
    fromCandid (Nat32V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word64 where
    asType = Nat64T
    toCandid = Nat64V
    fromCandid (Nat64V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Integer where
    asType = IntT
    toCandid = IntV
    fromCandid (NatV n) = Right (fromIntegral n)
    fromCandid (IntV n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int8 where
    asType = Int8T
    toCandid = Int8V
    fromCandid (Int8V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int16 where
    asType = Int16T
    toCandid = Int16V
    fromCandid (Int16V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int32 where
    asType = Int32T
    toCandid = Int32V
    fromCandid (Int32V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int64 where
    asType = Int64T
    toCandid = Int64V
    fromCandid (Int64V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Float where
    asType = Float32T
    toCandid = Float32V
    fromCandid (Float32V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Double where
    asType = Float64T
    toCandid = Float64V
    fromCandid (Float64V n) = Right n
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Void where
    asType = EmptyT
    toCandid = absurd
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid T.Text where
    asType = TextT
    toCandid = TextV
    fromCandid (TextV t) = return t
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid BS.ByteString where
    asType = BlobT
    toCandid = BlobV
    -- should avoid going through vector here somehow
    fromCandid (VecV v) =  BS.pack . V.toList <$> mapM (fromCandid @Word8) v
    fromCandid (BlobV t) = return t
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid BSL.ByteString where
    asType = asType @BS.ByteString
    toCandid = toCandid . BSL.toStrict
    fromCandid x = BSL.fromStrict <$> fromCandid x


instance Candid Principal where
    asType = PrincipalT
    toCandid = PrincipalV
    fromCandid (PrincipalV t) = return t
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Reserved where
    asType = ReservedT
    toCandid Reserved = ReservedV
    fromCandid _ = return Reserved

instance Candid a => Candid (Maybe a) where
    asType = OptT (asType' @a)
    toCandid = OptV . fmap toCandid
    fromCandid (OptV x) = traverse fromCandid x
    fromCandid NullV = return Nothing
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)


instance Candid a => Candid (V.Vector a) where
    asType = VecT (asType' @a)
    toCandid = VecV . fmap toCandid
    fromCandid (VecV x) = traverse fromCandid x
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance Candid a => Candid [a] where
    asType = asType @(V.Vector a)
    toCandid = toCandid . V.fromList
    fromCandid x = V.toList <$> fromCandid x

-- | Maybe a bit opinionated, but 'null' seems to be the unit of Candid
instance Candid () where
    asType = NullT
    toCandid () = NullV
    fromCandid NullV = Right ()
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance (Candid a, Candid b) => Candid (a, b) where
    asType = tupT [asType' @a, asType' @b]
    toCandid (x,y) = tupV [toCandid x, toCandid y]
    fromCandid (RecV m) = do
        x <- fromTupField 0 m >>= fromCandid
        y <- fromTupField 1 m >>= fromCandid
        return (x, y)
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

instance (Candid a, Candid b, Candid c) => Candid (a, b, c) where
    asType = tupT [asType' @a, asType' @b, asType' @c]
    toCandid (x,y,z) = tupV [toCandid x, toCandid y, toCandid z]
    fromCandid (RecV m) = do
        x <- fromTupField 0 m >>= fromCandid
        y <- fromTupField 1 m >>= fromCandid
        z <- fromTupField 2 m >>= fromCandid
        return (x, y, z)
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

fromField :: FieldName -> [(FieldName, a)] -> Either String a
fromField f m = case lookupField f m of
    Just v -> return v
    Nothing -> Left $ "Could not find field " ++ show (pretty f)

fromTupField :: Word32 -> [(FieldName, a)] -> Either String a
fromTupField = fromField . escapeFieldHash

{-
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

-}
instance (Candid a, Candid b) => Candid (Either a b) where
    asType = VariantT [(N "Left", asType' @a), (N "Right", asType' @b) ]
    toCandid (Left x) = VariantV (N "Left") (toCandid x)
    toCandid (Right x) = VariantV (N "Right") (toCandid x)
    fromCandid (VariantV f x)
        | hashFieldName f == hashFieldName (N "Left") = Left <$> fromCandid x
        | hashFieldName f == hashFieldName (N "Right") = Right <$> fromCandid x
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

-- row-types integration

class FromRowRec r where
    asTypesRec :: Fields (Ref TypeRep Type)
    fromRowRec :: R.Rec r -> [(FieldName, Value)]
    toRowRec :: [(FieldName, Value)] -> Either String (R.Rec r)


instance FromRowRec ('R.R '[]) where
    asTypesRec = []
    fromRowRec _ = mempty
    toRowRec _ = return R.empty

instance (Candid t, R.KnownSymbol f, FromRowRec ('R.R xs)) => FromRowRec ('R.R (f 'R.:-> t ': xs)) where
    asTypesRec = (N (R.toKey l), asType' @t) : asTypesRec @('R.R xs)
      where l = R.Label @f
    fromRowRec r = (N (R.toKey l), toCandid (r R..! l)) : fromRowRec (r R..- l)
      where l = R.Label @f
    toRowRec m = do
        v <- fromField (N (R.toKey l)) m
        x <- fromCandid @t v
        r' <- toRowRec @('R.R xs) m
        return $ R.unsafeInjectFront l x r'
      where l = R.Label @f

instance (FromRowRec r, Typeable r) => Candid (R.Rec r) where
    asType = RecT (asTypesRec @r)
    toCandid = RecV . fromRowRec
    fromCandid (RecV m) = toRowRec m
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

class FromRowVar r where
    asTypesVar :: Fields (Ref TypeRep Type)
    fromRowVar :: V.Var r -> (FieldName, Value)
    toRowVar :: FieldName -> Value -> Either String (V.Var r)

instance FromRowVar ('R.R '[]) where
    asTypesVar = []
    fromRowVar = V.impossible
    toRowVar f v = Left $ "Unexpected " ++ show (pretty (VariantV f v))

instance (Candid t, R.KnownSymbol f, FromRowVar ('R.R xs)) => FromRowVar ('R.R (f 'R.:-> t ': xs)) where
    asTypesVar = (N (R.toKey l), asType' @t) : asTypesVar @('R.R xs)
      where l = R.Label @f
    fromRowVar r = case V.trial r l of
        Left x -> (N (R.toKey l), toCandid x)
        Right r' -> fromRowVar @('R.R xs) r'
      where l = R.Label @f
    toRowVar f v
        | hashFieldName f == hashFieldName (N (R.toKey l))
        = V.unsafeMakeVar l <$> fromCandid v
        | otherwise
        = V.unsafeInjectFront <$> toRowVar @('R.R xs) f v
      where l = R.Label @f

instance (FromRowVar r, Typeable r) => Candid (V.Var r) where
    asType = VariantT (asTypesVar @r)
    toCandid = uncurry VariantV . fromRowVar
    fromCandid (VariantV f v) = toRowVar f v
    fromCandid v = Left $ "Unexpected " ++ show (pretty v)

