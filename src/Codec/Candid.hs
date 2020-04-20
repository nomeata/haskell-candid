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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Codec.Candid
    ( Type(..)
    , Fields
    , FieldName(..)
    , KnownFields
    , Val(..)
    , Rec(..)
    , Variant(..)
    , Seq(..)
    , DecodeVal(..)
    , Candid(..)
    , Unary(..)
    , CandidArgs(..)
    , encode
    , encodeBuilder
    , decode
    , DecodeTypes
    ) where

import Numeric.Natural
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import Data.Proxy
import Data.Bifunctor
import Data.Bits
import Data.Word
import Data.Int
import Data.List
import Control.Monad.RWS.Lazy
import GHC.TypeLits
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.IEEE754 as G

-- |
-- The type of candid values
-- (coinductively, without named types, i.e. after name resolution)
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

type Fields = [(FieldName, Type)]

data FieldName
   = Named Symbol -- ^ Use this in types
   | Named' T.Text -- ^ Use this in terms (usually not needed)
   | Hashed Nat -- ^ Use this in types
   | Hashed' Word32 -- ^ Use this in terms (mostly internal)

prettyFieldName :: FieldName -> String
prettyFieldName (Named _) = error "Named in term"
prettyFieldName (Named' t) = T.unpack t
prettyFieldName (Hashed _) = error "Nat in term"
prettyFieldName (Hashed' n) = "id " <> show n


-- The values

data Val (t :: Type) where
    NatV :: Natural -> Val 'NatT
    Nat8V :: Word8 -> Val 'Nat8T
    Nat16V :: Word16 -> Val 'Nat16T
    Nat32V :: Word32 -> Val 'Nat32T
    Nat64V :: Word64 -> Val 'Nat64T
    IntV :: Integer -> Val 'IntT
    Int8V :: Int8 -> Val 'Int8T
    Int16V :: Int16 -> Val 'Int16T
    Int32V :: Int32 -> Val 'Int32T
    Int64V :: Int64 -> Val 'Int64T
    Float32V :: Float -> Val 'Float32T
    Float64V :: Double -> Val 'Float64T
    BoolV :: Bool -> Val 'BoolT
    TextV :: T.Text -> Val 'TextT
    NullV :: Val 'NullT
    ReservedV :: Val 'ReservedT
    -- NB: No EmptyV
    OptV :: KnownType t => Maybe (Val t) -> Val ('OptT t)
    VecV :: KnownType t => V.Vector (Val t) -> Val ('VecT t)
    RecV :: KnownFields fs => Rec fs -> Val ('RecT fs)
    VariantV :: KnownFields fs => Variant fs -> Val ('VariantT fs)

deriving instance Show (Val a)
deriving instance Eq (Val a)

data Seq (ts :: [Type]) where
  EmptySeq :: Seq '[]
  (::>) :: (KnownType t, KnownTypes ts) => Val t -> Seq ts ->  Seq (t ': ts)
infixr 5 ::>
deriving instance Show (Seq fs)
deriving instance Eq (Seq fs)

data Rec (fs :: Fields) where
  EmptyRec :: Rec '[]
  (:>) :: (KnownFieldName f, KnownType t, KnownFields fs) =>
          Val t -> Rec fs -> Rec ('(f,t) ': fs)
infixr 5 :>

deriving instance Show (Rec fs)
deriving instance Eq (Rec fs)

data Variant (fs :: [(FieldName, Type)]) where
  This :: KnownType t => Val t -> Variant ('(f,t) ': fs)
  Other :: KnownFields fs => Variant fs -> Variant ('(f,t) ': fs)

deriving instance Show (Variant fs)
deriving instance Eq (Variant fs)

encode :: CandidArgs a => a -> BS.ByteString
encode = BSL.toStrict . B.toLazyByteString . encodeBuilder

encodeBuilder :: forall a. CandidArgs a => a -> B.Builder
encodeBuilder x = mconcat
    [ B.stringUtf8 "DIDL"
    , typTable (types @(ArgRep a))
    , encodeSeq (toSeq x)
    ]

-- Argument sequences, although represented as records
-- are always encoded in order
encodeSeq :: KnownTypes ts => Seq ts -> B.Builder
encodeSeq EmptySeq = mempty
encodeSeq (x ::> xs) = encodeVal x <> encodeSeq xs

encodeVal :: forall t. KnownType t => Val t -> B.Builder
encodeVal (BoolV False) = B.word8 0
encodeVal (BoolV True) = B.word8 1
encodeVal (NatV n) = leb128 n
encodeVal (Nat8V n) = B.word8 n
encodeVal (Nat16V n) = B.word16LE n
encodeVal (Nat32V n) = B.word32LE n
encodeVal (Nat64V n) = B.word64LE n
encodeVal (IntV n) = sleb128 n
encodeVal (Int8V n) = B.int8 n
encodeVal (Int16V n) = B.int16LE n
encodeVal (Int32V n) = B.int32LE n
encodeVal (Int64V n) = B.int64LE n
encodeVal (Float32V n) = B.floatLE n
encodeVal (Float64V n) = B.doubleLE n
encodeVal (TextV t) = leb128 (fromIntegral (BS.length bytes)) <> B.byteString bytes
  where bytes = T.encodeUtf8 t
encodeVal NullV = mempty
encodeVal ReservedV = mempty
encodeVal (OptV Nothing) = B.word8 0
encodeVal (OptV (Just x)) = B.word8 1 <> encodeVal x
encodeVal (VecV xs) = leb128 (fromIntegral (V.length xs)) <> foldMap encodeVal xs
encodeVal (RecV xs) = foldMap snd $ sortOn fst $ encodeRec xs
encodeVal (VariantV (x :: Variant fs)) = leb128 (fromIntegral i) <> b
  where
    (pos, b) = encodeVar x
    m = map fst $ sortOn snd $ zip [0..] (map (hashFieldName . fst) (fields @fs))
    Just i = elemIndex pos m


-- Encodes the fields, sorting happens later
encodeRec :: forall fs. KnownFields fs => Rec fs -> [(Word32, B.Builder)]
encodeRec EmptyRec = []
encodeRec (x :> xs) =
    (hashFieldName (fieldName @(FieldNameHead fs)), encodeVal x) : encodeRec xs

-- Encodes the value, returns fields and index
encodeVar :: forall fs. KnownFields fs => Variant fs -> (Natural, B.Builder)
encodeVar (This x) = (0, encodeVal x)
encodeVar (Other v) = first succ (encodeVar v)

type family FieldNameHead (fs :: Fields) :: FieldName where
    FieldNameHead ('(f,_)':_) = f


type TypTableBuilder = RWS () B.Builder Natural
typTable :: [Type] -> B.Builder
typTable ts = mconcat
    [ leb128 typ_tbl_len
    , typ_tbl
    , leb128Len ts
    , foldMap sleb128 typ_idxs
    ]
  where
    (typ_idxs, typ_tbl_len, typ_tbl) = runRWS (mapM go ts) () 0

    addTyp :: B.Builder -> TypTableBuilder Integer
    addTyp b = fmap fromIntegral $ tell b *> get <* modify' succ

    go :: Type -> TypTableBuilder Integer
    go NullT     = return $ -1
    go BoolT     = return $ -2
    go NatT      = return $ -3
    go IntT      = return $ -4
    go Nat8T     = return $ -5
    go Nat16T    = return $ -6
    go Nat32T    = return $ -7
    go Nat64T    = return $ -8
    go Int8T     = return $ -9
    go Int16T    = return $ -10
    go Int32T    = return $ -11
    go Int64T    = return $ -12
    go Float32T  = return $ -13
    go Float64T  = return $ -14
    go TextT     = return $ -15
    go ReservedT = return $ -16
    go EmptyT    = return $ -17

    go (OptT t) = do
        ti <- go t
        addTyp $ sleb128 (-18) <> sleb128 ti
    go (VecT t) = do
        ti <- go t
        addTyp $ sleb128 (-19) <> sleb128 ti
    go (RecT fs) = do
        tis <- forM fs $ \(fn, t) -> do
            ti <- go t
            return (hashFieldName fn, ti)
        addTyp $ mconcat
            [ sleb128 (-20)
            , leb128Len fs
            , foldMap (\(n,ti) -> leb128 (fromIntegral n) <> sleb128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]
    go (VariantT fs) = do
        tis <- forM fs $ \(fn, t) -> do
            ti <- go t
            return (hashFieldName fn, ti)
        addTyp $ mconcat
            [ sleb128 (-21)
            , leb128Len fs
            , foldMap (\(n,ti) -> leb128 (fromIntegral n) <> sleb128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]


decode :: CandidArgs a => BS.ByteString -> Either String a
decode = G.runGet $ do
    decodeMagic
    arg_tys <- decodeTypTable
    -- get the argument sequence
    fromSeq <$> decodeParams arg_tys

class KnownTypes ts => DecodeTypes ts where
    decodeParams :: [Type] -> G.Get (Seq ts)

instance DecodeTypes '[] where
    decodeParams _ = return EmptySeq -- NB: This is where we ignore extra arguments

instance (DecodeTypes ts, DecodeVal pt) => DecodeTypes (pt ': ts) where
    decodeParams [] = fail "Missing argument"
    decodeParams (t' : ts) = do
        v <- decodeVal t'
        vs <- decodeParams ts
        return $ v ::> vs

class KnownType t => DecodeVal t where
    decodeVal :: Type -> G.Get (Val t)

instance DecodeVal 'BoolT where
    decodeVal BoolT = G.getWord8 >>= \case
        0 -> return $ BoolV False
        1 -> return $ BoolV True
        _ -> fail "Invalid boolean value"
    decodeVal _ = fail "unexpected type, expected BoolT"

instance DecodeVal 'NatT where
    decodeVal NatT = NatV <$> getLeb128
    decodeVal _ = fail "unexpected type, expected NatT"
instance DecodeVal 'Nat8T where
    decodeVal Nat8T = Nat8V <$> G.getWord8
    decodeVal _ = fail "unexpected type, expected Nat8T"
instance DecodeVal 'Nat16T where
    decodeVal Nat16T = Nat16V <$> G.getWord16le
    decodeVal _ = fail "unexpected type, expected Nat16T"
instance DecodeVal 'Nat32T where
    decodeVal Nat32T = Nat32V <$> G.getWord32le
    decodeVal _ = fail "unexpected type, expected Nat32T"
instance DecodeVal 'Nat64T where
    decodeVal Nat64T = Nat64V <$> G.getWord64le
    decodeVal _ = fail "unexpected type, expected Nat64T"
instance DecodeVal 'IntT where
    decodeVal NatT = IntV . fromIntegral <$> getLeb128
    decodeVal IntT = IntV <$> getSleb128
    decodeVal _ = fail "unexpected type, expected IntT (or NatT)"
instance DecodeVal 'Int8T where
    decodeVal Int8T = Int8V <$> G.getInt8
    decodeVal _ = fail "unexpected type, expected Int8T"
instance DecodeVal 'Int16T where
    decodeVal Int16T = Int16V <$> G.getInt16le
    decodeVal _ = fail "unexpected type, expected Int16T"
instance DecodeVal 'Int32T where
    decodeVal Int32T = Int32V <$> G.getInt32le
    decodeVal _ = fail "unexpected type, expected Int32T"
instance DecodeVal 'Int64T where
    decodeVal Int64T = Int64V <$> G.getInt64le
    decodeVal _ = fail "unexpected type, expected Int64T"
instance DecodeVal 'Float32T where
    decodeVal Float32T = Float32V <$> G.getFloat32le
    decodeVal _ = fail "unexpected type, expected Float32T"
instance DecodeVal 'Float64T where
    decodeVal Float64T = Float64V <$> G.getFloat64le
    decodeVal _ = fail "unexpected type, expected Float64T"
instance DecodeVal 'TextT where
    decodeVal TextT = do
        n <- getLeb128
        bs <- G.getByteString (fromIntegral n)
        case T.decodeUtf8' bs of
            Left err -> fail (show err)
            Right t -> return $ TextV t
    decodeVal _ = fail "unexpected type, expected TextT"
instance DecodeVal 'NullT where
    decodeVal NullT = return NullV
    decodeVal _ = fail "unexpected type, expected NullT"
instance DecodeVal 'ReservedT where
    decodeVal t = skipVal t >> return ReservedV

instance DecodeVal t => DecodeVal ('OptT t) where
    decodeVal NullT = return (OptV Nothing)
    decodeVal (OptT t) = G.getWord8 >>= \case
        0 -> return $ OptV Nothing
        1 -> OptV . Just <$> decodeVal t
        _ -> fail "Invalid optional value"
    decodeVal _ = fail "unexpected type, expected OptT"
instance DecodeVal t => DecodeVal ('VecT t) where
    decodeVal (VecT t) = do
        n <- getLeb128
        VecV . V.fromList <$> replicateM (fromIntegral n) (decodeVal t)
    decodeVal _ = fail "unexpected type, expected VecT"
instance DecodeFields fs => DecodeVal ('RecT fs) where
    decodeVal (RecT fs) = RecV <$> decodeRec fs
    decodeVal _ = fail "unexpected type, expected RecT"

decodeRec :: forall fs. DecodeFields fs => Fields -> G.Get (Rec fs)
decodeRec [] = noFields
decodeRec ((h,t):dfs) = case findField @fs (hashFieldName h) of
    Found (>:) -> do
        x <- decodeVal t
        xs <- decodeRec dfs
        return (x >: xs)
    NotFound -> do
        skipVal t
        decodeRec dfs

class KnownFields fs => DecodeFields fs where
    noFields ::G.Get (Rec fs)
    findField :: Word32 -> FindFieldResult fs

instance DecodeFields '[] where
    noFields = return EmptyRec
    findField _ = NotFound

instance (KnownFieldName f, DecodeVal t, DecodeFields fs) => DecodeFields ('(f,t) ': fs) where
    noFields = fail $ "missing field " <> prettyFieldName (fieldName @f)
    findField h
        | h == hashFieldName (fieldName @f) = Found (:>)
        | otherwise = case findField @fs h of
            NotFound -> NotFound
            Found ((>:) :: Val t' -> Rec fs' -> Rec fs) ->
                Found @t' @('(f,t) ': fs') (\x (y :> ys) -> y :> (x >: ys))

data FindFieldResult fs where
    Found :: forall t' fs' fs. (DecodeVal t', DecodeFields fs') =>
        (Val t' -> Rec fs' -> Rec fs) -> FindFieldResult fs
    NotFound :: FindFieldResult fs

instance DecodeVariant fs => DecodeVal ('VariantT fs) where
    decodeVal (VariantT fs) = do
        i <- getLeb128
        unless (i <= fromIntegral (length fs)) $ fail "variant index out of bound"
        let (fn, t) = fs !! fromIntegral i
        VariantV <$> decodeVariant (hashFieldName fn) t
    decodeVal _ = fail "unexpected type"

class KnownFields fs => DecodeVariant fs where
    decodeVariant :: Word32 -> Type -> G.Get (Variant fs)

instance DecodeVariant '[] where
    decodeVariant _ _ = fail "unexpected variant tag"

instance (KnownFieldName f, DecodeVal t, DecodeVariant fs) => DecodeVariant ('(f,t) ': fs) where
    decodeVariant h t
        | h == hashFieldName (fieldName @f) = This <$> decodeVal t
        | otherwise = Other <$> decodeVariant h t

skipVal :: Type -> G.Get ()
skipVal BoolT = G.skip 1
skipVal NatT = void getLeb128
skipVal Nat8T = G.skip 1
skipVal Nat16T = G.skip 2
skipVal Nat32T = G.skip 4
skipVal Nat64T = G.skip 8
skipVal IntT = void getSleb128
skipVal Int8T = G.skip 1
skipVal Int16T = G.skip 2
skipVal Int32T = G.skip 4
skipVal Int64T = G.skip 8
skipVal Float32T = G.skip 4
skipVal Float64T = G.skip 8
skipVal TextT = getLeb128 >>= G.skip . fromIntegral
skipVal NullT = return ()
skipVal ReservedT = return ()
skipVal EmptyT = fail "skipping empty value"
skipVal (OptT t) = G.getWord8 >>= \case
    0 -> return ()
    1 -> skipVal t
    _ -> fail "Invalid optional value"
skipVal (VecT t) = do
    n <- getLeb128
    replicateM_ (fromIntegral n) (skipVal t)
skipVal (RecT fs) = mapM_ (skipVal . snd) fs
skipVal (VariantT fs) = do
    i <- getLeb128
    unless (i <= fromIntegral (length fs)) $ fail "variant index out of bound"
    let (_fn, t) = fs !! fromIntegral i
    skipVal t



decodeMagic :: G.Get ()
decodeMagic = do
    magic <- G.getBytes 4
    unless (magic == T.encodeUtf8 (T.pack "DIDL")) $ fail "Expected magic bytes \"DIDL\""

decodeSeq :: G.Get a -> G.Get [a]
decodeSeq act = do
    len <- getLeb128
    replicateM (fromIntegral len) act

decodeTypTable :: G.Get [Type]
decodeTypTable = do
    -- typ table
    len <- getLeb128
    pre_table <- V.fromList <$> replicateM (fromIntegral len) (decodeTypTableEntry len)
    -- tie the know
    let table = fmap ($ table) pre_table
    -- argument list
    map ($ table) <$> decodeSeq (decodeTypRef len)

decodeTypTableEntry :: Natural -> G.Get (V.Vector Type -> Type)
decodeTypTableEntry max = getSleb128 >>= \case
    -18 -> (OptT <$>) <$> decodeTypRef max
    -19 -> (VecT <$>) <$> decodeTypRef max
    -20 -> (RecT <$>) <$> decodeTypFields max
    -21 -> (VariantT <$>) <$> decodeTypFields max
    _ -> fail "Unknown structural type"

decodeTypRef :: Natural -> G.Get (V.Vector Type -> Type)
decodeTypRef max = do
    i <- getSleb128
    when (i > fromIntegral max) $ fail "Type reference out of range"
    if i < 0
    then case primTyp i of
        Just t -> return $ const t
        Nothing -> fail "Unknown prim typ"
    else return $ \v ->v V.! fromIntegral i

decodeTypFields :: Natural -> G.Get (V.Vector Type -> Fields)
decodeTypFields max = sequence <$> decodeSeq (decodeTypField max)

decodeTypField :: Natural -> G.Get (V.Vector Type -> (FieldName, Type))
decodeTypField max = do
    h <- getLeb128
    when (h > fromIntegral (maxBound :: Word32)) $ fail "Field hash too large"
    t <- decodeTypRef max
    return $ (Hashed' (fromIntegral h),) <$> t

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
primTyp _     = Nothing


hashFieldName :: FieldName -> Word32
hashFieldName (Hashed _) = error "Nat on value level"
hashFieldName (Hashed' n) = n
hashFieldName (Named _) = error "Symbol in value level computation"
hashFieldName (Named' s) =
    BS.foldl (\h c -> (h * 223 + fromIntegral c)) 0 $ T.encodeUtf8 s

getLeb128 :: G.Get Natural
getLeb128 = go 0 0
  where
    go :: Int -> Natural -> G.Get Natural
    go shift w = do
        byte <- G.getWord8
        let !byteVal = fromIntegral (clearBit byte 7)
        let !hasMore = testBit byte 7
        let !val = w .|. (byteVal `unsafeShiftL` shift)
        if hasMore
            then go (shift+7) val
            else return $! val

getSleb128 :: G.Get Integer
getSleb128 = do
    (val,shift,signed) <- go 0 0
    return $ if signed
        then val - 2^shift
        else val
    where
        go :: Int -> Integer -> G.Get (Integer, Int, Bool)
        go shift val = do
            byte <- G.getWord8
            let !byteVal = fromIntegral (clearBit byte 7)
            let !val' = val .|. (byteVal `unsafeShiftL` shift)
            let !more = testBit byte 7
            let !shift' = shift+7
            if more
                then go shift' val'
                else do
                    let !signed = testBit byte 6
                    return (val',shift',signed)

leb128 :: Natural -> B.Builder
leb128 = go
  where
    go i
      | i <= 127
      = B.word8 (fromIntegral i :: Word8)
      | otherwise =
        -- bit 7 (8th bit) indicates more to come.
        B.word8 (setBit (fromIntegral i) 7) <> go (i `unsafeShiftR` 7)

leb128Len :: [a] -> B.Builder
leb128Len = leb128 . fromIntegral . length

sleb128 :: Integer -> B.Builder
sleb128 = go
  where
    go val = do
        let !byte = fromIntegral (clearBit val 7) :: Word8
        let !val' = val `unsafeShiftR` 7
        let !signBit = testBit byte 6
        let !done =
                -- Unsigned value, val' == 0 and and last value can
                -- be discriminated from a negative number.
                (val' == 0 && not signBit) ||
                -- Signed value,
                (val' == -1 && signBit)
        let !byte' = if done then byte else setBit byte 7
        B.word8 byte' <> if done then mempty else go val'

-- Using normal Haskell values

class DecodeTypes (ArgRep a) => CandidArgs a where
    type ArgRep a :: [Type]
    toSeq :: a -> Seq (ArgRep a)
    fromSeq :: Seq (ArgRep a) -> a

newtype Unary a = Unary a deriving (Eq, Show)

instance DecodeTypes ts => CandidArgs (Seq ts) where
    type ArgRep (Seq ts) = ts
    toSeq = id
    fromSeq = id

instance CandidArgs () where
    type ArgRep () = '[]
    toSeq () = EmptySeq
    fromSeq EmptySeq = ()

instance Candid a => CandidArgs (Unary a) where
    type ArgRep (Unary a) = '[Rep a]
    toSeq (Unary x) = toCandid x ::> EmptySeq
    fromSeq (x ::> EmptySeq) = Unary $ fromCandid x

instance (Candid a, Candid b) => CandidArgs (a, b) where
    type ArgRep (a, b) = '[Rep a, Rep b]
    toSeq (x,y) = toCandid x ::> toCandid y ::> EmptySeq
    fromSeq (x ::> y ::> EmptySeq) = (fromCandid x, fromCandid y)

class DecodeVal (Rep a) => Candid a where
    type Rep a :: Type
    toCandid :: a -> Val (Rep a)
    fromCandid :: Val (Rep a) -> a

instance DecodeVal t => Candid (Val t) where
    type Rep (Val t) = t
    toCandid = id
    fromCandid = id

instance Candid Bool where
    type Rep Bool = 'BoolT
    toCandid = BoolV
    fromCandid (BoolV b) = b

instance Candid Natural where
    type Rep Natural = 'NatT
    toCandid = NatV
    fromCandid (NatV b) = b

instance Candid Integer where
    type Rep Integer = 'IntT
    toCandid = IntV
    fromCandid (IntV b) = b

instance Candid a => Candid (Maybe a) where
    type Rep (Maybe a) = 'OptT (Rep a)
    toCandid = OptV . fmap toCandid
    fromCandid (OptV x) = fmap fromCandid x

instance (Candid a, Candid b) => Candid (a, b) where
    type Rep (a, b) = 'RecT '[ TupField 0 a, TupField 1 b]
    toCandid (x,y) = RecV $ toCandid x :> toCandid y :> EmptyRec
    fromCandid (RecV (x :> y :> EmptyRec)) = (fromCandid x, fromCandid y)

instance (Candid a, Candid b, Candid c) => Candid (a, b, c) where
    type Rep (a, b, c) = 'RecT '[ TupField 0 a, TupField 1 b, TupField 2 c]
    toCandid (x,y,z) = RecV $ toCandid x :> toCandid y :> toCandid z :> EmptyRec
    fromCandid (RecV (x :> y :> z :> EmptyRec)) = (fromCandid x, fromCandid y, fromCandid z)

type TupField n a = '( 'Hashed n, Rep a)

instance (Candid a, Candid b) => Candid (Either a b) where
    type Rep (Either a b) = 'VariantT '[ '( 'Named "Left", Rep a), '( 'Named "Right", Rep b) ]
    toCandid (Left x) = VariantV $ This $ toCandid x
    toCandid (Right x) = VariantV $ Other $ This $ toCandid x
    fromCandid (VariantV (This x)) = Left (fromCandid x)
    fromCandid (VariantV (Other (This x))) = Right (fromCandid x)
    fromCandid (VariantV (Other (Other _))) = error "unreachable"


-- Repetitive stuff for dependently typed programming

class KnownType (t :: Type) where typ :: Type

instance KnownType 'NatT where typ = NatT
instance KnownType 'Nat8T where typ = Nat8T
instance KnownType 'Nat16T where typ = Nat16T
instance KnownType 'Nat32T where typ = Nat32T
instance KnownType 'Nat64T where typ = Nat64T
instance KnownType 'IntT where typ = IntT
instance KnownType 'Int8T where typ = Int8T
instance KnownType 'Int16T where typ = Int16T
instance KnownType 'Int32T where typ = Int32T
instance KnownType 'Int64T where typ = Int64T
instance KnownType 'Float32T where typ = Float32T
instance KnownType 'Float64T where typ = Float64T
instance KnownType 'TextT where typ = TextT
instance KnownType 'BoolT where typ = BoolT
instance KnownType 'NullT where typ = NullT
instance KnownType 'ReservedT where typ = ReservedT
instance KnownType 'EmptyT where typ = EmptyT
instance KnownType t => KnownType ('OptT t) where typ = OptT (typ @t)
instance KnownType t => KnownType ('VecT t) where typ = VecT (typ @t)
instance KnownFields fs => KnownType ('RecT fs) where typ = RecT (fields @fs)
instance KnownFields fs => KnownType ('VariantT fs) where typ = VariantT (fields @fs)

class KnownFields (t :: [(FieldName, Type)]) where fields :: [(FieldName, Type)]
instance KnownFields '[] where fields = []
instance (KnownFieldName n, KnownType t, KnownFields fs) => KnownFields ('(n,t) ': fs) where
    fields = (fieldName @n, typ @t) : fields @fs

class KnownTypes (t :: [Type]) where types :: [Type]
instance KnownTypes '[] where types = []
instance (KnownType t, KnownTypes ts) => KnownTypes (t ': ts) where
    types = typ @t : types @ts

class KnownFieldName (fn :: FieldName) where fieldName :: FieldName
instance KnownSymbol s => KnownFieldName ('Named s) where
    fieldName = Named' (T.pack (symbolVal @s @Proxy undefined))
instance KnownNat s => KnownFieldName ('Hashed s) where
    fieldName = Hashed' (fromIntegral (natVal @s @Proxy undefined))

