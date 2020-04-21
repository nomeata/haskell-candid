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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-} -- for Eq CandidVal
module Codec.Candid
    ( Type(..)
    , Fields
    , Field(H,N)
    , Candid(..)
    , CandidArgs(..)
    , Unary(..)
    , encode
    , encodeBuilder
    , decode
    , Val
    , Rec
    , Variant
    , Seq
    , CandidVal(..)
    , CandidSeq(..)
    , KnownType(..)
    , KnownArgs(..)
    , KnownFields(..)
    , fromSArgs
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
import Data.Void
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
  deriving Show

type Fields = [Field]

data Field
    = N Symbol Type
    | N_ T.Text Type -- ^ internal only
    | H Nat Type
    | H_ Word32 Type -- ^ internal only

-- TODO: Do a proper pretty-printer
instance Show Field where
    show (N _ _) = error "show on type"
    show (H _ _) = error "show on type"
    show (N_ s t) = T.unpack s <> " : " <> show t
    show (H_ n t) = show n  <> " : " <> show t

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
    Val ('RecT fs) = Rec fs
    Val ('VariantT fs) = Variant fs

type family Seq (ts :: [Type]) where
    Seq '[] = ()
    Seq (t ': ts) = (Val t, Seq ts)

type family Rec (fs :: Fields) where
    Rec '[] = ()
    Rec (f :fs) = (Val (FieldType f), Rec fs)

type family FieldType (f :: Field) where
    FieldType ('N _ t) = t
    FieldType ('N_ _ t) = t
    FieldType ('H _ t) = t
    FieldType ('H_ _ t) = t

type family Variant (fs :: [Field]) where
    Variant '[] = Void
    Variant (f ': fs) = Either (Val (FieldType f)) (Variant fs)

encode :: CandidArgs a => a -> BS.ByteString
encode = BSL.toStrict . B.toLazyByteString . encodeBuilder

encodeBuilder :: forall a. CandidArgs a => a -> B.Builder
encodeBuilder x = mconcat
    [ B.stringUtf8 "DIDL"
    , typTable (fromSArgs (args @(ArgRep a)))
    , encodeSeq (args @(ArgRep a)) (toSeq x)
    ]

-- Argument sequences, although represented as records
-- are always encoded in order
encodeSeq :: SArgs ts -> Seq ts -> B.Builder
encodeSeq SArgsNil () = mempty
encodeSeq (SArgsCons t ts) (x, xs) = encodeVal t x <> encodeSeq ts xs

encodeVal :: SType t -> Val t -> B.Builder
encodeVal SEmptyT v = case v of {}
encodeVal SBoolT False = B.word8 0
encodeVal SBoolT True = B.word8 1
encodeVal SNatT n = leb128 n
encodeVal SNat8T n = B.word8 n
encodeVal SNat16T n = B.word16LE n
encodeVal SNat32T n = B.word32LE n
encodeVal SNat64T n = B.word64LE n
encodeVal SIntT n = sleb128 n
encodeVal SInt8T n = B.int8 n
encodeVal SInt16T n = B.int16LE n
encodeVal SInt32T n = B.int32LE n
encodeVal SInt64T n = B.int64LE n
encodeVal SFloat32T n = B.floatLE n
encodeVal SFloat64T n = B.doubleLE n
encodeVal STextT t = leb128 (fromIntegral (BS.length bytes)) <> B.byteString bytes
  where bytes = T.encodeUtf8 t
encodeVal SNullT () = mempty
encodeVal SReservedT () = mempty
encodeVal (SOptT _) Nothing = B.word8 0
encodeVal (SOptT t) (Just x) = B.word8 1 <> encodeVal t x
encodeVal (SVecT t) xs =
    leb128 (fromIntegral (V.length xs)) <>
    foldMap (encodeVal t) xs
encodeVal (SRecT fs) rec =
    foldMap snd $ sortOn fst $ encodeRec fs rec
encodeVal (SVariantT fs) x =
    leb128 (fromIntegral i) <> b
  where
    (pos, b) = encodeVariant fs x
    m = map fst $ sortOn snd $ zip [0..] (map fieldHash (fromSFields fs))
    Just i = elemIndex pos m

-- Encodes the fields, sorting happens later
encodeRec :: SFields fs -> Rec fs -> [(Word32, B.Builder)]
encodeRec SFieldsNil () = []
encodeRec (SFieldsCons f fs) (x, xs) =
    (fieldHash (fromSField f), encodeVal (sFieldType f) x) : encodeRec fs xs

-- Encodes the value, returns fields and index
encodeVariant :: SFields fs -> Variant fs -> (Natural, B.Builder)
encodeVariant SFieldsNil x = case x of {}
encodeVariant (SFieldsCons f _) (Left x) = (0, encodeVal (sFieldType f) x)
encodeVariant (SFieldsCons _ fs) (Right v) = first succ (encodeVariant fs v)

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
        tis <- forM fs $ \f -> do
            ti <- go (fieldType f)
            return (fieldHash f, ti)
        addTyp $ mconcat
            [ sleb128 (-20)
            , leb128Len fs
            , foldMap (\(n,ti) -> leb128 (fromIntegral n) <> sleb128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]
    go (VariantT fs) = do
        tis <- forM fs $ \f -> do
            ti <- go (fieldType f)
            return (fieldHash f, ti)
        addTyp $ mconcat
            [ sleb128 (-21)
            , leb128Len fs
            , foldMap (\(n,ti) -> leb128 (fromIntegral n) <> sleb128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]


decode :: forall a. CandidArgs a => BS.ByteString -> Either String a
decode = G.runGet $ do
    decodeMagic
    arg_tys <- decodeTypTable
    -- get the argument sequence
    fromSeq <$> decodeParams (args @(ArgRep a)) arg_tys

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
decodeVal SNatT NatT = getLeb128
decodeVal SNat8T Nat8T = G.getWord8
decodeVal SNat16T Nat16T = G.getWord16le
decodeVal SNat32T Nat32T = G.getWord32le
decodeVal SNat64T Nat64T = G.getWord64le
decodeVal SIntT NatT = fromIntegral <$> getLeb128
decodeVal SIntT IntT = getSleb128
decodeVal SInt8T Int8T = G.getInt8
decodeVal SInt16T Int16T = G.getInt16le
decodeVal SInt32T Int32T = G.getInt32le
decodeVal SInt64T Int64T = G.getInt64le
decodeVal SFloat32T Float32T = G.getFloat32le
decodeVal SFloat64T Float64T = G.getFloat64le
decodeVal STextT TextT = do
    n <- getLeb128
    bs <- G.getByteString (fromIntegral n)
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
    n <- getLeb128
    V.fromList <$> replicateM (fromIntegral n) (decodeVal st t)
decodeVal (SRecT sfs) (RecT fs) = decodeRec sfs fs
decodeVal (SVariantT sfs) (VariantT fs) = do
        i <- getLeb128
        unless (i <= fromIntegral (length fs)) $ fail "variant index out of bound"
        let f = fs !! fromIntegral i
        decodeVariant sfs (fieldHash f) (fieldType f)
decodeVal s t = fail $ "unexpected type " ++ take 20 (show t) ++  " when decoding " ++ take 20 (show s)

decodeRec :: SFields fs -> Fields -> G.Get (Rec fs)
decodeRec SFieldsNil [] = return ()
decodeRec (SFieldsCons f _) [] = fail $ "missing field " <> show (fromSField f)
decodeRec sfs (f:dfs) =
    findField sfs (fieldHash f)
        (skipVal (fieldType f) >> decodeRec sfs dfs)
        (\st' sfs' sortIn -> do
            x <- decodeVal st' (fieldType f)
            xs <- decodeRec sfs' dfs
            return (x `sortIn` xs))

-- findField, in CPS style, produces a function with a type
-- that inserts the value in the right slot in the nested pairs
findField :: SFields fs ->
        Word32 ->
        a ->
        (forall t' fs'.
            SType t' -> SFields fs' ->
            (Val t' -> Rec fs' -> Rec fs) -> a
        ) ->
        a
findField SFieldsNil _ k1 _ = k1
findField (SFieldsCons sf sfs) h k1 k2
    | h == fieldHash (fromSField sf) = k2 (sFieldType sf) sfs (,)
    | otherwise = findField sfs h k1 $ \st' sfs' sortIn ->
        k2 st' (SFieldsCons sf sfs') (\x (y, ys) -> (y, x `sortIn` ys))

decodeVariant :: SFields fs -> Word32 -> Type -> G.Get (Variant fs)
decodeVariant SFieldsNil _ _ = fail "unexpected variant tag"
decodeVariant (SFieldsCons sf sfs) h t
        | h == fieldHash (fromSField sf) = Left <$> decodeVal (sFieldType sf) t
        | otherwise = Right <$> decodeVariant sfs h t

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
skipVal (RecT fs) = mapM_ (skipVal . fieldType) fs
skipVal (VariantT fs) = do
    i <- getLeb128
    unless (i <= fromIntegral (length fs)) $ fail "variant index out of bound"
    let f = fs !! fromIntegral i
    skipVal (fieldType f)


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

decodeTypField :: Natural -> G.Get (V.Vector Type -> Field)
decodeTypField max = do
    h <- getLeb128
    when (h > fromIntegral (maxBound :: Word32)) $ fail "Field hash too large"
    t <- decodeTypRef max
    return $ H_ (fromIntegral h) <$> t

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


fieldHash :: Field -> Word32
fieldHash (H _ _) = error "Nat on value level"
fieldHash (H_ n _) = n
fieldHash (N _ _) = error "Symbol in value level computation"
fieldHash (N_ s _) =
    BS.foldl (\h c -> (h * 223 + fromIntegral c)) 0 $ T.encodeUtf8 s

fieldType :: Field -> Type
fieldType (N _ _) = error "Symbol in value level computation"
fieldType (N_ _ t) = t
fieldType (H _ _) = error "Nat on value level"
fieldType (H_ _ t) = t

sFieldType :: SField f -> SType (FieldType f)
sFieldType (SN _ t) = t
sFieldType (SH _ t) = t

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

class KnownArgs (ArgRep a) => CandidArgs a where
    type ArgRep a :: [Type]
    toSeq :: a -> Seq (ArgRep a)
    fromSeq :: Seq (ArgRep a) -> a

    default toSeq :: Seq (ArgRep a) ~ a => a -> Seq (ArgRep a)
    toSeq = id
    default fromSeq :: Seq (ArgRep a) ~ a => Seq (ArgRep a) -> a
    fromSeq = id

newtype Unary a = Unary a deriving (Eq, Show)

{-
instance KnownArgs ts => CandidArgs (Seq ts) where
    type ArgRep (Seq ts) = ts
    toSeq = id
    fromSeq = id
-}

instance CandidArgs () where
    type ArgRep () = '[]

instance Candid a => CandidArgs (Unary a) where
    type ArgRep (Unary a) = '[Rep a]
    toSeq (Unary x) = (toCandid x, ())
    fromSeq (x, ()) = Unary $ fromCandid x

instance (Candid a, Candid b) => CandidArgs (a, b) where
    type ArgRep (a, b) = '[Rep a, Rep b]
    toSeq (x,y) = (toCandid x, (toCandid y, ()))
    fromSeq (x, (y, ())) = (fromCandid x, fromCandid y)

class KnownType (Rep a) => Candid a where
    type Rep a :: Type
    toCandid :: a -> Val (Rep a)
    fromCandid :: Val (Rep a) -> a

    default toCandid :: Val (Rep a) ~ a => a -> Val (Rep a)
    toCandid = id
    default fromCandid :: Val (Rep a) ~ a => Val (Rep a) -> a
    fromCandid = id

newtype CandidVal (t :: Type) where CandidVal :: Val t -> CandidVal t
deriving instance Eq (Val t) => Eq (CandidVal t)
deriving instance Show (Val t) => Show (CandidVal t)
instance KnownType t => Candid (CandidVal t) where
    type Rep (CandidVal t) = t
    toCandid (CandidVal x) = x
    fromCandid = CandidVal

newtype CandidSeq (t :: [Type]) where CandidSeq :: Seq t -> CandidSeq t
deriving instance Eq (Seq t) => Eq (CandidSeq t)
deriving instance Show (Seq t) => Show (CandidSeq t)
instance KnownArgs t => CandidArgs (CandidSeq t) where
    type ArgRep (CandidSeq t) = t
    toSeq (CandidSeq x) = x
    fromSeq = CandidSeq

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
instance Candid String where
    type Rep String = 'TextT
    toCandid = toCandid . T.pack
    fromCandid = T.unpack . fromCandid

instance Candid a => Candid (Maybe a) where
    type Rep (Maybe a) = 'OptT (Rep a)
    toCandid = fmap toCandid
    fromCandid = fmap fromCandid

instance Candid a => Candid (V.Vector a) where
    type Rep (V.Vector a) = 'VecT (Rep a)
    toCandid = fmap toCandid
    fromCandid = fmap fromCandid

instance (Candid a, Candid b) => Candid (a, b) where
    type Rep (a, b) = 'RecT '[ TupField 0 a, TupField 1 b]
    toCandid (x,y) = (toCandid x, (toCandid y, ()))
    fromCandid (x, (y, ())) = (fromCandid x, fromCandid y)

instance (Candid a, Candid b, Candid c) => Candid (a, b, c) where
    type Rep (a, b, c) = 'RecT '[ TupField 0 a, TupField 1 b, TupField 2 c]
    toCandid (x,y,z) = (toCandid x, (toCandid y, (toCandid z, ())))
    fromCandid (x, (y, (z, ()))) = (fromCandid x, fromCandid y, fromCandid z)

type TupField n a = 'H n (Rep a)

instance (Candid a, Candid b) => Candid (Either a b) where
    type Rep (Either a b) = 'VariantT '[ 'N "Left" (Rep a), 'N "Right" (Rep b) ]
    toCandid (Left x) = Left (toCandid x)
    toCandid (Right x) = Right (Left (toCandid x))
    fromCandid (Left x) = Left (fromCandid x)
    fromCandid (Right (Left x)) = Right (fromCandid x)
    fromCandid (Right (Right x)) = case x of {}

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
    SOptT :: SType t -> SType ('OptT t)
    SVecT :: SType t -> SType ('VecT t)
    SRecT :: SFields fs -> SType ('RecT fs)
    SVariantT :: SFields fs -> SType ('VariantT fs)

deriving instance Show (SType t)

data SFields (fs :: Fields) where
    SFieldsNil :: SFields '[]
    SFieldsCons :: SField f -> SFields fs -> SFields (f ': fs)

deriving instance Show (SFields fs)

data SArgs (t :: Args) where
    SArgsNil :: SArgs '[]
    SArgsCons :: SType t -> SArgs fs -> SArgs (t ': fs)
deriving instance Show (SArgs t)

data SField (n :: Field) where
    SN :: KnownSymbol s => Proxy s -> SType t -> SField ('N s t)
    SH :: KnownNat n => Proxy n -> SType t -> SField ('H n t)
deriving instance Show (SField n)

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

fromSFields :: SFields fs -> Fields
fromSFields SFieldsNil = []
fromSFields (SFieldsCons f fs) = fromSField f : fromSFields fs

fromSArgs :: SArgs fs -> Args
fromSArgs SArgsNil = []
fromSArgs (SArgsCons t fs) = fromSType t : fromSArgs fs

fromSField :: SField n -> Field
fromSField (SN p t) = N_ (T.pack (symbolVal p)) (fromSType t)
fromSField (SH p t) = H_ (fromIntegral (natVal p)) (fromSType t)


class KnownType (t :: Type) where typ :: SType t

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

class KnownFields (fs :: Fields) where fields :: SFields fs
instance KnownFields '[] where fields = SFieldsNil
instance (KnownField f, KnownFields fs) => KnownFields (f ': fs) where
    fields = SFieldsCons (field @f) (fields @fs)

class KnownArgs (t :: [Type]) where args :: SArgs t
instance KnownArgs '[] where args = SArgsNil
instance (KnownType t, KnownArgs ts) => KnownArgs (t ': ts) where
    args = SArgsCons (typ @t) (args @ts)

class KnownField (f :: Field) where field :: SField f
instance (KnownSymbol s, KnownType t) => KnownField ('N s t) where
    field = SN (Proxy @s) (typ @t)
instance (KnownNat s, KnownType t) => KnownField ('H s t) where
    field = SH (Proxy @s) (typ @t)

