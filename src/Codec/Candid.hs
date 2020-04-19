{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Codec.Candid
    ( Type(..)
    , Fields
    , FieldName
    , Val(..)
    , Rec(..)
    , Variant(..)
    , encode
    )where

import Numeric.Natural
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Proxy
import Data.Bifunctor
import Data.Bits
import Data.Word
import Data.Int
import Data.List
import Control.Monad.RWS.Lazy
import GHC.TypeLits

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
   {-
   | Hashed Word32
   -}

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

data Rec (fs :: Fields) where
  EmptyRec :: Rec '[]
  (:>) :: (KnownFieldName f, KnownType t, KnownFields fs) =>
          Val t -> Rec fs -> Rec ('(f,t) ': fs)
infixr 5 :>

data Variant (fs :: [(FieldName, Type)]) where
  This :: KnownType t => Val t -> Variant ('(f,t) ': fs)
  Other :: KnownFields fs => Variant fs -> Variant ('(f,t) ': fs)

encode :: forall fs. KnownFields fs => Rec fs -> B.Builder
encode xs = mconcat
    [ B.stringUtf8 "DIDL"
    , typTable (map snd (fields @fs))
    , encodeSeq xs
    ]

-- Argument sequences, although represented as records
-- are always encoded in order
encodeSeq :: forall fs. KnownFields fs => Rec fs -> B.Builder
encodeSeq EmptyRec = mempty
encodeSeq (x :> xs) = encodeVal x <> encodeSeq xs

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
    m = sortOn snd $ zip [0..] (map (hashFieldName . fst) (fields @fs))
    Just i = lookup pos m


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
            , leb128Len ts
            , foldMap (\(n,ti) -> leb128 (fromIntegral n) <> sleb128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]
    go (VariantT fs) = do
        tis <- forM fs $ \(fn, t) -> do
            ti <- go t
            return (hashFieldName fn, ti)
        addTyp $ mconcat
            [ sleb128 (-21)
            , leb128Len ts
            , foldMap (\(n,ti) -> leb128 (fromIntegral n) <> sleb128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]


hashFieldName :: FieldName -> Word32
-- hashFieldName (Hashed n) = n
hashFieldName (Named _) = error "Symbol in value level computation"
hashFieldName (Named' s) =
    BS.foldl (\h c -> (h * 223 + fromIntegral c)) 0 $ T.encodeUtf8 s


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

-- Repetitive stuff for dependently types programming

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

class KnownFields (t :: [(FieldName, Type)]) where fields :: [(FieldName, Type)]
instance KnownFields '[] where fields = []
instance (KnownFieldName n, KnownType t, KnownFields fs) => KnownFields ('(n,t) ': fs) where
    fields = (fieldName @n, typ @t) : fields @fs

class KnownFieldName (fn :: FieldName) where fieldName :: FieldName
instance KnownSymbol s => KnownFieldName ('Named s) where
    fieldName = Named' (T.pack (symbolVal @s @Proxy undefined))
