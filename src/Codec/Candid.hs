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
module Codec.Candid where

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
-- import Data.Singletons
-- import Data.Singletons.TypeLits
-- import Data.Singletons.TH hiding (Null)

-- |
-- The type of candid values
-- (coinductively, without named types, i.e. after name resolution)
data Type
    = PrimType Prim
    | ConsType Cons

data Prim
    = NatT | Nat8T | Nat16T | Nat32T | Nat64T
    | IntT | Int8T | Int16T | Int32T | Int64T
    | Float32T | Float64T
    | BoolT
    | TextT
    | NullT
    | ReservedT
    | EmptyT

data Cons
    = OptT Type
    | VecT Type
    | RecT Fields
    | VariantT Fields

type Fields = [(FieldName, Type)]

data FieldName
   = Named Symbol
   | Named' T.Text
   {-
   | Hashed Word32
   -}

data FieldNameTerm
    = NamedTerm T.Text

-- genSingletons [''Type, ''Prim, ''Cons] -- singletons do not work well with strings

-- The values

{-
type family Val (t :: Type) = r | r -> t where
    Val ('PrimType 'NatT) = Natural
    Val ('PrimType 'Nat8T) = Word8
    Val ('PrimType 'Nat16T) = Word16
    Val ('PrimType 'Nat32T) = Word32
    Val ('PrimType 'Nat64T) = Word64
    Val ('PrimType 'IntT) = Integer
    Val ('PrimType 'Int8T) = Int8
    Val ('PrimType 'Int16T) = Int16
    Val ('PrimType 'Int32T) = Int32
    Val ('PrimType 'Int64T) = Int64
    Val ('PrimType 'Float32T) = Float
    Val ('PrimType 'Float64T) = Double
    Val ('PrimType 'BoolT) = Bool
    Val ('PrimType 'TextT) = T.Text
    Val ('PrimType 'NullT) = Null
    Val ('PrimType 'ReservedT) = Reserved
    Val ('PrimType 'EmptyT) = Void
    Val ('ConsType ('OptT t)) = Maybe (Val t)
    Val ('ConsType ('VecT t)) = V.Vector (Val t)
    Val ('ConsType ('RecT fs)) = Rec fs
    Val ('ConsType ('VariantT fs)) = Variant fs
-}

data Val (t :: Type) where
    NatV :: Natural -> Val ('PrimType 'NatT)
    Nat8V :: Word8 -> Val ('PrimType 'Nat8T)
    Nat16V :: Word16 -> Val ('PrimType 'Nat16T)
    Nat32V :: Word32 -> Val ('PrimType 'Nat32T)
    Nat64V :: Word64 -> Val ('PrimType 'Nat64T)
    IntV :: Integer -> Val ('PrimType 'IntT)
    Int8V :: Int8 -> Val ('PrimType 'Int8T)
    Int16V :: Int16 -> Val ('PrimType 'Int16T)
    Int32V :: Int32 -> Val ('PrimType 'Int32T)
    Int64V :: Int64 -> Val ('PrimType 'Int64T)
    Float32V :: Float -> Val ('PrimType 'Float32T)
    Float64V :: Double -> Val ('PrimType 'Float64T)
    BoolV :: Bool -> Val ('PrimType 'BoolT)
    TextV :: T.Text -> Val ('PrimType 'TextT)
    NullV :: Val ('PrimType 'NullT)
    ReservedV :: Val ('PrimType 'ReservedT)
    -- NB: No EmptyV
    OptV :: KnownType t => Maybe (Val t) -> Val ('ConsType ('OptT t))
    VecV :: KnownType t => V.Vector (Val t) -> Val ('ConsType ('VecT t))
    RecV :: KnownFields fs => Rec fs -> Val ('ConsType ('RecT fs))
    VariantV :: KnownFields fs => Variant fs -> Val ('ConsType ('VariantT fs))


data Null = Null
data Reserved = Reserved

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
    go (PrimType p) = return $ prim p
    go (ConsType (OptT t)) = do
        ti <- go t
        addTyp $ sleb128 (-18) <> sleb128 ti
    go (ConsType (VecT t)) = do
        ti <- go t
        addTyp $ sleb128 (-19) <> sleb128 ti
    go (ConsType (RecT fs)) = do
        tis <- forM fs $ \(fn, t) -> do
            ti <- go t
            return (hashFieldName fn, ti)
        addTyp $ mconcat
            [ sleb128 (-20)
            , leb128Len ts
            , foldMap (\(n,ti) -> leb128 (fromIntegral n) <> sleb128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]
    go (ConsType (VariantT fs)) = do
        tis <- forM fs $ \(fn, t) -> do
            ti <- go t
            return (hashFieldName fn, ti)
        addTyp $ mconcat
            [ sleb128 (-21)
            , leb128Len ts
            , foldMap (\(n,ti) -> leb128 (fromIntegral n) <> sleb128 ti) $
              sortOn fst tis -- TODO: Check duplicates maybe?
            ]

    prim :: Prim -> Integer
    prim NullT     = -1
    prim BoolT     = -2
    prim NatT      = -3
    prim IntT      = -4
    prim Nat8T     = -5
    prim Nat16T    = -6
    prim Nat32T    = -7
    prim Nat64T    = -8
    prim Int8T     = -9
    prim Int16T    = -10
    prim Int32T    = -11
    prim Int64T    = -12
    prim Float32T  = -13
    prim Float64T  = -14
    prim TextT     = -15
    prim ReservedT = -16
    prim EmptyT    = -17

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

instance KnownType ('PrimType 'NatT) where typ = PrimType NatT
instance KnownType ('PrimType 'Nat8T) where typ = PrimType Nat8T
instance KnownType ('PrimType 'Nat16T) where typ = PrimType Nat16T
instance KnownType ('PrimType 'Nat32T) where typ = PrimType Nat32T
instance KnownType ('PrimType 'Nat64T) where typ = PrimType Nat64T
instance KnownType ('PrimType 'IntT) where typ = PrimType IntT
instance KnownType ('PrimType 'Int8T) where typ = PrimType Int8T
instance KnownType ('PrimType 'Int16T) where typ = PrimType Int16T
instance KnownType ('PrimType 'Int32T) where typ = PrimType Int32T
instance KnownType ('PrimType 'Int64T) where typ = PrimType Int64T
instance KnownType ('PrimType 'Float32T) where typ = PrimType Float32T
instance KnownType ('PrimType 'Float64T) where typ = PrimType Float64T
instance KnownType ('PrimType 'TextT) where typ = PrimType TextT
instance KnownType ('PrimType 'BoolT) where typ = PrimType BoolT
instance KnownType ('PrimType 'NullT) where typ = PrimType NullT
instance KnownType ('PrimType 'ReservedT) where typ = PrimType ReservedT
instance KnownType ('PrimType 'EmptyT) where typ = PrimType EmptyT
instance KnownType t => KnownType ('ConsType ('OptT t)) where typ = ConsType (OptT (typ @t))
instance KnownType t => KnownType ('ConsType ('VecT t)) where typ = ConsType (VecT (typ @t))
instance KnownFields fs => KnownType ('ConsType ('RecT fs)) where typ = ConsType (RecT (fields @fs))

class KnownFields (t :: [(FieldName, Type)]) where fields :: [(FieldName, Type)]
instance KnownFields '[] where fields = []
instance (KnownFieldName n, KnownType t, KnownFields fs) => KnownFields ('(n,t) ': fs) where
    fields = (fieldName @n, typ @t) : fields @fs

class KnownFieldName (fn :: FieldName) where fieldName :: FieldName
instance KnownSymbol s => KnownFieldName ('Named s) where
    fieldName = Named' (T.pack (symbolVal @s @Proxy undefined))
