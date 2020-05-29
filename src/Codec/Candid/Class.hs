{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-orphans #-}
-- | This (internal) module contains the encoding and decoding, as well
-- as the relevant classes
module Codec.Candid.Class where

import Numeric.Natural
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import Data.Row
import qualified Data.Row.Records as R
import qualified Data.Row.Internal as R
import qualified Data.Row.Variants as V
import Control.Monad.State.Lazy
import Data.Proxy
import Data.Typeable
import Data.Word
import Data.Int
import Data.Void
import Data.Text.Prettyprint.Doc

import Codec.Candid.Tuples
import Codec.Candid.Data
import Codec.Candid.TypTable
import Codec.Candid.Types
import Codec.Candid.Decode
import Codec.Candid.Encode

-- | Encode based on Haskell type
encode :: CandidArg a => a -> BS.ByteString
encode = BSL.toStrict . B.toLazyByteString . encodeBuilder

-- | Encode to a 'B.Builder' based on Haskell type
encodeBuilder :: forall a. CandidArg a => a -> B.Builder
encodeBuilder x = encodeValues t (seqVal (asTuple x))
  where
    t = seqDesc @a


-- | Decode to Haskell type
decode :: forall a. CandidArg a => BS.ByteString -> Either String a
decode = decodeVals >=> fromVals >=> return . fromTuple

-- Using normal Haskell values

-- | The class of types that can be used as Candid argument sequences.
-- Essentially all types that are in 'Candid', but tuples need to be treated specially.
type CandidArg a = (CandidSeq (AsTuple a), Tuplable a)


class CandidSeq a where
    asTypes :: [Type (Ref TypeRep Type)]
    seqVal :: a -> [Value]
    fromVals :: [Value] -> Either String a

seqDesc :: forall a. CandidArg a => SeqDesc
seqDesc = buildSeqDesc (asTypes @(AsTuple a))

-- | NB: This will loop with recursive types!
typeDesc :: forall a. Candid a => Type Void
typeDesc = asType @(AsCandid a) >>= go
  where go (Ref _ t) = t >>= go

instance Pretty TypeRep where
    pretty = pretty . show

instance CandidSeq () where
    asTypes = []
    seqVal () = []
    fromVals _ = return () -- Subtyping

instance Candid a => CandidSeq (Unary a) where
    asTypes = [asType' @a]
    seqVal (Unary x) = [ toCandidVal x ]
    fromVals (x:_) = Unary <$> fromCandidVal x -- Subtyping
    fromVals _ = error "Not enough arguments"

instance (Candid a, Candid b) => CandidSeq (a, b) where
    asTypes = [asType' @a, asType' @b]
    seqVal (x, y) = [ toCandidVal x, toCandidVal y ]
    fromVals (x:y:_) = (,) <$> fromCandidVal x <*> fromCandidVal y
    fromVals _ = error "Not enough arguments"

-- | The internal class of Haskell types that canonically map to Candid.
-- You would add instances to the 'Candid' type class.
class Typeable a => CandidVal a where
    asType :: Type (Ref TypeRep Type)
    toCandidVal' :: a -> Value
    fromCandidVal' :: Value -> Either String a

-- | The class of Haskell types that can be converted to Candid.
--
-- You can create intances of this class for your own types, see the overview above for examples. The default instance is mostly for internal use.
class (Typeable a, CandidVal (AsCandid a)) => Candid a where
    type AsCandid a
    toCandid :: a -> AsCandid a
    fromCandid :: AsCandid a -> a

    type AsCandid a = a
    default toCandid :: a ~ AsCandid a => a -> AsCandid a
    toCandid = id
    default fromCandid :: a ~ AsCandid a => AsCandid a -> a
    fromCandid = id

toCandidVal :: Candid a => a -> Value
toCandidVal = toCandidVal' . toCandid

fromCandidVal :: Candid a => Value -> Either String a
fromCandidVal = fmap fromCandid . fromCandidVal'

asType' :: forall a.  Candid a => Type (Ref TypeRep Type)
asType' = RefT (Ref (typeRep (Proxy @(AsCandid a))) (asType @(AsCandid a)))

instance Candid Bool
instance CandidVal Bool where
    asType = BoolT
    toCandidVal' = BoolV
    fromCandidVal' (BoolV b) = Right b
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Natural
instance CandidVal Natural where
    asType = NatT
    toCandidVal' = NatV
    fromCandidVal' (NatV n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word8
instance CandidVal Word8 where
    asType = Nat8T
    toCandidVal' = Nat8V
    fromCandidVal' (Nat8V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word16
instance CandidVal Word16 where
    asType = Nat16T
    toCandidVal' = Nat16V
    fromCandidVal' (Nat16V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word32
instance CandidVal Word32 where
    asType = Nat32T
    toCandidVal' = Nat32V
    fromCandidVal' (Nat32V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word64
instance CandidVal Word64 where
    asType = Nat64T
    toCandidVal' = Nat64V
    fromCandidVal' (Nat64V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Integer
instance CandidVal Integer where
    asType = IntT
    toCandidVal' = IntV
    fromCandidVal' (NatV n) = Right (fromIntegral n)
    fromCandidVal' (IntV n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int8
instance CandidVal Int8 where
    asType = Int8T
    toCandidVal' = Int8V
    fromCandidVal' (Int8V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int16
instance CandidVal Int16 where
    asType = Int16T
    toCandidVal' = Int16V
    fromCandidVal' (Int16V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int32
instance CandidVal Int32 where
    asType = Int32T
    toCandidVal' = Int32V
    fromCandidVal' (Int32V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int64
instance CandidVal Int64 where
    asType = Int64T
    toCandidVal' = Int64V
    fromCandidVal' (Int64V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Float
instance CandidVal Float where
    asType = Float32T
    toCandidVal' = Float32V
    fromCandidVal' (Float32V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Double
instance CandidVal Double where
    asType = Float64T
    toCandidVal' = Float64V
    fromCandidVal' (Float64V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Void
instance CandidVal Void where
    asType = EmptyT
    toCandidVal' = absurd
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid T.Text
instance CandidVal T.Text where
    asType = TextT
    toCandidVal' = TextV
    fromCandidVal' (TextV t) = return t
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid BS.ByteString
instance CandidVal BS.ByteString where
    asType = BlobT
    toCandidVal' = BlobV
    -- should avoid going through vector here somehow
    fromCandidVal' (VecV v) =  BS.pack . V.toList <$> mapM (fromCandidVal @Word8) v
    fromCandidVal' (BlobV t) = return t
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Principal
instance CandidVal Principal where
    asType = PrincipalT
    toCandidVal' = PrincipalV
    fromCandidVal' (PrincipalV t) = return t
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Reserved
instance CandidVal Reserved where
    asType = ReservedT
    toCandidVal' Reserved = ReservedV
    fromCandidVal' _ = return Reserved

instance Candid a => Candid (Maybe a)
instance Candid a => CandidVal (Maybe a) where
    asType = OptT (asType' @a)
    toCandidVal' = OptV . fmap toCandidVal
    fromCandidVal' (OptV x) = traverse fromCandidVal x
    fromCandidVal' NullV = return Nothing
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid a => Candid (V.Vector a)
instance Candid a => CandidVal (V.Vector a) where
    asType = VecT (asType' @a)
    toCandidVal' = VecV . fmap toCandidVal
    fromCandidVal' (VecV x) = traverse fromCandidVal x
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

-- | Maybe a bit opinionated, but 'null' seems to be the unit of Candid
instance Candid ()
instance CandidVal () where
    asType = NullT
    toCandidVal' () = NullV
    fromCandidVal' NullV = Right ()
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

fromField :: FieldName -> [(FieldName, a)] -> Either String a
fromField f m = case lookupField f m of
    Just v -> return v
    Nothing -> Left $ "Could not find field " ++ show (pretty f)

-- row-types integration

class Typeable r => FromRowRec r where
    asTypesRec :: Fields (Ref TypeRep Type)
    fromRowRec :: Rec r -> [(FieldName, Value)]
    toRowRec :: [(FieldName, Value)] -> Either String (Rec r)

instance FromRowRec ('R.R '[]) where
    asTypesRec = []
    fromRowRec _ = mempty
    toRowRec _ = return empty

instance (Candid t, R.KnownSymbol f, FromRowRec ('R.R xs), Typeable xs) => FromRowRec ('R.R (f 'R.:-> t ': xs)) where
    asTypesRec = (N (R.toKey l), asType' @t) : asTypesRec @('R.R xs)
      where l = Label @f
    fromRowRec r = (N (R.toKey l), toCandidVal (r .! l)) : fromRowRec (r .- l)
      where l = Label @f
    toRowRec m = do
        v <- fromField (N (R.toKey l)) m
        x <- fromCandidVal @t v
        r' <- toRowRec @('R.R xs) m
        return $ R.unsafeInjectFront l x r'
      where l = Label @f

instance FromRowRec r => Candid (Rec r)
instance FromRowRec r => CandidVal (Rec r) where
    asType = RecT (asTypesRec @r)
    toCandidVal' = RecV . fromRowRec
    fromCandidVal' (RecV m) = toRowRec m
    fromCandidVal' (TupV m) = toRowRec (zip (map escapeFieldHash [0..]) m)
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

class Typeable r => FromRowVar r where
    asTypesVar :: Fields (Ref TypeRep Type)
    fromRowVar :: V.Var r -> (FieldName, Value)
    toRowVar :: FieldName -> Value -> Either String (V.Var r)

instance FromRowVar ('R.R '[]) where
    asTypesVar = []
    fromRowVar = V.impossible
    toRowVar f v = Left $ "Unexpected " ++ show (pretty (VariantV f v))

instance (Candid t, R.KnownSymbol f, FromRowVar ('R.R xs), Typeable xs) => FromRowVar ('R.R (f 'R.:-> t ': xs)) where
    asTypesVar = (N (R.toKey l), asType' @t) : asTypesVar @('R.R xs)
      where l = R.Label @f
    fromRowVar r = case V.trial r l of
        Left x -> (N (R.toKey l), toCandidVal x)
        Right r' -> fromRowVar @('R.R xs) r'
      where l = R.Label @f
    toRowVar f v
        | hashFieldName f == hashFieldName (N (R.toKey l))
        = V.unsafeMakeVar l <$> fromCandidVal v
        | otherwise
        = V.unsafeInjectFront <$> toRowVar @('R.R xs) f v
      where l = R.Label @f

instance FromRowVar r => Candid (V.Var r)
instance FromRowVar r => CandidVal (V.Var r) where
    asType = VariantT (asTypesVar @r)
    toCandidVal' = uncurry VariantV . fromRowVar
    fromCandidVal' (VariantV f v) = toRowVar f v
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

-- Derived forms

instance Candid BSL.ByteString where
    type AsCandid BSL.ByteString = BS.ByteString
    toCandid = BSL.toStrict
    fromCandid = BSL.fromStrict

instance (Candid a, Candid b) => Candid (a, b) where
    type AsCandid (a,b) = Rec ("_0_" .== a .+ "_1_" .== b)
    toCandid (a,b) = #_0_ .== a .+ #_1_ .== b
    fromCandid r = (r .! #_0_, r .! #_1_)

instance (Candid a, Candid b, Candid c) => Candid (a, b, c) where
    type AsCandid (a,b,c) = Rec ("_0_" .== a .+ "_1_" .== b .+ "_2_" .== c)
    toCandid (a,b,c) = #_0_ .== a .+ #_1_ .== b .+ #_2_ .== c
    fromCandid r = (r .! #_0_, r .! #_1_, r .! #_2_)


instance Candid a => Candid [a] where
    type AsCandid [a] = V.Vector a
    toCandid = V.fromList
    fromCandid = V.toList


instance (Candid a, Candid b) => Candid (Either a b) where
    type AsCandid (Either a b) = V.Var ("Left" V..== a V..+ "Right" V..== b)
    toCandid (Left x) = IsJust (Label @"Left") x
    toCandid (Right x) = IsJust (Label @"Right") x
    fromCandid v = switch v $ empty
        .+ Label @"Left" .== Left
        .+ Label @"Right" .== Right

