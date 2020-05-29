{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
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
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.Map as M
import Data.Row
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
import Data.Text.Prettyprint.Doc

import Codec.Candid.Tuples
import Codec.Candid.Data
import Codec.Candid.TypTable
import Codec.Candid.Types
import Codec.Candid.Decode

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
    t = seqDesc @a

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

typTable :: SeqDesc -> B.Builder
typTable (SeqDesc m (ts :: [Type k])) = mconcat
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

buildLEB128Int :: Int -> B.Builder
buildLEB128Int = buildLEB128 @Natural . fromIntegral

leb128Len :: [a] -> B.Builder
leb128Len = buildLEB128Int . length


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

