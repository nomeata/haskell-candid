{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-orphans #-}
-- | This (internal) module contains the encoding and decoding, as well
-- as the relevant classes
module Codec.Candid.Class where

import Numeric.Natural
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as B
import Data.Row
import Data.Row.Internal (Row(R), LT((:->)), metamorph)
import qualified Data.Row.Records as R
import qualified Data.Row.Internal as R
import qualified Data.Row.Variants as V
import Control.Monad.State.Lazy
import Control.Applicative ((<|>), Alternative)
import Data.Functor.Const
import Data.Bifunctor
import Data.Proxy
import Data.Typeable
import Data.Scientific
import Data.Word
import Data.Int
import Data.Void
import Data.Text.Prettyprint.Doc
import Data.Constraint ((\\))
import Language.Haskell.TH (mkName)
import Language.Haskell.TH.Lib (appT, tupleT, varT, litT, strTyLit, tupP, varP, labelE, varE, tupE)

import Codec.Candid.Tuples
import Codec.Candid.Data
import Codec.Candid.TypTable
import Codec.Candid.Types
import Codec.Candid.FieldName
import Codec.Candid.Decode
import Codec.Candid.Encode

-- | Encode based on Haskell type
encode :: CandidArg a => a -> BS.ByteString
encode = B.toLazyByteString . encodeBuilder

-- | Encode to a 'B.Builder' based on Haskell type
encodeBuilder :: forall a. CandidArg a => a -> B.Builder
encodeBuilder x = encodeValues (seqDesc @a) (toCandidVals x)

-- | Decode to Haskell type
decode :: forall a. CandidArg a => BS.ByteString -> Either String a
decode = decodeVals >=> fromCandidVals

-- | Decode values to Haskell type
fromCandidVals :: CandidArg a => [Value] -> Either String a
fromCandidVals = fromVals >=> return . fromTuple

toCandidVals :: CandidArg a => a -> [Value]
toCandidVals = seqVal . asTuple

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
    fromVals _ = Left "Not enough arguments"

instance (Candid a, Candid b) => CandidSeq (a, b) where
    asTypes = [asType' @a, asType' @b]
    seqVal (x, y) = [ toCandidVal x, toCandidVal y ]
    fromVals (x:y:_) = (,) <$> fromCandidVal x <*> fromCandidVal y
    fromVals _ = Left "Not enough arguments"

-- | The internal class of Haskell types that canonically map to Candid.
-- You would add instances to the 'Candid' type class.
class Typeable a => CandidVal a where
    asType :: Type (Ref TypeRep Type)
    toCandidVal' :: a -> Value
    fromCandidVal' :: Value -> Either String a

-- | The class of Haskell types that can be converted to Candid.
--
-- You can create intances of this class for your own types, see the tutorial above for examples. The default instance is mostly for internal use.
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
    fromCandidVal' (NumV n)
        | n >= 0, Right i <- floatingOrInteger @Double n = Right i
        | otherwise = Left $ "Not a natural number: " ++ show n
    fromCandidVal' (NatV n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

inBounds :: forall a. (Integral a, Bounded a) => Integer -> Either String a
inBounds i
    | fromIntegral (minBound :: a) <= i
    , fromIntegral (maxBound :: a) >= i
    = Right (fromIntegral i)
    | otherwise
    = Left $ "Out of bounds: " ++ show i

instance Candid Word8
instance CandidVal Word8 where
    asType = Nat8T
    toCandidVal' = Nat8V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Nat8V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word16
instance CandidVal Word16 where
    asType = Nat16T
    toCandidVal' = Nat16V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Nat16V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word32
instance CandidVal Word32 where
    asType = Nat32T
    toCandidVal' = Nat32V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Nat32V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Word64
instance CandidVal Word64 where
    asType = Nat64T
    toCandidVal' = Nat64V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Nat64V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Integer
instance CandidVal Integer where
    asType = IntT
    toCandidVal' = IntV
    fromCandidVal' (NumV n)
        | Right i <- floatingOrInteger @Double n = Right i
        | otherwise = Left $ "Not an integer: " ++ show n
    fromCandidVal' (NatV n) = Right (fromIntegral n)
    fromCandidVal' (IntV n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int8
instance CandidVal Int8 where
    asType = Int8T
    toCandidVal' = Int8V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Int8V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int16
instance CandidVal Int16 where
    asType = Int16T
    toCandidVal' = Int16V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Int16V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int32
instance CandidVal Int32 where
    asType = Int32T
    toCandidVal' = Int32V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Int32V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Int64
instance CandidVal Int64 where
    asType = Int64T
    toCandidVal' = Int64V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Int64V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Float
instance CandidVal Float where
    asType = Float32T
    toCandidVal' = Float32V
    fromCandidVal' (NumV n) = Right (toRealFloat n)
    fromCandidVal' (Float32V n) = Right n
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

instance Candid Double
instance CandidVal Double where
    asType = Float64T
    toCandidVal' = Float64V
    fromCandidVal' (NumV n) = Right (toRealFloat n)
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
    fromCandidVal' (VecV v) =  BS.pack . Vec.toList <$> mapM (fromCandidVal @Word8) v
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

instance Candid a => Candid (Vec.Vector a)
instance Candid a => CandidVal (Vec.Vector a) where
    asType = VecT (asType' @a)
    toCandidVal' = VecV . fmap toCandidVal
    fromCandidVal' (VecV x) = traverse fromCandidVal x
    fromCandidVal' (BlobV b) = traverse (fromCandidVal . Nat8V) $ Vec.fromList $ BS.unpack b
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

-- | Maybe a bit opinionated, but 'null' seems to be the unit of Candid
instance Candid ()
instance CandidVal () where
    asType = NullT
    toCandidVal' () = NullV
    fromCandidVal' NullV = Right ()
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

fromField :: FieldName -> [(FieldName, a)] -> Either String a
fromField f m = case lookup f m of
    Just v -> return v
    Nothing -> Left $ "Could not find field " ++ show (pretty f)

-- row-types integration

fieldOfRow :: forall r. Forall r Candid => Fields (Ref TypeRep Type)
fieldOfRow = getConst $ metamorph @_ @r @Candid @(Const ()) @(Const (Fields (Ref TypeRep Type))) @Proxy Proxy doNil doUncons doCons (Const ())
      where
        doNil :: Const () Empty -> Const (Fields (Ref TypeRep Type)) Empty
        doNil = const $ Const []
        doUncons :: forall l t r. (KnownSymbol l)
                 => Label l -> Const () ('R (l ':-> t ': r)) -> (Proxy t, Const () ('R r))
        doUncons _ _ = (Proxy, Const ())
        doCons :: forall l t r. (KnownSymbol l, Candid t)
               => Label l -> Proxy t -> Const (Fields (Ref TypeRep Type)) ('R r) -> Const (Fields (Ref TypeRep Type)) ('R (l ':-> t ': r))
        doCons l Proxy (Const lst) = Const $ (unescapeFieldName (R.toKey l), asType' @t) : lst


type CandidRow r = (Typeable r, AllUniqueLabels r, AllUniqueLabels (V.Map (Either String) r), Forall r Candid, Forall r R.Unconstrained1)

instance CandidRow r => Candid (Rec r)
instance CandidRow r => CandidVal (Rec r) where
    asType = RecT $ fieldOfRow @r

    toCandidVal' = do
        RecV . fmap (first unescapeFieldName) . R.eraseWithLabels @Candid @r @T.Text @Value toCandidVal

    fromCandidVal' = \case
        RecV m -> toRowRec m
        TupV m -> toRowRec (zip (map hashedField [0..]) m)
        v -> Left $ "Unexpected " ++ show (pretty v)
      where
        toRowRec m = R.fromLabelsA @Candid $ \l ->
            fromField (unescapeFieldName (R.toKey l)) m >>= fromCandidVal

instance CandidRow r => Candid (V.Var r)
instance CandidRow r => CandidVal (V.Var r) where
    asType = VariantT $ fieldOfRow @r

    toCandidVal' v = VariantV (unescapeFieldName t) val
      where (t, val) = V.eraseWithLabels @Candid toCandidVal v

    fromCandidVal' (VariantV f v) = do
        needle :: V.Var (V.Map (Either String) r) <-
            (fromLabelsMapA @Candid @_ @_ @r $ \l -> do
                guard (f == unescapeFieldName (R.toKey l))
                return $ fromCandidVal v
            ) <|> Left ("Unexpected variant tag " ++ show (pretty f))
        V.sequence (needle :: V.Var (V.Map (Either String) r))
    fromCandidVal' v = Left $ "Unexpected " ++ show (pretty v)

-- https://github.com/target/row-types/issues/66
fromLabelsMapA :: forall c f g ρ. (Alternative f, Forall ρ c, AllUniqueLabels ρ)
               => (forall l a. (KnownSymbol l, c a) => Label l -> f (g a)) -> f (V.Var (V.Map g ρ))
fromLabelsMapA f = V.fromLabels @(R.IsA c g) @(V.Map g ρ) @f inner
                \\ R.mapForall @g @c @ρ
                \\ R.uniqueMap @g @ρ
   where inner :: forall l a. (KnownSymbol l, R.IsA c g a) => Label l -> f a
         inner l = case R.as @c @g @a of R.As -> f l


-- Derived forms

instance Candid SBS.ByteString where
    type AsCandid SBS.ByteString = BS.ByteString
    toCandid = BS.fromStrict
    fromCandid = BS.toStrict

-- Tuples, generated by TH

-- This is what it looks like:
instance (Candid a, Candid b) => Candid (a, b) where
    type AsCandid (a,b) = Rec ("_0_" .== a .+ "_1_" .== b)
    toCandid (a,b) = #_0_ .== a .+ #_1_ .== b
    fromCandid r = (r .! #_0_, r .! #_1_)

$(
  let tupT ts  = foldl appT (tupleT (length ts)) ts in
  let fieldLabelT n = litT $ strTyLit ("_" ++ show (n::Int) ++ "_") in
  let fieldLabelE n = labelE ("_" ++ show (n::Int) ++ "_") in

  fmap concat . sequence $
  [
    let tvs = take n $ map (varT . mkName . (:[])) ['a'..]
        pvs = take n $ map (varP . mkName . (:[])) ['a'..]
        vs  = take n $ map (varE . mkName . (:[])) ['a'..]
    in [d|
     instance  $(tupT [ [t|Candid $v |] | v <- tvs ]) => Candid $(tupT tvs) where
        type AsCandid $(tupT tvs) =
          Rec $(
            foldr1 (\a b -> [t| $a .+ $b |])
              [ [t| $(fieldLabelT n) .== $b |]
              | (n,b) <- zip [0..] tvs ])
        toCandid $(tupP pvs) =
          $( foldr1 (\a b -> [| $a .+ $b |])
              [ [| $(fieldLabelE n) .== $b |]
              | (n,b) <- zip [0..] vs ])
        fromCandid $(varP (mkName "r")) =
          $( tupE [ [| $(varE (mkName "r")) .! $(fieldLabelE n) |]
                  | (n,_) <- zip [0..] vs])
     |]
  | n <- [3..15]
  ]
 )



instance Candid a => Candid [a] where
    type AsCandid [a] = Vec.Vector a
    toCandid = Vec.fromList
    fromCandid = Vec.toList


instance (Candid a, Candid b) => Candid (Either a b) where
    type AsCandid (Either a b) = V.Var ("Left" V..== a V..+ "Right" V..== b)
    toCandid (Left x) = IsJust (Label @"Left") x
    toCandid (Right x) = IsJust (Label @"Right") x
    fromCandid v = switch v $ empty
        .+ Label @"Left" .== Left
        .+ Label @"Right" .== Right

