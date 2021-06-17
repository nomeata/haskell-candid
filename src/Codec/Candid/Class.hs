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
{-# OPTIONS -Wno-orphans -Wno-deprecations #-}
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
import qualified Data.Row.Records as R
import qualified Data.Row.Internal as R
import qualified Data.Row.Variants as V
import Data.Row.Internal (metamorph)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Error
import Control.Applicative ((<|>))
import Data.Functor.Const
import Data.Bifunctor
import Data.Proxy
import Data.Typeable
import Data.Scientific
import Data.Word
import Data.Int
import Data.Void
import Data.Text.Prettyprint.Doc
import Language.Haskell.TH (mkName, tupleDataName)
import Language.Haskell.TH.Lib
  ( appT, tupleT, varT, litT, strTyLit
  , tupP, varP, wildP, infixP
  , labelE, varE, conE, tupE, listE, uInfixE
  )

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

-- see below for tuple  instances

data DeserializeError
    = DecodeError String -- ^ fatal
    | CoerceError String Value -- ^ can be recovered
    | MissingFieldError FieldName -- ^ can be recovered
    | UnexpectedTagError FieldName -- ^ can be recovered

-- TODO: Can we get rid of this?
instance Error DeserializeError where strMsg = DecodeError

isRecoverable :: DeserializeError -> Bool
isRecoverable (DecodeError _) = False
isRecoverable _ = True

recoverWith :: a -> Either DeserializeError a -> Either DeserializeError a
recoverWith x (Left e) | isRecoverable e = Right x
recoverWith _ y = y

showDeserializeError :: DeserializeError -> String
showDeserializeError e = case e of
    DecodeError err -> err
    CoerceError t v -> "Cannot coerce " ++ show (pretty v) ++ " into " ++ t
    MissingFieldError f -> "Missing field " ++ show (pretty f)
    UnexpectedTagError f -> "Unexpected tag " ++ show (pretty f)

cannotDecode :: String -> Either DeserializeError a
cannotDecode s = Left (DecodeError s)
cannotCoerce :: String -> Value -> Either DeserializeError a
cannotCoerce t v = Left (CoerceError t v)
missingField :: FieldName -> Either DeserializeError a
missingField f = Left (MissingFieldError f)
unexpectedTag :: FieldName -> Either DeserializeError a
unexpectedTag f = Left (UnexpectedTagError f)

-- | The internal class of Haskell types that canonically map to Candid.
-- You would add instances to the 'Candid' type class.
class Typeable a => CandidVal a where
    asType :: Type (Ref TypeRep Type)
    toCandidVal' :: a -> Value
    fromCandidVal' :: Value -> Either DeserializeError a
    fromMissingField :: Maybe a
    fromMissingField = Nothing

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
fromCandidVal = first showDeserializeError . fromCandidVal''

fromCandidVal'' :: Candid a => Value -> Either DeserializeError a
fromCandidVal'' = fmap fromCandid . fromCandidVal'

asType' :: forall a.  Candid a => Type (Ref TypeRep Type)
asType' = RefT (Ref (typeRep (Proxy @(AsCandid a))) (asType @(AsCandid a)))

instance Candid Bool
instance CandidVal Bool where
    asType = BoolT
    toCandidVal' = BoolV
    fromCandidVal' (BoolV b) = Right b
    fromCandidVal' v = cannotCoerce "bool" v

instance Candid Natural
instance CandidVal Natural where
    asType = NatT
    toCandidVal' = NatV
    fromCandidVal' (NumV n)
        | n >= 0, Right i <- floatingOrInteger @Double n = Right i
        | otherwise = cannotDecode $ "Not a natural number: " ++ show n
    fromCandidVal' (NatV n) = Right n
    fromCandidVal' v = cannotCoerce "nat" v

inBounds :: forall a. (Integral a, Bounded a) => Integer -> Either DeserializeError a
inBounds i
    | fromIntegral (minBound :: a) <= i
    , fromIntegral (maxBound :: a) >= i
    = Right (fromIntegral i)
    | otherwise
    = cannotDecode $ "Out of bounds: " ++ show i

instance Candid Word8
instance CandidVal Word8 where
    asType = Nat8T
    toCandidVal' = Nat8V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Nat8V n) = Right n
    fromCandidVal' v = cannotCoerce "word8" v

instance Candid Word16
instance CandidVal Word16 where
    asType = Nat16T
    toCandidVal' = Nat16V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Nat16V n) = Right n
    fromCandidVal' v = cannotCoerce "word16" v

instance Candid Word32
instance CandidVal Word32 where
    asType = Nat32T
    toCandidVal' = Nat32V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Nat32V n) = Right n
    fromCandidVal' v = cannotCoerce "word32" v

instance Candid Word64
instance CandidVal Word64 where
    asType = Nat64T
    toCandidVal' = Nat64V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Nat64V n) = Right n
    fromCandidVal' v = cannotCoerce "word64" v

instance Candid Integer
instance CandidVal Integer where
    asType = IntT
    toCandidVal' = IntV
    fromCandidVal' (NumV n)
        | Right i <- floatingOrInteger @Double n = Right i
        | otherwise = cannotDecode $ "Not an integer: " ++ show n
    fromCandidVal' (NatV n) = Right (fromIntegral n)
    fromCandidVal' (IntV n) = Right n
    fromCandidVal' v = cannotCoerce "int" v

instance Candid Int8
instance CandidVal Int8 where
    asType = Int8T
    toCandidVal' = Int8V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Int8V n) = Right n
    fromCandidVal' v = cannotCoerce "int8" v

instance Candid Int16
instance CandidVal Int16 where
    asType = Int16T
    toCandidVal' = Int16V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Int16V n) = Right n
    fromCandidVal' v = cannotCoerce "int16" v

instance Candid Int32
instance CandidVal Int32 where
    asType = Int32T
    toCandidVal' = Int32V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Int32V n) = Right n
    fromCandidVal' v = cannotCoerce "int32" v

instance Candid Int64
instance CandidVal Int64 where
    asType = Int64T
    toCandidVal' = Int64V
    fromCandidVal' (NumV n) | Right i <- floatingOrInteger @Double n = inBounds i
    fromCandidVal' (Int64V n) = Right n
    fromCandidVal' v = cannotCoerce "int64" v

instance Candid Float
instance CandidVal Float where
    asType = Float32T
    toCandidVal' = Float32V
    fromCandidVal' (NumV n) = Right (toRealFloat n)
    fromCandidVal' (Float32V n) = Right n
    fromCandidVal' v = cannotCoerce "float32" v

instance Candid Double
instance CandidVal Double where
    asType = Float64T
    toCandidVal' = Float64V
    fromCandidVal' (NumV n) = Right (toRealFloat n)
    fromCandidVal' (Float64V n) = Right n
    fromCandidVal' v = cannotCoerce "float64" v

instance Candid Void
instance CandidVal Void where
    asType = EmptyT
    toCandidVal' = absurd
    fromCandidVal' v = cannotCoerce "void" v

instance Candid T.Text
instance CandidVal T.Text where
    asType = TextT
    toCandidVal' = TextV
    fromCandidVal' (TextV t) = return t
    fromCandidVal' v = cannotCoerce "text" v

instance Candid BS.ByteString
instance CandidVal BS.ByteString where
    asType = BlobT
    toCandidVal' = BlobV
    fromCandidVal' (VecV v) =  BS.pack . Vec.toList <$> mapM (fromCandidVal'' @Word8) v
    fromCandidVal' (BlobV t) = return t
    fromCandidVal' v = cannotCoerce "blob" v

instance Candid Principal
instance CandidVal Principal where
    asType = PrincipalT
    toCandidVal' = PrincipalV
    fromCandidVal' (PrincipalV t) = return t
    fromCandidVal' v = cannotCoerce "principal" v

instance Candid ServiceRef
instance CandidVal ServiceRef where
    asType = ServiceT [] -- TODO
    toCandidVal' (ServiceRef p) = ServiceV p
    fromCandidVal' (ServiceV p) = return (ServiceRef p)
    fromCandidVal' v = cannotCoerce "service" v

instance Candid FuncRef
instance CandidVal FuncRef where
    asType = FuncT [] []-- TODO
    toCandidVal' (FuncRef (ServiceRef p) n) = FuncV p n
    fromCandidVal' (FuncV p n) = return (FuncRef (ServiceRef p) n)
    fromCandidVal' v = cannotCoerce "func" v

instance Candid Reserved
instance CandidVal Reserved where
    asType = ReservedT
    toCandidVal' Reserved = ReservedV
    fromCandidVal' _ = return Reserved
    fromMissingField = Just Reserved

instance Candid a => Candid (Maybe a)
instance Candid a => CandidVal (Maybe a) where
    asType = OptT (asType' @a)
    toCandidVal' = OptV . fmap toCandidVal
    fromCandidVal' (OptV x) = recoverWith Nothing $
        traverse fromCandidVal'' x
    fromCandidVal' NullV = return Nothing
    fromCandidVal' ReservedV = return Nothing
    fromCandidVal' v = case asType @(AsCandid a) of
        OptT _    -> pure Nothing
        NullT     -> pure Nothing
        ReservedT -> pure Nothing
        _         -> recoverWith Nothing $
            Just <$> fromCandidVal'' v
    fromMissingField = Just Nothing



instance Candid a => Candid (Vec.Vector a)
instance Candid a => CandidVal (Vec.Vector a) where
    asType = VecT (asType' @a)
    toCandidVal' = VecV . fmap toCandidVal
    fromCandidVal' (VecV x) = traverse fromCandidVal'' x
    fromCandidVal' (BlobV b) = traverse (fromCandidVal'' . Nat8V) $ Vec.fromList $ BS.unpack b
    fromCandidVal' v = cannotCoerce "vec" v

-- | Maybe a bit opinionated, but 'null' seems to be the unit of Candid
instance Candid ()
instance CandidVal () where
    asType = NullT
    toCandidVal' () = NullV
    fromCandidVal' NullV = Right ()
    fromCandidVal' v = cannotCoerce "null" v

-- row-types integration

fieldOfRow :: forall r. Forall r Candid => Fields (Ref TypeRep Type)
fieldOfRow = getConst $ metamorph @_ @r @Candid @(,) @(Const ()) @(Const (Fields (Ref TypeRep Type))) @Proxy Proxy doNil doUncons doCons (Const ())
      where
        doNil :: Const () Empty -> Const (Fields (Ref TypeRep Type)) Empty
        doNil = const $ Const []
        doUncons :: forall l t r. (KnownSymbol l, Candid t, HasType l t r)
                 => Label l -> Const () r -> (Const () (r .- l), Proxy t)
        doUncons _ _ = (Const (), Proxy)
        doCons :: forall l t r. (KnownSymbol l, Candid t)
               => Label l -> (Const (Fields (Ref TypeRep Type)) r, Proxy t) -> Const (Fields (Ref TypeRep Type)) (R.Extend l t r)
        doCons l (Const lst, Proxy) = Const $ (unescapeFieldName (R.toKey l), asType' @t) : lst


type CandidRow r = (Typeable r, AllUniqueLabels r, AllUniqueLabels (V.Map (Either String) r), Forall r Candid, Forall r R.Unconstrained1)

instance CandidRow r => Candid (Rec r)
instance CandidRow r => CandidVal (Rec r) where
    asType = RecT $ fieldOfRow @r

    toCandidVal' = do
        RecV . fmap (first unescapeFieldName) . R.eraseWithLabels @Candid @r @T.Text @Value toCandidVal

    fromCandidVal' = \case
        RecV m -> toRowRec m
        TupV m -> toRowRec (zip (map hashedField [0..]) m)
        v -> cannotCoerce "record" v
      where
        toRowRec m = R.fromLabelsA @Candid $ \l ->
            let fn = unescapeFieldName (R.toKey l) in
            case lookup fn m of
                Just v -> fromCandidVal'' v
                Nothing -> case fromMissingField of
                    Just v -> return (fromCandid v)
                    Nothing -> missingField fn

instance CandidRow r => Candid (V.Var r)
instance CandidRow r => CandidVal (V.Var r) where
    asType = VariantT $ fieldOfRow @r

    toCandidVal' v = VariantV (unescapeFieldName t) val
      where (t, val) = V.eraseWithLabels @Candid toCandidVal v

    fromCandidVal' (VariantV f v) = do
        needle :: V.Var (V.Map (Either DeserializeError) r) <-
            (V.fromLabelsMap @Candid @_ @_ @r $ \l -> do
                guard (f == unescapeFieldName (R.toKey l))
                return $ fromCandidVal'' v
            ) <|> unexpectedTag f
        V.sequence (needle :: V.Var (V.Map (Either DeserializeError) r))
    fromCandidVal' v = cannotCoerce "variant" v


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

instance (Candid a, Candid b) => CandidSeq (a, b) where
    asTypes = [asType' @a, asType' @b]
    seqVal (x, y) = [ toCandidVal x, toCandidVal y ]
    fromVals (x:y:_) = (,) <$> fromCandidVal x <*> fromCandidVal y
    fromVals _ = Left "Not enough arguments"

$(
  let tupT ts  = foldl appT (tupleT (length ts)) ts in
  let fieldLabelT n = litT $ strTyLit ("_" ++ show (n::Int) ++ "_") in
  let fieldLabelE n = labelE ("_" ++ show (n::Int) ++ "_") in

  fmap concat . sequence $
  [
    let names = take n $ map (mkName . (:[])) ['a'..]
        tvs = map varT names
        pvs = map varP names
        vs  = map varE names
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

      instance  $(tupT [ [t|Candid $v |] | v <- tvs ]) => CandidSeq $(tupT tvs) where
        asTypes = $(listE [ [| asType' @ $v |] | v <- tvs ])
        seqVal $(tupP pvs) = $(listE [ [| toCandidVal $v |] | v <- vs ])
        fromVals $(foldr (`infixP` '(:)) wildP pvs)
          = $( foldl (`uInfixE` varE '(<*>))
                [| pure $(conE (tupleDataName n)) |]
                [ [| fromCandidVal $v |] | v <- vs ] )
        fromVals _ = Left "Not enough arguments"
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
