{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Codec.Candid.Infer where

import qualified Data.Vector as V
import Control.Monad
import Data.Void
import Data.List
import Prettyprinter

import Codec.Candid.Types

inferTypes :: [Value] -> Either String [Type Void]
inferTypes = mapM inferTyp

inferTyp :: Value -> Either String (Type Void)
inferTyp (NumV v) = return $ if v >= 0 then NatT else IntT
inferTyp (BoolV _) = return BoolT
inferTyp (NatV _) = return NatT
inferTyp (Nat8V _) = return Nat8T
inferTyp (Nat16V _) = return Nat16T
inferTyp (Nat32V _) = return Nat32T
inferTyp (Nat64V _) = return Nat64T
inferTyp (IntV _) = return IntT
inferTyp (Int8V _) = return Int8T
inferTyp (Int16V _) = return Int16T
inferTyp (Int32V _) = return Int32T
inferTyp (Int64V _) = return Int64T
inferTyp (Float32V _) = return Float32T
inferTyp (Float64V _) = return Float64T
inferTyp (TextV _) = return TextT
inferTyp NullV = return NullT
inferTyp ReservedV = return ReservedT
inferTyp (OptV Nothing) = return $ OptT EmptyT
inferTyp (OptV (Just v)) = OptT <$> inferTyp v
inferTyp (RepeatV _ v) = VecT <$> inferTyp v
inferTyp (VecV vs) = VecT <$> (mapM inferTyp (V.toList vs) >>= lubs)
inferTyp (RecV fs) = RecT <$> sequence [ (fn,) <$> inferTyp t | (fn, t) <- fs ]
inferTyp (VariantV f v) = do
    t <- inferTyp v
    return $ VariantT [ (f, t) ]
inferTyp (TupV vs) = tupT <$> mapM inferTyp vs
inferTyp (FuncV _ _) = return (FuncT (MethodType [] [] False False False)) -- no principal type
inferTyp (ServiceV _) = return (ServiceT []) -- no principal type
inferTyp (PrincipalV _) = return PrincipalT
inferTyp FutureV = return FutureT
inferTyp (BlobV _) = return BlobT
inferTyp (AnnV _ t) = return t -- Maybe do type checking?

lubs :: [Type Void] -> Either String (Type Void)
lubs = foldM lub EmptyT

lub :: Type Void -> Type Void -> Either String (Type Void)
lub ReservedT _ = return ReservedT
lub _ ReservedT = return ReservedT
lub EmptyT t = return t
lub t EmptyT = return t
lub NatT IntT = return IntT
lub IntT NatT = return IntT
lub NullT (OptT t) = return (OptT t)
lub (OptT t) NullT = return (OptT t)
lub (OptT t1) (OptT t2) = OptT <$> lub t1 t2
lub (VecT t1) (VecT t2) = VecT <$> lub t1 t2
lub (RecT fs1) (RecT fs2) = RecT <$> go (sortOn fst fs1) (sortOn fst fs2)
  where
    go [] _ = return []
    go _ [] = return []
    go ((f1, v1):fs1) ((f2,v2):fs2)
        | f1 < f2   = go fs1 ((f2,v2):fs2)
        | f1 > f2   = go ((f1,v1):fs1) fs2
        | otherwise = (:) <$> ((f1,) <$> lub v1 v2) <*> go fs1 fs2
lub (VariantT fs1) (VariantT fs2) = VariantT <$> go (sortOn fst fs1) (sortOn fst fs2)
  where
    go [] fs = return fs
    go fs [] = return fs
    go ((f1, v1):fs1) ((f2,v2):fs2)
        | f1 < f2   = ((f1,v1) :) <$> go fs1 ((f2,v2):fs2)
        | f1 > f2   = ((f2,v2) :) <$> go ((f1,v1):fs1) fs2
        | otherwise = (:) <$> ((f1,) <$> lub v1 v2) <*> go fs1 fs2

-- the reflexive cases
lub NatT NatT = return NatT
lub Nat8T Nat8T = return Nat8T
lub Nat16T Nat16T = return Nat16T
lub Nat32T Nat32T = return Nat32T
lub Nat64T Nat64T = return Nat64T
lub IntT IntT = return IntT
lub Int8T Int8T = return Int8T
lub Int16T Int16T = return Int16T
lub Int32T Int32T = return Int32T
lub Int64T Int64T = return Int64T
lub Float32T Float32T = return Float32T
lub Float64T Float64T = return Float64T
lub BoolT BoolT = return BoolT
lub TextT TextT = return TextT
lub NullT NullT = return NullT
lub BlobT BlobT = return BlobT
lub PrincipalT PrincipalT = return PrincipalT

-- The shorthands
lub BlobT t@(VecT _) = lub (VecT Nat8T) t
lub t@(VecT _) BlobT = lub (VecT Nat8T) t

-- failure
lub t1 t2 = Left $ show $ "Incompatible types: " <+> pretty t1 <+> " and " <+> pretty t2
