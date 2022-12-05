{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Codec.Candid.Coerce
  ( coerceSeqDesc
  , coerce
  )
  where

import Prettyprinter
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Control.Monad.Except

import Codec.Candid.FieldName
import Codec.Candid.Types
import Codec.Candid.TypTable
import Codec.Candid.Subtype

coerceSeqDesc :: [Value] -> SeqDesc -> SeqDesc -> Either String [Value]
coerceSeqDesc vs sd1 sd2 =
    unrollTypeTable sd1 $ \ts1 ->
    unrollTypeTable sd2 $ \ts2 ->
    coerceSeq vs ts1 ts2

coerceSeq ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    [Value] ->
    [Type (Ref k1 Type)] ->
    [Type (Ref k2 Type)] ->
    Either String [Value]
coerceSeq vs t1 t2 = runSubTypeM $ goSeq vs t1 t2

-- | This function implements the @V : T ~> V' : T'@ relation from the Candid spec.
--
-- Because values in this library are untyped, we have to pass what we know about
-- their type down, so that we can do the subtype check upon a reference.
-- The given type must match the value closely (as it is the case when decoding
-- from the wire) and this function may behave oddly if @v@ and @t1@ are not related.
--
-- Morally, this function looks only at @v@ and @t2@. It only needs @t1@ for
-- refences, and hence needs to take @t2@ apart for the recursive calls.
-- Practically, it's sometimes more concise to look at t2 instead of v.
coerce ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    Value ->
    Type (Ref k1 Type) ->
    Type (Ref k2 Type) ->
    Either String Value
coerce v t1 t2 = runSubTypeM $ go v t1 t2

go ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    Value ->
    Type (Ref k1 Type) ->
    Type (Ref k2 Type) ->
    SubTypeM k1 k2 Value

goSeq ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    [Value] ->
    [Type (Ref k1 Type)] ->
    [Type (Ref k2 Type)] ->
    SubTypeM k1 k2 [Value]

-- Look through refs
go v (RefT (Ref _ t1)) t2 = go v t1 t2
go v t1 (RefT (Ref _ t2)) = go v t1 t2

-- Identity coercion for primitive values
go v NatT NatT = pure v
go v Nat8T Nat8T = pure v
go v Nat16T Nat16T = pure v
go v Nat32T Nat32T = pure v
go v Nat64T Nat64T = pure v
go v IntT IntT = pure v
go v Int8T Int8T = pure v
go v Int16T Int16T = pure v
go v Int32T Int32T = pure v
go v Int64T Int64T = pure v
go v Float32T Float32T = pure v
go v Float64T Float64T = pure v
go v BoolT BoolT = pure v
go v TextT TextT = pure v
go v NullT NullT = pure v
go v PrincipalT PrincipalT = pure v

-- Nat <: Int
go (NatV n) NatT IntT = pure $ IntV (fromIntegral n)

-- t <: reserved
go _ _ ReservedT = pure ReservedV

-- empty <: t (actually just a special case of `v :/ t`)
go v EmptyT _ = throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing empty"

-- vec t1 <: vec t2
go (VecV vs) (VecT t1) (VecT t2) = VecV <$> mapM (\v -> go v t1 t2) vs

-- Option: The normal rule
go (OptV Nothing)  (OptT _) (OptT _) = pure NullV
go (OptV (Just v)) (OptT t1) (OptT t2) =
    lift (runExceptT (go v t1 t2)) >>= \case
        Right v' -> pure (OptV (Just v'))
        Left _   -> pure (OptV Nothing)

-- Option: The constituent rule
go v t1 (OptT t2) | not (isOptLike t2) =
    lift (runExceptT (go v t1 t2)) >>= \case
        Right v' -> pure (OptV (Just v'))
        Left _   -> pure (OptV Nothing)

-- Option: The fallback rule
go _ _ (OptT _) = pure (OptV Nothing)

-- Records
go rv (RecT fs1) (RecT fs2) = do
    vm <- case rv of
        TupV ts -> pure $ M.fromList $ zip [hashedField n | n <- [0..]] ts
        RecV fvs -> pure $ M.fromList fvs
        v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing record"

    let m1 = M.fromList fs1
    fmap RecV $ forM fs2 $ \(fn, t2) -> (fn,) <$>
      case (M.lookup fn vm, M.lookup fn m1) of
        (Just v, Just t1) -> go v t1 t2
        _ -> case unRef t2 of
            OptT _ -> pure (OptV Nothing)
            ReservedT -> pure ReservedV
            t -> throwError $ show $ "Missing record field" <+> pretty fn <+> "of type" <+> pretty t

-- Variants
go (VariantV fn v) (VariantT fs1) (VariantT fs2) = do
    let m1 = M.fromList fs1
    let m2 = M.fromList fs2
    case (M.lookup fn m1, M.lookup fn m2) of
      (Just t1, Just t2) -> VariantV fn <$> go v t1 t2
      (Nothing, _) -> throwError $ show $ "Wrongly typed variant missing field " <+> pretty fn
      (_, Nothing) -> throwError $ show $ "Unexpected variant field" <+> pretty fn

-- Reference types
go v t1@(FuncT _) t2@(FuncT _) = isSubtypeOfM t1 t2 >> pure v
go v t1@(ServiceT _) t2@(ServiceT _) = isSubtypeOfM t1 t2 >> pure v

-- BlobT
go v BlobT BlobT = pure v
go (VecV vs) (VecT t) BlobT | isNat8 t = BlobV . BS.pack . V.toList <$> mapM goNat8 vs
   where
    goNat8 (Nat8V n) = pure n
    goNat8 v = throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing vec nat8 to blob"
go (BlobV b) BlobT (VecT t) | isNat8 t = pure $ VecV $ V.fromList $ map (Nat8V . fromIntegral) $ BS.unpack b

go v t1 t2 = throwError $ show $ "Cannot coerce " <+> pretty v <+> ":" <+> pretty t1 <+> "to type " <+> pretty t2

goSeq _ _ []  = pure []
goSeq vs ts1 (RefT (Ref _ t) : ts) = goSeq vs ts1 (t:ts)
goSeq vs@[] ts1@[] (OptT _    : ts) = (OptV Nothing :) <$> goSeq vs ts1 ts
goSeq vs@[] ts1@[] (ReservedT : ts) = (ReservedV :)    <$> goSeq vs ts1 ts
goSeq [] [] ts = throwError $ show $ "Argument type list too short, expecting types" <+> pretty ts
goSeq (v:vs) (t1:ts1) (t2:ts2) = do
    v' <- go v t1 t2
    vs' <- goSeq vs ts1 ts2
    pure $ v' : vs'
goSeq _ _ _ = throwError $ "Illtyped input to goSeq"

unRef :: Type (Ref a Type) -> Type (Ref a Type)
unRef (RefT (Ref _ t)) = unRef t
unRef t = t

isNat8 :: Type (Ref a Type) -> Bool
isNat8 (RefT (Ref _ t)) = isNat8 t
isNat8 Nat8T = True
isNat8 _ = False

-- | `null <: t`?
isOptLike :: Type (Ref a Type) -> Bool
isOptLike (RefT (Ref _ t)) = isOptLike t
isOptLike NullT = True
isOptLike (OptT _) = True
isOptLike ReservedT = True
isOptLike _ = False

