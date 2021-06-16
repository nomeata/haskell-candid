{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Codec.Candid.Coerce where

import Data.Text.Prettyprint.Doc
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Data.List
import Control.Monad.State.Lazy
import Control.Monad.Except

import Codec.Candid.FieldName
import Codec.Candid.Types
import Codec.Candid.TypTable

type SeqCoercion = [Value] -> Either String [Value]
type Coercion = Value -> Either String Value

coerceSeqDesc :: SeqDesc -> SeqDesc -> Either String SeqCoercion
coerceSeqDesc sd1 sd2 =
    unrollTypeTable sd1 $ \ts1 ->
    unrollTypeTable sd2 $ \ts2 ->
    coerceSeq ts1 ts2

coerceSeq :: (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    [Type (Ref k1 Type)] -> [Type (Ref k2 Type)] -> Either String SeqCoercion
coerceSeq _ []  = pure (const (return []))
coerceSeq ts1@[] (OptT _ : ts) = do
    cs2 <- coerceSeq ts1 ts
    pure $ \_vs -> do
        vs2 <- cs2 []
        return (OptV Nothing : vs2)
coerceSeq ts1@[] (ReservedT : ts) = do
    cs2 <- coerceSeq ts1 ts
    pure $ \_vs -> do
        vs2 <- cs2 []
        return (ReservedV : vs2)
coerceSeq [] ts =
    Left $ show $ "Argument type list too short, expecting types" <+> pretty ts
coerceSeq (t1:ts1) (t2:ts2) = do
    c1 <- coerce t1 t2
    cs2 <- coerceSeq ts1 ts2
    pure $ \case
        [] -> Left $ show $ "Expecting value of type:" <+> pretty t1
        (v:vs) -> do
            v' <- c1 v
            vs' <- cs2 vs
            return (v':vs')

-- | This function implements the `C[<t> <: <t>]` coercion function from the
-- spec. It returns `Left` if no subtyping relation holds, or `Right c` if it
-- holds, together with a coercion function.
--
-- The coercion function itself is not total because the intput value isn’t
-- typed, so we have to cater for errors there. It should not fail if the
-- passed value really is inherently of the input type.
--
-- In a dependently typed language we’d maybe have something like
-- `coerce :: foreach t1 -> foreach t2 -> Either String (t1 -> t2)`
-- instead, and thus return a total function
coerce ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    Type (Ref k1 Type) -> Type (Ref k2 Type) -> Either String Coercion

-- we do the memoization only for coerce, not coerceSeq. Might miss some
-- caching, but good enough.
coerce t1 t2 = evalState (runExceptT (memo t1 t2)) mempty

memo, go ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    Type (Ref k1 Type) ->
    Type (Ref k2 Type) ->
    ExceptT String (State (M.Map (Type (Ref k1 Type), Type (Ref k2 Type)) Coercion)) Coercion


-- Memoization uses lazyiness: When we see a pair for the first time,
-- we optimistically put the resulting coercion into the map.
-- Either the following recursive call will fail (but then this optimistic
-- value was never used), or it will succeed, but then the guess was correct.
memo t1 t2 = gets (M.lookup (t1,t2)) >>= \case
    Just c -> pure c
    Nothing -> mdo
        modify (M.insert (t1,t2) c)
        c <- go t1 t2
        return c

-- Look through refs
go (RefT (Ref _ t1)) t2 = go t1 t2
go t1 (RefT (Ref _ t2)) = go t1 t2

-- Identity coercion for primitive values
go NatT NatT = pure pure
go Nat8T Nat8T = pure pure
go Nat16T Nat16T = pure pure
go Nat32T Nat32T = pure pure
go Nat64T Nat64T = pure pure
go IntT IntT = pure pure
go Int8T Int8T = pure pure
go Int16T Int16T = pure pure
go Int32T Int32T = pure pure
go Int64T Int64T = pure pure
go Float32T Float32T = pure pure
go Float64T Float64T = pure pure
go BoolT BoolT = pure pure
go TextT TextT = pure pure
go NullT NullT = pure pure
go PrincipalT PrincipalT = pure pure

-- Nat <: Int
go NatT IntT = pure $ \case
    NatV n -> pure $ IntV (fromIntegral n)
    v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing nat <: int"

-- t <: reserved
go _ ReservedT = pure (const (pure ReservedV))

-- empty <: t
go EmptyT _ = pure $ \v ->
    throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing empty"

-- vec t1 <: vec t2
go (VecT t1) (VecT t2) = do
    c <- memo t1 t2
    pure $ \case
        VecV vs -> VecV <$> mapM c vs
        v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing vector"

-- Option: The normal rule
go (OptT t1) (OptT t2) = lift (runExceptT (memo t1 t2)) >>= \case
    Right c -> pure $ \case
        OptV Nothing -> pure (OptV Nothing)
        OptV (Just v) -> OptV . Just <$> c v
        v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing option"
    Left _ -> pure (const (pure (OptV Nothing)))

-- Option: The consituent rule
go t (OptT t2) | not (isOptLike t2) = lift (runExceptT (memo t t2)) >>= \case
    Right c -> pure $ \v -> OptV . Just <$> c v
    Left _ -> pure (const (pure (OptV Nothing)))
-- Option: The fallback rule
go _ (OptT _) = pure (const (pure (OptV Nothing)))

-- Records
go (RecT fs1) (RecT fs2) = do
    let m1 = M.fromList fs1
    let m2 = M.fromList fs2
    new_fields <- sequence
            [ case t of
                OptT _ -> pure (fn, OptV Nothing)
                ReservedT -> pure (fn, ReservedV)
                t -> throwError $ show $ "Missing record field" <+> pretty fn <+> "of type" <+> pretty t
            | (fn, t) <- M.toList $ m2 M.\\ m1
            ]
    field_coercions <- sequence
            [ do c <- memo t1 t2
                 pure $ \vm -> case M.lookup fn vm of
                    Nothing -> throwError $ show $ "Record value lacks field" <+> pretty fn <+> "of type" <+> pretty t1
                    Just v -> (fn, ) <$> c v
            | (fn, (t1, t2)) <- M.toList $ M.intersectionWith (,) m1 m2
            ]
    pure $ \case
        TupV ts -> do
            let vm = M.fromList $ zip [hashedField n | n <- [0..]] ts
            coerced_fields <- mapM ($ vm) field_coercions
            return $ RecV $ sortOn fst $ coerced_fields <> new_fields
        RecV fvs -> do
            let vm = M.fromList fvs
            coerced_fields <- mapM ($ vm) field_coercions
            return $ RecV $ sortOn fst $ coerced_fields <> new_fields
        v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing record"

-- Variants
go (VariantT fs1) (VariantT fs2) = do
    let m1 = M.fromList fs1
    let m2 = M.fromList fs2
    cm <- M.traverseWithKey (\fn t1 ->
        case M.lookup fn m2 of
            Just t2 -> memo t1 t2
            Nothing -> throwError $ show $ "Missing variant field" <+> pretty fn <+> "of type" <+> pretty t1
        ) m1
    pure $ \case
        VariantV fn v | Just c <- M.lookup fn cm -> VariantV fn <$> c v
                      | otherwise -> throwError $ show $ "Unexpected variant field" <+> pretty fn
        v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing variant"

-- Reference types (TODO)
go (FuncT _ta1 _tr1) (FuncT _ta2 _tr2) = pure pure
go (ServiceT _m1) (ServiceT _m2) = pure pure

-- BlobT
go BlobT BlobT = pure pure
go (VecT Nat8T) BlobT = pure $ \case
    VecV vs ->  BlobV . BS.pack . V.toList <$> mapM goNat8 vs
    v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing vec nat8 to blob"
   where
    goNat8 (Nat8V n) = pure n
    goNat8 v = throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing vec nat8 to blob"
go BlobT (VecT Nat8T) = pure $ \case
    BlobV b -> return $ VecV $ V.fromList $ map (Nat8V . fromIntegral) $ BS.unpack b
    v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing blob to vec nat8"



go t1 t2 = throwError $ show $ "Type" <+> pretty t1 <+> "is not a subtype of" <+> pretty t2


-- | `null <: t`?
isOptLike :: Type a -> Bool
isOptLike NullT = True
isOptLike (OptT _) = True
isOptLike ReservedT = True
isOptLike _ = True

