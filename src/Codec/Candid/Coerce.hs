{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Codec.Candid.Coerce where

import Data.Void
import Data.Text.Prettyprint.Doc
import qualified Data.Map as M
import Data.List

import Codec.Candid.Types

type SeqCoercion = [Value] -> Either String [Value]
type Coercion = Value -> Either String Value

coerceSeq :: [Type Void] -> [Type Void] -> Either String SeqCoercion
coerceSeq _ []  = pure (const (return []))
coerceSeq [] (OptT _ : ts) = do
    cs2 <- coerceSeq [] ts
    pure $ \_vs -> do
        vs2 <- cs2 []
        return (OptV Nothing : vs2)
coerceSeq [] (ReservedT : ts) = do
    cs2 <- coerceSeq [] ts
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
-- instead
coerce :: Type Void -> Type Void -> Either String Coercion
-- Identity coercion for primitive values
coerce NatT NatT = pure pure
coerce Nat8T Nat8T = pure pure
coerce Nat16T Nat16T = pure pure
coerce Nat32T Nat32T = pure pure
coerce Nat64T Nat64T = pure pure
coerce IntT IntT = pure pure
coerce Int8T Int8T = pure pure
coerce Int16T Int16T = pure pure
coerce Int32T Int32T = pure pure
coerce Int64T Int64T = pure pure
coerce Float32T Float32T = pure pure
coerce Float64T Float64T = pure pure
coerce BoolT BoolT = pure pure
coerce TextT TextT = pure pure
coerce NullT NullT = pure pure

-- Nat <: Int
coerce NatT IntT = pure $ \case
    NatV n -> pure $ IntV (fromIntegral n)
    v -> Left $ show $ "Unexpected value" <+> pretty v <+> "while coercing nat <: int"

-- t <: reserved
coerce _ ReservedT = pure (const (pure ReservedV))

-- empty <: t
coerce EmptyT _ = pure $ \v ->
    Left $ show $ "Unexpected value" <+> pretty v <+> "while coercing empty"

-- vec t1 <: vec t2
coerce (VecT t1) (VecT t2) = do
    c <- coerce t1 t2
    pure $ \case
        VecV vs -> VecV <$> mapM c vs
        v -> Left $ show $ "Unexpected value" <+> pretty v <+> "while coercing vector"

-- Option: The normal rule
coerce (OptT t1) (OptT t2) | Right c <- coerce t1 t2 = pure $ \case
    OptV Nothing -> pure (OptV Nothing)
    OptV (Just v) -> OptV . Just <$> c v
    v -> Left $ show $ "Unexpected value" <+> pretty v <+> "while coercing option"
-- Option: The consituent rule
coerce t (OptT t2) | not (isOptLike t2), Right c <- coerce t t2 =
    pure $ \v -> OptV . Just <$> c v
-- Option: The fallback rule
coerce _ (OptT _) = pure (const (pure (OptV Nothing)))

-- Records
coerce (RecT fs1) (RecT fs2) = do
    let m1 = M.fromList fs1
    let m2 = M.fromList fs2
    new_fields <- sequence
            [ case t of
                OptT _ -> pure (fn, OptV Nothing)
                ReservedT -> pure (fn, ReservedV)
                t -> Left $ show $ "Missing record field" <+> pretty fn <+> "of type" <+> pretty t
            | (fn, t) <- M.toList $ m2 M.\\ m1
            ]
    field_coercions <- sequence
            [ do c <- coerce t1 t2
                 pure $ \vm -> case M.lookup fn vm of
                    Nothing -> Left $ show $ "Record value lacks field " <+> pretty fn <+> "of type" <+> pretty t1
                    Just v -> (fn, ) <$> c v
            | (fn, (t1, t2)) <- M.toList $ M.intersectionWith (,) m1 m2
            ]
    pure $ \case
        RecV fvs -> do
            let vm = M.fromList fvs
            coerced_fields <- mapM ($ vm) field_coercions
            return $ RecV $ sortOn fst $ coerced_fields <> new_fields
        v -> Left $ show $ "Unexpected value" <+> pretty v <+> "while coercing record"

-- | `null <: t`?
isOptLike :: Type a -> Bool
isOptLike NullT = True
isOptLike (OptT _) = True
isOptLike ReservedT = True
isOptLike _ = True

