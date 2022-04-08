{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Codec.Candid.Coerce
  ( coerceSeqDesc
  , SeqCoercion
  , coerce
  , Coercion
  )
  where

import Prettyprinter
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Data.Bifunctor
import Data.List
import Data.Tuple
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

coerceSeq ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    [Type (Ref k1 Type)] ->
    [Type (Ref k2 Type)] ->
    Either String SeqCoercion
coerceSeq t1 t2 = runM $ goSeq t1 t2

-- | This function implements the `V : T ~> V' : T'` relation from the Candid spec.
--
-- `C[<t> <: <t>]` coercion function from the
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
    Type (Ref k1 Type) ->
    Type (Ref k2 Type) ->
    Either String Coercion
coerce t1 t2 = runM $ memo t1 t2

type Memo k1 k2 =
    (M.Map (Type (Ref k1 Type), Type (Ref k2 Type)) Coercion,
     M.Map (Type (Ref k2 Type), Type (Ref k1 Type)) Coercion)
type M k1 k2 = ExceptT String (State (Memo k1 k2))

runM :: (Ord k1, Ord k2) => M k1 k2 a -> Either String a
runM act = evalState (runExceptT act) (mempty, mempty)

flipM :: M k1 k2 a -> M k2 k1 a
flipM (ExceptT (StateT f)) = ExceptT (StateT f')
  where
    f' (m1,m2) = second swap <$> f (m2,m1) -- f (m2,m1) >>= \case (r, (m2',m1')) -> pure (r, (m1', m2'))

memo, go ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    Type (Ref k1 Type) ->
    Type (Ref k2 Type) ->
    M k1 k2 Coercion

goSeq ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    [Type (Ref k1 Type)] ->
    [Type (Ref k2 Type)] ->
    M k1 k2 SeqCoercion


-- Memoization uses lazyiness: When we see a pair for the first time,
-- we optimistically put the resulting coercion into the map.
-- Either the following recursive call will fail (but then this optimistic
-- value was never used), or it will succeed, but then the guess was correct.
memo t1 t2 = do
  gets (M.lookup (t1,t2) . fst) >>= \case
    Just c -> pure c
    Nothing -> mdo
        modify (first (M.insert (t1,t2) c))
        c <- go t1 t2
        return c

-- Look through refs
go (RefT (Ref _ t1)) t2 = memo t1 t2
go t1 (RefT (Ref _ t2)) = memo t1 t2

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

-- Option: The constituent rule
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
            [ case unRef t of
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

-- Reference types
go (FuncT mt1) (FuncT mt2) = goMethodType mt1 mt2 >> pure pure
go (ServiceT meths1) (ServiceT meths2) = do
    let m1 = M.fromList meths1
    forM_ meths2 $ \(m, mt2) -> case M.lookup m m1 of
        Just mt1 -> goMethodType mt1 mt2
        Nothing -> throwError $ show $ "Missing service method" <+> pretty m <+> "of type" <+> pretty mt2
    pure pure

-- BlobT
go BlobT BlobT = pure pure
go (VecT t) BlobT | isNat8 t = pure $ \case
    VecV vs ->  BlobV . BS.pack . V.toList <$> mapM goNat8 vs
    v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing vec nat8 to blob"
   where
    goNat8 (Nat8V n) = pure n
    goNat8 v = throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing vec nat8 to blob"
go BlobT (VecT t) | isNat8 t = pure $ \case
    BlobV b -> return $ VecV $ V.fromList $ map (Nat8V . fromIntegral) $ BS.unpack b
    v -> throwError $ show $ "Unexpected value" <+> pretty v <+> "while coercing blob to vec nat8"

go t1 t2 = throwError $ show $ "Type" <+> pretty t1 <+> "is not a subtype of" <+> pretty t2

goMethodType ::
    (Pretty k2, Pretty k1, Ord k2, Ord k1) =>
    MethodType (Ref k1 Type) ->
    MethodType (Ref k2 Type) ->
    M k1 k2 ()
goMethodType (MethodType ta1 tr1 q1 o1) (MethodType ta2 tr2 q2 o2) = do
    unless (q1 == q2) $ throwError "Methods differ in query annotation"
    unless (o1 == o2) $ throwError "Methods differ in oneway annotation"
    void $ flipM $ goSeq ta2 ta1
    void $ goSeq tr1 tr2

goSeq _ []  = pure (const (return []))
goSeq ts1 (RefT (Ref _ t) : ts) = goSeq ts1 (t:ts)
goSeq ts1@[] (NullT  : ts) = do
    cs2 <- goSeq ts1 ts
    pure $ \_vs -> (NullV :) <$> cs2 []
goSeq ts1@[] (OptT _ : ts) = do
    cs2 <- goSeq ts1 ts
    pure $ \_vs -> (OptV Nothing :) <$> cs2 []
goSeq ts1@[] (ReservedT : ts) = do
    cs2 <- goSeq ts1 ts
    pure $ \_vs -> (ReservedV :) <$> cs2 []
goSeq [] ts =
    throwError $ show $ "Argument type list too short, expecting types" <+> pretty ts
goSeq (t1:ts1) (t2:ts2) = do
    c1 <- memo t1 t2
    cs2 <- goSeq ts1 ts2
    pure $ \case
        [] -> throwError $ show $ "Expecting value of type:" <+> pretty t1
        (v:vs) -> do
            v' <- c1 v
            vs' <- cs2 vs
            return (v':vs')

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

