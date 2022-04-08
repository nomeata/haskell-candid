{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Codec.Candid.Subtype
  ( isSubtypeOf
  , SubTypeM
  , runSubTypeM
  , isSubtypeOfM
  )
  where

import Prettyprinter
import qualified Data.Map as M
import Data.Bifunctor
import Data.Tuple
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except

import Codec.Candid.Types
import Codec.Candid.TypTable

type Memo k1 k2 =
    (M.Map (Type (Ref k1 Type), Type (Ref k2 Type)) (Either String ()),
     M.Map (Type (Ref k2 Type), Type (Ref k1 Type)) (Either String ()))

type SubTypeM k1 k2 = ExceptT String (State (Memo k1 k2))

runSubTypeM :: (Ord k1, Ord k2) => SubTypeM k1 k2 a -> Either String a
runSubTypeM act = evalState (runExceptT act) (mempty, mempty)

isSubtypeOf ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    Type (Ref k1 Type) ->
    Type (Ref k2 Type) ->
    Either String ()
isSubtypeOf t1 t2 = runSubTypeM $ isSubtypeOfM t1 t2

isSubtypeOfM ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    Type (Ref k1 Type) ->
    Type (Ref k2 Type) ->
    SubTypeM k1 k2 ()
isSubtypeOfM t1 t2 = memo t1 t2

flipM :: SubTypeM k1 k2 a -> SubTypeM k2 k1 a
flipM (ExceptT (StateT f)) = ExceptT (StateT f')
  where
    f' (m1,m2) = second swap <$> f (m2,m1) -- f (m2,m1) >>= \case (r, (m2',m1')) -> pure (r, (m1', m2'))

memo, go ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    Type (Ref k1 Type) ->
    Type (Ref k2 Type) ->
    SubTypeM k1 k2 ()

goSeq ::
    (Pretty k1, Pretty k2, Ord k1, Ord k2) =>
    [Type (Ref k1 Type)] ->
    [Type (Ref k2 Type)] ->
    SubTypeM k1 k2 ()


-- Memoization: When we see a pair for the first time,
-- we optimistically put 'True' into the map.
-- Either the following recursive call will fail (but then this optimistic
-- value wasn't a problem), or it will succeed, but then the guess was correct.
-- If it fails we put 'False' into it, to as a caching optimization
memo t1 t2 = do
  gets (M.lookup (t1,t2) . fst) >>= \case
    Just r -> except r
    Nothing -> assume_ok >> (go t1 t2 `catchE` remember_failure)
  where
    remember r         = modify (first (M.insert (t1,t2) r))
    assume_ok          = remember (Right ())
    remember_failure e = remember (Left e) >> throwError e

-- Look through refs
go (RefT (Ref _ t1)) t2 = memo t1 t2
go t1 (RefT (Ref _ t2)) = memo t1 t2

-- Identity coercion for primitive values
go NatT NatT = pure ()
go Nat8T Nat8T = pure ()
go Nat16T Nat16T = pure ()
go Nat32T Nat32T = pure ()
go Nat64T Nat64T = pure ()
go IntT IntT = pure ()
go Int8T Int8T = pure ()
go Int16T Int16T = pure ()
go Int32T Int32T = pure ()
go Int64T Int64T = pure ()
go Float32T Float32T = pure ()
go Float64T Float64T = pure ()
go BoolT BoolT = pure ()
go TextT TextT = pure ()
go NullT NullT = pure ()
go PrincipalT PrincipalT = pure ()

-- Nat <: Int
go NatT IntT = pure ()

-- t <: reserved
go _ ReservedT = pure ()

-- empty <: t
go EmptyT _ = pure ()

-- vec t1 <: vec t2
go (VecT t1) (VecT t2) = memo t1 t2

-- Option: very simple
go _ (OptT _) = pure ()

-- Records
go (RecT fs1) (RecT fs2) = do
    let m1 = M.fromList fs1
    let m2 = M.fromList fs2
    -- Check missing fields
    sequence_
      [ case unRef t of
          OptT _ -> pure ()
          ReservedT -> pure ()
          t -> throwError $ show $ "Missing record field" <+> pretty fn <+> "of type" <+> pretty t
      | (fn, t) <- M.toList $ m2 M.\\ m1
      ]
    -- Check existing fields
    sequence_ [ memo t1 t2 | (_fn, (t1, t2)) <- M.toList $ M.intersectionWith (,) m1 m2 ]

-- Variants
go (VariantT fs1) (VariantT fs2) = do
    let m1 = M.fromList fs1
    let m2 = M.fromList fs2
    sequence_
      [ case M.lookup fn m2 of
            Just t2 -> memo t1 t2
            Nothing -> throwError $ show $ "Missing variant field" <+> pretty fn <+> "of type" <+> pretty t1
      | (fn, t1) <- M.toList m1
      ]

-- Reference types
go (FuncT mt1) (FuncT mt2) = goMethodType mt1 mt2
go (ServiceT meths1) (ServiceT meths2) = do
    let m1 = M.fromList meths1
    forM_ meths2 $ \(m, mt2) -> case M.lookup m m1 of
        Just mt1 -> goMethodType mt1 mt2
        Nothing -> throwError $ show $ "Missing service method" <+> pretty m <+> "of type" <+> pretty mt2

-- BlobT
go BlobT BlobT = pure ()
go (VecT t) BlobT | isNat8 t = pure ()
go BlobT (VecT t) | isNat8 t = pure ()

-- Final catch-all
go t1 t2 = throwError $ show $ "Type" <+> pretty t1 <+> "is not a subtype of" <+> pretty t2

goMethodType ::
    (Pretty k2, Pretty k1, Ord k2, Ord k1) =>
    MethodType (Ref k1 Type) ->
    MethodType (Ref k2 Type) ->
    SubTypeM k1 k2 ()
goMethodType (MethodType ta1 tr1 q1 o1) (MethodType ta2 tr2 q2 o2) = do
    unless (q1 == q2) $ throwError "Methods differ in query annotation"
    unless (o1 == o2) $ throwError "Methods differ in oneway annotation"
    flipM $ goSeq ta2 ta1
    goSeq tr1 tr2

goSeq _ []  = pure ()
goSeq ts1 (RefT (Ref _ t) : ts) = goSeq ts1 (t:ts)
-- Missing optional arguments are ok
goSeq ts1@[] (OptT _ : ts) = goSeq ts1 ts
goSeq ts1@[] (ReservedT : ts) = goSeq ts1 ts
goSeq [] ts = throwError $ show $ "Argument type list too short, expecting types" <+> pretty ts
goSeq (t1:ts1) (t2:ts2) = memo t1 t2 >> goSeq ts1 ts2

unRef :: Type (Ref a Type) -> Type (Ref a Type)
unRef (RefT (Ref _ t)) = unRef t
unRef t = t

isNat8 :: Type (Ref a Type) -> Bool
isNat8 (RefT (Ref _ t)) = isNat8 t
isNat8 Nat8T = True
isNat8 _ = False
