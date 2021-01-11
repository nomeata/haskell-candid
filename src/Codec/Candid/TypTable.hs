{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-orphans #-}
module Codec.Candid.TypTable where

import qualified Data.Map as M
import Control.Monad.State.Lazy
import Data.Void
import Data.Text.Prettyprint.Doc
import Data.DList (singleton, DList)
import Data.Graph
import Data.Foldable

import Codec.Candid.Types

data SeqDesc where
    SeqDesc :: forall k. (Pretty k, Ord k) => M.Map k (Type k) -> [Type k] -> SeqDesc

instance Pretty SeqDesc where
    pretty (SeqDesc m ts) = pretty (M.toList m, ts)

data Ref k f  = Ref k (f (Ref k f))

buildSeqDesc :: forall k. (Pretty k, Ord k) => [Type (Ref k Type)] -> SeqDesc
buildSeqDesc ts = SeqDesc m ts'
  where
    (ts', m) = runState (mapM (mapM go) ts) mempty

    go :: Ref k Type -> State (M.Map k (Type k)) k
    go (Ref k t) = do
        seen <- gets (M.member k)
        unless seen $ mdo
            modify (M.insert k t')
            t' <- mapM go t
            return ()
        return k

-- This resolves PreServiceT to ServiceT
resolvePreServiceT :: SeqDesc -> Either String SeqDesc
resolvePreServiceT (SeqDesc m ts) = do
    m' <- mapM go m
    ts' <- mapM go ts
    return $ SeqDesc m' ts'
  where
    go (PreServiceT ms) = ServiceT <$> mapM goMethod ms
    -- No need to recurse; this will only show up top-level.
    go t = pure t

    goMethod (n, i) = case m M.! i of
        FuncT as bs -> return $ DidMethod n as bs
        _ -> Left "Method type not a function type"

voidEmptyTypes :: SeqDesc -> SeqDesc
voidEmptyTypes (SeqDesc m ts) = SeqDesc m' ts
  where
    edges = [ (k,k, toList (underRec t)) | (k,t) <- M.toList m ]
    sccs = stronglyConnComp edges
    bad = concat [ xs | CyclicSCC xs <- sccs ]
    m' = foldl' (\m k -> M.insert k EmptyT m) m bad


underRec :: Type k -> DList k
underRec (RefT x) = singleton x
underRec (RecT fs) = foldMap (underRec . snd) fs
underRec _ = mempty

tieKnot :: SeqDesc -> [Type Void]
tieKnot (SeqDesc m (ts :: [Type k])) = ts'
  where
    f :: k -> Type Void
    f k = m' M.! k
    m' :: M.Map k (Type Void)
    m' = (>>= f) <$> m
    ts' :: [Type Void]
    ts' = (>>= f) <$> ts

