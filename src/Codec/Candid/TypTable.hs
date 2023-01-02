{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-orphans #-}
module Codec.Candid.TypTable where

import qualified Data.Map as M
import Control.Monad.State.Lazy
import Data.Void
import Prettyprinter
import Data.DList (singleton, DList)
import Data.Graph
import Data.Foldable
import Control.Monad

import Codec.Candid.Types

data SeqDesc where
    SeqDesc :: forall k. (Pretty k, Ord k) => M.Map k (Type k) -> [Type k] -> SeqDesc

instance Pretty SeqDesc where
    pretty (SeqDesc m ts) = pretty (M.toList m, ts)

data Ref k f  = Ref k (f (Ref k f))

instance Pretty k => Pretty (Ref k f) where
    pretty (Ref k _) = pretty k
instance Eq k => Eq (Ref k f) where
    (==) (Ref k1 _) (Ref k2 _) = (==) k1 k2
instance Ord k => Ord (Ref k f) where
    compare (Ref k1 _) (Ref k2 _) = compare k1 k2

unrollTypeTable :: SeqDesc -> (forall k. (Pretty k, Ord k) => [Type (Ref k Type)] -> r) -> r
unrollTypeTable (SeqDesc m t) k = k (unrollTypeTable' m t)

unrollTypeTable' :: forall k. Ord k => M.Map k (Type k) -> [Type k] -> [Type (Ref k Type)]
unrollTypeTable' m ts = ts'
  where
    f :: k -> Type (Ref k Type)
    f k = RefT (Ref k (m' M.! k))
    m' :: M.Map k (Type (Ref k Type))
    m' = (>>= f) <$> m
    ts' :: [Type (Ref k Type)]
    ts' = (>>= f) <$> ts

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

-- | This takes a type description and replaces all named types with their definition.
--
-- This can produce an infinite type! Only use this in sufficiently lazy contexts, or when the
-- type is known to be not recursive.
tieKnot :: SeqDesc -> [Type Void]
tieKnot (SeqDesc m (ts :: [Type k])) = ts'
  where
    f :: k -> Type Void
    f k = m' M.! k
    m' :: M.Map k (Type Void)
    m' = (>>= f) <$> m
    ts' :: [Type Void]
    ts' = (>>= f) <$> ts

