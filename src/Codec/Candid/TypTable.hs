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

tieKnot :: SeqDesc -> [Type Void]
tieKnot (SeqDesc m (ts :: [Type k])) = ts'
  where
    f :: k -> Type Void
    f k = m' M.! k
    m' :: M.Map k (Type Void)
    m' = (>>= f) <$> m
    ts' :: [Type Void]
    ts' = (>>= f) <$> ts

