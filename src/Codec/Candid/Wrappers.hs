{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- the reason for this isolation
module Codec.Candid.Wrappers where

import Data.Typeable
import Codec.Candid.Core

newtype CandidVal (t :: Type) where CandidVal :: Val t -> CandidVal t
deriving instance Eq (Val t) => Eq (CandidVal t)
deriving instance Show (Val t) => Show (CandidVal t)
instance (Typeable t, KnownType t) => Candid (CandidVal t) where
    type Rep (CandidVal t) = t
    toCandid (CandidVal x) = x
    fromCandid = CandidVal

newtype CandidSeq (t :: [Type]) where CandidSeq :: Seq t -> CandidSeq t
deriving instance Eq (Seq t) => Eq (CandidSeq t)
deriving instance Show (Seq t) => Show (CandidSeq t)
instance KnownArgs t => CandidArgs (CandidSeq t) where
    type ArgRep (CandidSeq t) = t
    toSeq (CandidSeq x) = x
    fromSeq = CandidSeq

