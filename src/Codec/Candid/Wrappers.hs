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
