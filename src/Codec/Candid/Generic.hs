{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- the reason for this being in its own module
{-# OPTIONS_GHC -Wno-orphans #-}
module Codec.Candid.Generic (AsRecord(..), AsVariant(..)) where

import qualified Data.Row as R
import qualified Data.Row.Records as R
import qualified Data.Row.Variants as V
import Data.Typeable

import Codec.Candid.Class

-- | This newtype encodes a Haskell record type using generic programming. Best used with @DerivingVia@, as shown in the tutorial.
newtype AsRecord a = AsRecord { unAsRecord :: a }


type CanBeCandidRecord a =
    ( Typeable a
    , Candid (R.Rec (R.NativeRow a))
    , R.ToNative a
    , R.FromNative a
    )
instance CanBeCandidRecord a => Candid (AsRecord a) where
    type AsCandid (AsRecord a) = AsCandid (R.Rec (R.NativeRow a))
    toCandid = toCandid @(R.Rec (R.NativeRow a)) . R.fromNative . unAsRecord
    fromCandid = AsRecord . R.toNative . fromCandid @(R.Rec (R.NativeRow a))

-- | This newtype encodes a Haskell data type as a variant using generic programming. Best used with @DerivingVia@, as shown in the tutorial.
newtype AsVariant a = AsVariant { unAsVariant :: a }

type CanBeCandidVariant a =
    ( Typeable a
    , Candid (V.Var (V.NativeRow a))
    , V.ToNative a
    , V.FromNative a
    )

instance CanBeCandidVariant a => Candid (AsVariant a) where
    type AsCandid (AsVariant a) = AsCandid (V.Var (V.NativeRow a))
    toCandid = toCandid @(V.Var (V.NativeRow a)) . V.fromNative . unAsVariant
    fromCandid = AsVariant . V.toNative . fromCandid @(V.Var (V.NativeRow a))
