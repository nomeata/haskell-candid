{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
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
import qualified GHC.Generics as G
import Data.Typeable

import Codec.Candid.Class

-- | This newtype encodes a Haskell record type using generic programming. Best used with @DerivingVia@, as shown in the overview.
newtype AsRecord a = AsRecord { unAsRecord :: a }


type CanBeCandidRecord a =
    ( Typeable a
    , Candid (R.Rec (NativeRowR a))
    , R.ToNativeExact a (NativeRowR a)
    , R.FromNative a (NativeRowR a)
    )
instance CanBeCandidRecord a => Candid (AsRecord a) where
    type AsCandid (AsRecord a) = AsCandid (R.Rec (NativeRowR a))
    toCandid = toCandid @(R.Rec (NativeRowR a)) . R.fromNative . unAsRecord
    fromCandid = AsRecord . R.toNativeExact . fromCandid @(R.Rec (NativeRowR a))

-- | This newtype encodes a Haskell data type as a variant using generic programming. Best used with @DerivingVia@, as shown in the overview.
newtype AsVariant a = AsVariant { unAsVariant :: a }

type CanBeCandidVariant a =
    ( Typeable a
    , Candid (V.Var (NativeRowV a))
    , V.ToNative a (NativeRowV a)
    , V.FromNativeExact a (NativeRowV a)
    )

instance CanBeCandidVariant a => Candid (AsVariant a) where
    type AsCandid (AsVariant a) = AsCandid (V.Var (NativeRowV a))
    toCandid = toCandid @(V.Var (NativeRowV a)) . V.fromNativeExact . unAsVariant
    fromCandid = AsVariant . V.toNative . fromCandid @(V.Var (NativeRowV a))

-- Extracted from https://github.com/target/row-types/pull/50/files#diff-28283e767b45c83675d1beb91ac71bb4R583
type family NativeRowR t where
  NativeRowR t = NativeRowRG (G.Rep t)

type family NativeRowRG t where
  NativeRowRG (G.M1 G.D m cs) = NativeRowRG cs
  NativeRowRG (G.M1 G.C m cs) = NativeRowRG cs
  NativeRowRG G.U1 = R.Empty
  NativeRowRG (l G.:*: r) = NativeRowRG l R..+ NativeRowRG r
  NativeRowRG (G.M1 G.S ('G.MetaSel ('Just name) p s l) (G.Rec0 t)) = name R..== t



type family NativeRowV t where
  NativeRowV t = NativeRowVG (G.Rep t)

type family NativeRowVG t where
  NativeRowVG (G.M1 G.D m cs) = NativeRowVG cs
  NativeRowVG G.V1 = V.Empty
  NativeRowVG (l G.:+: r) = NativeRowVG l V..+ NativeRowVG r
  NativeRowVG (G.C1 ('G.MetaCons name fixity sels) (G.S1 m (G.Rec0 t))) = name V..== t
