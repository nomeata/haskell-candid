{-# LANGUAGE StandaloneDeriving #-}
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
{-# LANGUAGE UndecidableInstances #-} -- the reason for this isolation
{-# OPTIONS_GHC -Wno-orphans #-}
module Codec.Candid.Row (AsRecord(..)) where

import qualified Data.Row as R
import qualified Data.Row.Records as R
import qualified GHC.Generics as G
import Data.Typeable

import Codec.Candid

newtype AsRecord a = AsRecord { unAsRecord :: a }

instance
    ( Typeable a
    , Candid (R.Rec (NativeRow a))
    , R.ToNativeExact a (NativeRow a)
    , R.FromNative a (NativeRow a)
    ) => Candid (AsRecord a) where
    type Rep (AsRecord a) = Rep (R.Rec (NativeRow a))
    toCandid = toCandid @(R.Rec (NativeRow a)) . R.fromNative . unAsRecord
    fromCandid = AsRecord . R.toNativeExact . fromCandid @(R.Rec (NativeRow a))

-- Extracted from https://github.com/target/row-types/pull/50/files#diff-28283e767b45c83675d1beb91ac71bb4R583
type family NativeRow t where
  NativeRow t = NativeRowG (G.Rep t)

type family NativeRowG t where
  NativeRowG (G.M1 G.D m cs) = NativeRowG cs
  NativeRowG (G.M1 G.C m cs) = NativeRowG cs
  NativeRowG G.U1 = R.Empty
  NativeRowG (l G.:*: r) = NativeRowG l R..+ NativeRowG r
  NativeRowG (G.M1 G.S ('G.MetaSel ('Just name) p s l) (G.Rec0 t)) = name R..== t

