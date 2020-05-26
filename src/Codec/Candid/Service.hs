{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Codec.Candid.Service where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as BS
import Data.Row
import Data.Row.Records
import Data.Row.Internal

import Codec.Candid.Core

-- | A raw service, operating on bytes
type RawService m = T.Text -> BS.ByteString -> m BS.ByteString
type RawMethod m = BS.ByteString -> m BS.ByteString

class CandidMethod (m :: * -> *) f  | f -> m where
  type MethArg f
  type MethRes f
  fromMeth :: (forall a. String -> m a) -> f -> RawMethod m
  toMeth :: (forall a. String -> m a) -> RawMethod m -> f

instance (CandidArg a, CandidArg b, Monad m) => CandidMethod m (a -> m b) where
  type MethArg (a -> m b) = a
  type MethRes (a -> m b) = b
  fromMeth onErr m b = case decode b of
    Left err -> onErr err
    Right x -> encode <$> m x

  toMeth onErr f x = do
    b <- f (encode x)
    case decode b of
      Left err -> onErr err
      Right y -> return y

-- | A Candid service. The @r@ describes the type of a 'Rec'.
type CandidService m r = (Forall r (CandidMethod m), AllUniqueLabels r)

-- | Turns a raw service (function operating on bytes) into a typed Candid service (a record of typed methods). The raw service is typically code that talks over the network.
toCandidService ::
  forall m r.
  CandidService m r =>
   -- | What to do if the raw service returns unparsable data
  (forall a. String -> m a) ->
  RawService m ->
  Rec r
toCandidService onErr f = fromLabels @ (CandidMethod m) $ \l ->
  toMeth onErr (f (toKey l))

-- | Turns a typed candid service into a raw service. Typically used in a framework warpping Candid services.
fromCandidService ::
  forall m r.
  CandidService m r =>
  -- | What to do if the method name does not exist
  (forall a. T.Text -> m a) ->
  -- | What to do when the caller provides unparsable data
  (forall a. String -> m a) ->
  Rec r ->
  RawService m
fromCandidService notFound onErr r =
    \meth a -> case H.lookup meth m of
      Just f -> f a
      Nothing -> notFound meth
  where
    m :: H.HashMap T.Text (RawMethod m)
    m = eraseToHashMap @(CandidMethod m) (fromMeth onErr) r
