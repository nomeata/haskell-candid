{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Codec.Candid.ServiceT where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Functor.Identity 
import Data.Row
import Data.Row.Records
import Data.Row.Internal
import GHC.TypeLits

import Codec.Candid.Core

type ServiceT = Row MethType
type MethType = ([Type], [Type])

type family Args (p :: MethType) where Args '(x,y) = x
type family Res  (p :: MethType) where Res '(x,y) = y

-- | A raw service, operating on bytes
type RawServiceT m = T.Text -> BS.ByteString -> m BS.ByteString
type RawMethod m = BS.ByteString -> m BS.ByteString

type family ToArgs (ts :: [Type]) where
    ToArgs '[] = ()
    ToArgs '[t] = Val t
    ToArgs '[t1,t2] = (Val t1, Val t2)
    ToArgs '[t1,t2,t3] = (Val t1, Val t2, Val t3)
    ToArgs _ = TypeError ('Text "Too many arguments for ToArgs to handle")

-- type family ToMeth m (p :: MethType) where
--     ToMeth m '(x, y) = Seq x -> m (Seq y)
newtype ToMeth m (p :: MethType) = ToMeth { asMeth :: Seq (Args p) -> m (Seq (Res p)) }

type Impl m (i :: ServiceT) = Rec (Map (ToMeth m) i)
{-
type family Impl m (i :: Service) where
    Impl m ('R r) = 'R (ImplR m r)

type family ImplR m i where
    ImplR m '[] = '[]
    ImplR m (n ':-> x ': xs) = ( n ':-> ToMeth m x ': ImplR m xs)
-}


class CandidMethodT (m :: * -> *) f where
  fromMeth :: (forall a. String -> m a) -> ToMeth m f -> RawMethod m
  toMeth :: (forall a. String -> m a) -> RawMethod m -> ToMeth m f

instance (KnownArgs ts1, KnownArgs ts2, Monad m) => CandidMethodT m '(ts1, ts2) where
  fromMeth onErr (ToMeth m) b = case decodeT @ts1 b of
    Left err -> onErr err
    Right x -> encodeT @ts2 <$> m x

  toMeth onErr f = ToMeth $ \x -> do
    b <- f (encodeT @ts1 x)
    case decodeT @ts2 b of
      Left err -> onErr err
      Right y -> return y

-- | A Candid service. The @i@ describes the interface of the service
type CandidServiceT m i = (Forall i (CandidMethodT m), AllUniqueLabels i)

-- | Turns a raw service (function operating on bytes) into a typed Candid service (a record of typed methods). The raw service is typically code that talks over the network.
toCandidServiceT ::
  forall m i.
  CandidServiceT m i =>
   -- | What to do if the raw service returns unparsable data
  (forall a. String -> m a) ->
  RawServiceT m ->
  Impl m i
toCandidServiceT onErr f =
    runIdentity $
    fromLabelsMapA @(CandidMethodT m) @Identity @(ToMeth m) @i $ \l ->
    pure $ toMeth onErr (f (toKey l))

{-

-- | Turns a typed candid service into a raw service. Typically used in a framework warpping Candid services.
fromCandidServiceT ::
  forall m i.
  CandidServiceT m i =>
  -- | What to do if the method name does not exist
  (forall a. T.Text -> m a) ->
  -- | What to do when the caller provides unparsable data
  (forall a. String -> m a) ->
  Impl m i ->
  RawService m
fromCandidServiceT notFound onErr r =
    \meth a -> case H.lookup meth m of
      Just f -> f a
      Nothing -> notFound meth
  where
    m :: H.HashMap T.Text (RawMethod m)
    m = eraseToHashMap @(CandidMethodT m) (fromMeth onErr) r

-}
