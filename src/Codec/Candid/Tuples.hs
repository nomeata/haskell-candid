{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Candid.Tuples (Unary(..), Tuplable, AsTuple, asTuple, fromTuple ) where

import Data.Type.Bool

-- | A newtype to stand in for the unary tuple
newtype Unary a = Unary {unUnary :: a} deriving (Eq, Show)

type family IsTuple a :: Bool where
    IsTuple () = 'True
    IsTuple (Unary t) = 'True
    IsTuple (t1,t2) = 'True
    IsTuple (t1,t2,t3) = 'True
    IsTuple (t1,t2,t3,t4) = 'True
    IsTuple t = 'False

type AsTuple a = If (IsTuple a) a (Unary a)

class IsTuple a ~ b => AsTuple_ a b where
    asTuple :: a -> AsTuple a
    fromTuple :: AsTuple a -> a
instance IsTuple a ~ 'True => AsTuple_ a 'True where
    asTuple = id
    fromTuple = id
instance IsTuple a ~ 'False => AsTuple_ a 'False where
    asTuple = Unary
    fromTuple = unUnary

type Tuplable a = (AsTuple_ a (IsTuple a))
