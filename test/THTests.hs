{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

module THTests (thTests) where

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Data.Row

import Codec.Candid

thTests :: TestTree
thTests = testGroup "Using TH interface"
  [ testCase "demo1: direct" $ do
      x <- greet1 .! #greet $ "World"
      x @?= "Hello World"
  , testCase "demo1: via toCandidService" $ do
      x <- greet2 .! #greet $ "World"
      x @?= "World"
  , testCase "demo2" $ do
      x <- demo2 .! #greet $ ("World", True)
      x @?= "WorldTrue"
  , testCase "demo3" $ do
      x <- demo3 .! #greet $ ("World", True)
      x @?= "WorldTrue"
  ]

-- NB: Fields in the wrong order
type Demo1 m = [candid|service : { "greet": (text) -> (text); "a" : () -> () } |]

greet1 :: Monad m => Rec (Demo1 m)
greet1 = #a .== (\() -> return ()) .+ #greet .== (\who -> return $ "Hello " <> who)

greet2 :: forall m. Monad m => Rec (Demo1 m)
greet2 = toCandidService error (\_ x -> return x)

type Demo2 m = [candid| service : { "greet": (text, bool) -> (text); } |]

demo2 :: Monad m => Rec (Demo2 m)
demo2 = #greet .== \(who, b) -> return $ who <> T.pack (show b)

-- NB type definitions:
type Demo3 m = [candid| type t = text; service : { "greet": (t, bool) -> (t); } |]

demo3 :: Monad m => Rec (Demo3 m)
demo3 = demo2

