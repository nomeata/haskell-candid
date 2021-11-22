{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module THTests (thTests) where

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Data.Row
import Type.Reflection

import Codec.Candid

thTests :: TestTree
thTests = testGroup "TH"
  [ testGroup "Candid services"
    [ testCase "demo1: direct" $ do
        x <- greet1 .! #greet $ "World"
        x @?= "Hello World"
    , testCase "demo1: via toCandidService" $ do
        x <- greet2 .! #greet $ "World"
        x @?= "World"
    , testCase "demo2: n-ary arguments" $ do
        x <- demo2 .! #greet $ ("World", True)
        x @?= "WorldTrue"
    , testCase "demo3: type definitions" $ do
        x <- demo3 .! #greet $ ("World", True)
        x @?= "WorldTrue"
    , testCase "demo4: tuple shorthands" $ do
        x <- demo4 .! #greet $ Unary (1,True, empty)
        x @?= Unary (#_0_ .== 2,False)
    ]
  , testGroup "Candid tye definitions"
    [ testCase "with service" $ do
      typeRep @(Defs1 .! "t") @?= typeRep @T.Text
    , testCase "with anonymous service" $ do
      typeRep @(Defs2 .! "t") @?= typeRep @T.Text
    ]

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

-- NB tuples:
type Demo4 m = [candid|service : { "greet": (record {int; bool; record {}}) -> (record {0 : record{int}; 1 : bool}); } |]

demo4 :: Monad m => Rec (Demo4 m)
demo4 = #greet .== \(Unary (i,b, _)) -> return $ Unary (#_0_ .== (i + 1), not b)

type Defs1 = [candidDefs|type t = text; service foo : {} |]
type Defs2 = [candidDefs|type t = text; service : {} |]
