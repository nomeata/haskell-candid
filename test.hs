{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V hiding (singleton)
import qualified Data.HashMap.Lazy as HM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Data.Void
import Data.Either
import GHC.Int
import GHC.Word
import Numeric.Natural
import Control.Monad
import GHC.TypeLits
import GHC.Generics (Generic)
import Data.Text.Prettyprint.Doc
import Data.Row
import qualified Data.Row.Records as R
import qualified Data.Row.Variants as V

import Codec.Candid

main = defaultMain tests

newtype Peano = Peano (Maybe Peano)
    deriving (Show, Eq)
    deriving Candid via (Maybe Peano)

peano :: Peano
peano = Peano $ Just $ Peano $ Just $ Peano $ Just $ Peano Nothing

newtype LinkedList a = LinkedList (Maybe (a, LinkedList a))
    deriving (Show, Eq)
    deriving newtype Candid

cons x y = LinkedList $ Just (x, y)
nil = LinkedList Nothing

natList :: LinkedList Natural
natList = cons 1 (cons 2 (cons 3 (cons 4 nil)))

stringList :: [T.Text]
stringList = [T.pack "HI", T.pack "Ho"]

newtype ARecord a = ARecord { foo :: a }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serial m)

deriving via (AsRecord (ARecord a))
    instance Candid a => Candid (ARecord a)

data EmptyRecord = EmptyRecord
    deriving (Eq, Show, Generic, Serial m)
    deriving Candid via (AsRecord EmptyRecord)

newtype MiddleField a = MiddleField a
    deriving (Eq, Show)

instance Candid a => Candid (MiddleField a) where
    asType = asType @(Rec ("_1_" .== a))
    toCandid (MiddleField x) = toCandid (#_1_ .== x)
    fromCandid x = (\r -> MiddleField (r .! #_1_)) <$> fromCandid @(Rec ("_1_" .== a)) x

newtype JustRight a = JustRight a
    deriving (Eq, Show)

instance Candid a => Candid (JustRight a) where
    asType = asType @(Var ("Right" .== a))
    toCandid (JustRight x) = toCandid (V.singleton (Label @"Right") x)
    fromCandid x = JustRight . snd . V.unSingleton <$> fromCandid @(Var ("Right" .== a)) x

data SimpleRecord = SimpleRecord { foo :: Bool, bar :: T.Text }
    deriving (Generic, Eq, Show)
    deriving (Serial m)
    deriving Candid via (AsRecord SimpleRecord)

roundTripTest :: forall a. (CandidArg a, Eq a, Show a) => a -> Assertion
roundTripTest v1 = do
  let bytes1 = encode v1
  v2 <- case decode @a bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  assertEqual "values" v1 v2

roundTripProp :: forall a. (CandidArg a, Serial IO a, Show a, Eq a) => TestTree
roundTripProp = testProperty desc $ \v ->
    case decode @a (encode @a v) of
        Right y | y == v -> Right ("all good" :: String)
        Right y -> Left $
            show v ++ " round-tripped to " ++ show y
        Left err -> Left $
            show v ++ " failed to decode: " ++ err
  where
    desc = show $ pretty (tieKnot (typeDesc @a))

subTypProp :: forall a b.  (CandidArg a, Serial IO a, Show a, CandidArg b) => TestTree
subTypProp = testProperty desc $ \v ->
    isRight $ decode @b (encode @a v)
  where
    desc = show $ pretty (tieKnot (typeDesc @a)) <+> "<:" <+> pretty (tieKnot (typeDesc @b))

subTypeTest' :: forall a b.
    (CandidArg a, Eq a, Show a) =>
    (CandidArg b, Eq b, Show b) =>
    a -> b -> Assertion
subTypeTest' v1 v2 = do
  let bytes1 = encode v1
  v2' <- case decode @b bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  v2 @=? v2'

subTypeTest :: forall a b.
    (CandidArg a, Eq a, Show a) =>
    (CandidArg b, Eq b, Show b) =>
    a -> b -> Assertion
subTypeTest v1 v2 = do
  subTypeTest' v1 v2
  -- now try the other direction
  let bytes2 = encode v2
  case decode @a bytes2 of
    Left err -> return ()
    Right _ -> assertFailure "converse subtype test succeeded"

instance Monad m => Serial m T.Text where
    series = T.pack <$> series

instance (Monad m, Serial m a) => Serial m (V.Vector a) where
    series = V.fromList <$> series

instance Monad m => Serial m Void where
    series = mzero

parseTest :: String -> DidFile -> TestTree
parseTest c e = testCase c $
    case parseDid c of
        Left err -> assertFailure err
        Right s -> s @?= e

tests = testGroup "tests"
  [ testGroup "encode tests"
    [ testCase "empty" $ encode () @?= B.pack "DIDL\0\0"
    , testCase "bool" $ encode (Unary True) @?= B.pack "DIDL\0\1\x7e\1"
    ]
  , testGroup "roundtrip"
    [ testCase "empty" $ roundTripTest ()
    , testCase "bool" $ roundTripTest $ Unary True
    , testCase "simple record 1" $ roundTripTest (ARecord True, False)
    , testCase "simple record 2" $ roundTripTest (ARecord (100000 :: Natural), False)
    , testCase "simple variant 1" $ roundTripTest $ Unary (Left True :: Either Bool Bool)
    , testCase "simple variant 2" $ roundTripTest $ Unary (Right False :: Either Bool Bool)
    , testCase "nested record 2" $ roundTripTest (ARecord (True,False), False)
    , testCase "peano" $ roundTripTest $ Unary peano
    , testCase "lists" $ roundTripTest (natList, stringList)
    , testCase "custom record" $ roundTripTest $ Unary (SimpleRecord True "Test")
    ]
  , testGroup "subtypes"
    [ testCase "nat/int" $ subTypeTest (Unary (42 :: Natural)) (Unary (42 :: Integer))
    , testCase "null/opt" $ subTypeTest (Unary ()) (Unary (Nothing @Integer))
    , testCase "rec" $ subTypeTest (ARecord True, True) (EmptyRecord, True)
    , testCase "tuple" $ subTypeTest ((42::Integer,-42::Integer), 100::Integer) (EmptyRecord, 100::Integer)
    , testCase "variant" $ subTypeTest' (JustRight (42 :: Natural), True) (Right 42 :: Either Bool Natural, True)
    , testCase "rec/any" $ subTypeTest (ARecord True, True) (Reserved, True)
    , testCase "tuple/any" $ subTypeTest ((42::Integer, 42::Natural), True) (Reserved, True)
    , testCase "tuple/tuple" $ subTypeTest ((42::Integer,-42::Integer,True), 100::Integer) ((42::Integer, -42::Integer), 100::Integer)
    , testCase "tuple/middle" $ subTypeTest ((42::Integer,-42::Integer,True), 100::Integer) (MiddleField (-42) :: MiddleField Integer, 100::Integer)
    , testCase "records" $ subTypeTest (Unary (SimpleRecord True "Test")) (Unary (ARecord True))
    ]
  , testGroup "roundtrip smallchecks"
    [ roundTripProp @Bool
    , roundTripProp @Natural
    , roundTripProp @Word8
    , roundTripProp @Word16
    , roundTripProp @Word32
    , roundTripProp @Word64
    , roundTripProp @Integer
    , roundTripProp @Int8
    , roundTripProp @Int16
    , roundTripProp @Int32
    , roundTripProp @Int64
    , roundTripProp @Float
    , roundTripProp @Double
    , roundTripProp @T.Text
    , roundTripProp @()
    , roundTripProp @Reserved
    , roundTripProp @Principal
    , roundTripProp @BS.ByteString
    , roundTripProp @(Maybe T.Text)
    , roundTripProp @(V.Vector T.Text)
    , roundTripProp @EmptyRecord
    , roundTripProp @(ARecord T.Text)
    , roundTripProp @(Either Bool T.Text)
    , roundTripProp @SimpleRecord
    ]
  , testGroup "subtype smallchecks"
    [ subTypProp @Natural @Natural
    , subTypProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8)) @Reserved
    , subTypProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8)) @(Rec ("Hi" .== Reserved))
    , subTypProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8)) @(Rec ("Hi" .== Word8))
    , subTypProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8)) @(Rec ("_1_" .== Word8))
    , subTypProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8 .+ "_2_" .== Bool)) @(Rec ("_1_" .== Word8))
    , subTypProp @(Maybe (Rec ("Hi" .== Word8 .+ "_1_" .== Word8 .+ "_0_" .== Bool))) @(Maybe (Bool,Word8))
    , subTypProp @(Var ("Hi" .== Word8)) @(Var ("Hi" .== Word8 .+ "Ho" .== T.Text))
    , subTypProp @(Var ("Ho" .== T.Text)) @(Var ("Hi" .== Word8 .+ "Ho" .== T.Text))
    , subTypProp @Natural @Reserved
    , subTypProp @BS.ByteString @Reserved
    , subTypProp @BS.ByteString @(V.Vector Word8)
    , subTypProp @(V.Vector Word8) @BS.ByteString
    , subTypProp @Principal @Reserved
    ]
  , testGroup "candid parsing" $
    let m x y z = (x, y, z) in
    [ parseTest "service : {}" []
    , parseTest "service : { foo : (text) -> (text) }"
        [ m "foo" [TextT] [TextT] ]
    , parseTest "service : { foo : (text,) -> (text,); }"
        [ m "foo" [TextT] [TextT] ]
    , parseTest "service : { foo : (opt text) -> () }"
        [ m "foo" [OptT TextT] []  ]
    , parseTest "service : { foo : (record { x_ : null; 5 : nat8 }) -> () }"
        [ m "foo" [RecT [(N "x__", NullT), (N "_5_", Nat8T)]] [] ]
    , parseTest "service : { foo : (record { x : null; 5 : nat8 }) -> () }"
        [ m "foo" [RecT [(N "x", NullT), (N "_5_", Nat8T)]] [] ]
    ]
  , testGroup "Using TH interface" $
    [ testCase "demo1: direct" $ do
        x <- greet1 .! #greet $ "World"
        x @?= "Hello World"
    , testCase "demo1: via toCandidService" $ do
        x <- greet2 .! #greet $ "World"
        x @?= "World"
    , testCase "demo2" $ do
        x <- demo2 .! #greet $ ("World", True)
        x @?= "WorldTrue"
    ]
  ]

instance Monad m => Serial m BS.ByteString where
    series = BS.pack <$> series

instance Monad m => Serial m Principal where
    series = Principal <$> series

instance Monad m => Serial m Reserved where
    series = Reserved <$ series @m @()

instance (Monad m, Forall r (Serial m), AllUniqueLabels r) => Serial m (Rec r) where
    series = R.fromLabelsA @(Serial m) (\l -> series)

instance (Monad m, Forall r (Serial m), AllUniqueLabels r) => Serial m (Var r) where
    series = V.fromLabels @(Serial m) (\l -> series)

type Demo1 m = [candid| service : { "greet": (text) -> (text); } |]

greet1 :: Monad m => Rec (Demo1 m)
greet1 = #greet .== \who -> return $ "Hello " <> who

greet2 :: forall m. Monad m => Rec (Demo1 m)
greet2 = toCandidService error (\_ x -> return x)

type Demo2 m = [candid| service : { "greet": (text, bool) -> (text); } |]

demo2 :: Monad m => Rec (Demo2 m)
demo2 = #greet .== \(who, b) -> return $ who <> T.pack (show b)
