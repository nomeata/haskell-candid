{-# LANGUAGE TypeApplications #-}
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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
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

import Codec.Candid

main = defaultMain tests

newtype Peano = Peano (Maybe Peano) deriving (Show, Eq)
type PeanoT = OtherT Peano

instance Candid Peano where
    type Rep Peano = 'OptT PeanoT
    toCandid (Peano x) = x
    fromCandid = Peano

peano :: Val PeanoT
peano = Peano $ Just $ Peano $ Just $ Peano $ Just $ Peano Nothing

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq, Show)
instance Candid a => Candid (LinkedList a) where
    type Rep (LinkedList a) = 'OptT ('RecT '[ '( 'H 0, Rep a), '( 'H 1, OtherT (LinkedList a))])
    toCandid Nil = Nothing
    toCandid (Cons x xs) = Just (toCandid x, (xs, ()))
    fromCandid Nothing = Nil
    fromCandid (Just (x,(xs, ()))) = Cons (fromCandid x) xs

natList :: LinkedList Natural
natList = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

stringList :: [T.Text]
stringList = [T.pack "HI", T.pack "Ho"]

newtype ARecord a = ARecord { foo :: a }
    deriving (Eq, Show)

instance Candid a => Candid (ARecord a) where
    type Rep (ARecord a) = 'RecT '[ '( 'N "foo", Rep a) ]
    toCandid (ARecord x) = (toCandid x, ())
    fromCandid (x, ()) = ARecord (fromCandid x)

newtype SingleField (n::Nat) a = SingleField a
    deriving (Eq, Show)

instance (KnownNat n, Candid a) => Candid (SingleField n a) where
    type Rep (SingleField n a) = 'RecT '[ '( 'H n, Rep a) ]
    toCandid (SingleField x) = (toCandid x, ())
    fromCandid (x, ()) = SingleField (fromCandid x)

newtype JustRight a = JustRight a
    deriving (Eq, Show)

instance Candid a => Candid (JustRight a) where
    type Rep (JustRight a) = 'VariantT '[ '( 'N "Right", Rep a) ]
    toCandid (JustRight x) = Left (toCandid x)
    fromCandid (Left x) = JustRight (fromCandid x)

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

roundTripProp :: forall ts. (KnownArgs ts, Serial IO (Seq ts), Eq (Seq ts), Show (Seq ts)) => TestTree
roundTripProp = testProperty (show (pretty (typesVal @ts))) $ \v ->
    case decodeT @ts (encodeT @ts v) of
        Right y | y == v -> Right ("all good" :: String)
        Right y -> Left $
            show v ++ " round-tripped to " ++ show y
        Left err -> Left $
            show v ++ " failed to decode: " ++ err

subTypProp :: forall ts1 ts2.
    (KnownArgs ts1, Serial IO (Seq ts1), Show (Seq ts1)) =>
    KnownArgs ts2 =>
    TestTree
subTypProp = testProperty (show (pretty (typesVal @ts1) <+> "<:" <+> pretty (typesVal @ts2))) $ \v ->
    isRight $ decodeT @ts2 (encodeT @ts1 v)

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

nullV :: CandidVal 'NullT
nullV = CandidVal ()

reservedV :: CandidVal 'ReservedT
reservedV = CandidVal ()

emptyRec :: CandidVal ('RecT '[])
emptyRec = CandidVal ()

instance Monad m => Serial m T.Text where
    series = T.pack <$> series

instance (Monad m, Serial m a) => Serial m (V.Vector a) where
    series = V.fromList <$> series

instance Monad m => Serial m Void where
    series = mzero

parseTest :: String -> DidFile -> TestTree
parseTest c e = testCase c $
    -- We don't have Eq on Type, so lets pretty-print
    case parseDid c of
        Left err -> assertFailure err
        Right s -> show (pretty s) @?= show (pretty e)

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
    , testCase "null/opt" $ subTypeTest (Unary nullV) (Unary (Nothing @Integer))
    , testCase "rec" $ subTypeTest (ARecord True, True) (emptyRec, True)
    , testCase "tuple" $ subTypeTest ((42::Integer,-42::Integer), 100::Integer) (emptyRec, 100::Integer)
    , testCase "variant" $ subTypeTest' (Right 42 :: Either Bool Natural, True) (JustRight (42 :: Natural), True)
    , testCase "rec/any" $ subTypeTest (ARecord True, True) (reservedV, True)
    , testCase "tuple/any" $ subTypeTest ((42::Integer, 42::Natural), True) (reservedV, True)
    , testCase "tuple/tuple" $ subTypeTest ((42::Integer,-42::Integer,True), 100::Integer) ((42::Integer, -42::Integer), 100::Integer)
    , testCase "tuple/middle" $ subTypeTest ((42::Integer,-42::Integer,True), 100::Integer) (SingleField (-42) :: SingleField 1 Integer, 100::Integer)
    , testCase "records" $ subTypeTest (Unary (SimpleRecord True "Test")) (Unary (ARecord True))
    ]
  , testGroup "roundtrip smallchecks"
    [ roundTripProp @ '[ 'BoolT]
    , roundTripProp @ '[ 'NatT]
    , roundTripProp @ '[ 'Nat8T]
    , roundTripProp @ '[ 'Nat16T]
    , roundTripProp @ '[ 'Nat32T]
    , roundTripProp @ '[ 'Nat64T]
    , roundTripProp @ '[ 'IntT]
    , roundTripProp @ '[ 'Int8T]
    , roundTripProp @ '[ 'Int16T]
    , roundTripProp @ '[ 'Int32T]
    , roundTripProp @ '[ 'Int64T]
    , roundTripProp @ '[ 'Float32T]
    , roundTripProp @ '[ 'Float64T]
    , roundTripProp @ '[ 'TextT]
    , roundTripProp @ '[ 'NullT]
    , roundTripProp @ '[ 'ReservedT]
    , roundTripProp @ '[ 'PrincipalT]
    , roundTripProp @ '[ 'BlobT]
    , roundTripProp @ '[ 'OptT TextT]
    , roundTripProp @ '[ 'VecT TextT]
    , roundTripProp @ '[ 'RecT '[] ]
    , roundTripProp @ '[ 'RecT '[ '(N "Hi", Nat8T) ] ]
    , roundTripProp @ '[ 'RecT '[ '(N "Hi", Nat8T), '(N "Ho", Nat8T) ] ]
    , roundTripProp @ '[ 'RecT '[ '(N "Hi", Nat8T), '(H 1, Nat8T) ] ]
    , roundTripProp @ '[ 'VariantT '[ '(N "Hi", BoolT), '(N "Ho", BoolT) ] ]
    , roundTripProp @ '[ OtherT SimpleRecord ]
    ]
  , testGroup "subtype smallchecks"
    [ subTypProp @ '[ 'NatT ] @ '[ 'IntT ]
    , subTypProp @ '[ 'RecT '[ '(N "Hi", Nat8T), '(H 1, Nat8T) ] ] @ '[ 'ReservedT ]
    , subTypProp @ '[ 'RecT '[ '(N "Hi", Nat8T), '(H 1, Nat8T) ] ] @ '[ 'RecT '[]]
    , subTypProp @ '[ 'RecT '[ '(N "Hi", Nat8T), '(H 1, Nat8T) ] ] @ '[ 'RecT '[ '(H 1, Nat8T)]]
    , subTypProp @ '[ 'RecT '[ '(H 0, TextT), '(H 1, Nat8T), '(H 2, BoolT) ] ] @ '[ 'RecT '[ '(H 1, Nat8T)]]
    , subTypProp @ '[ 'VariantT '[ '(N "Hi", BoolT) ] ] @ '[ 'VariantT '[ '(N "Hi", BoolT), '(N "Ho", TextT) ] ]
    , subTypProp @ '[ 'VariantT '[ '(N "Ho", TextT) ] ] @ '[ 'VariantT '[ '(N "Hi", BoolT), '(N "Ho", TextT) ] ]
    , subTypProp @ '[ 'NatT ] @ '[ 'ReservedT ]
    , subTypProp @ '[ 'BlobT ] @ '[ 'ReservedT ]
    , subTypProp @ '[ 'PrincipalT ] @ '[ 'ReservedT ]
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
    , parseTest "service : { foo : (record { x : null; 5 : nat8 }) -> () }"
        [ m "foo" [RecT [(N' "x", NullT), (H' 5, Nat8T)]] [] ]
    ]
  , testGroup "Using TH interface" $
    [ testCase "direct" $ do
        x <- greet1 .! #greet $ "World"
        x @?= "Hello World"
    , testCase "raw" $ do
        x <- greet2 .! #greet $ "World"
        x @?= "World"
    ]
  ]

instance Monad m => Serial m BS.ByteString where
    series = BS.pack <$> series

type DemoInterface m = [candid| service : { "greet": (text) -> (text); } |]

greet1 :: Monad m => Rec (DemoInterface m)
greet1 = #greet .== \who -> return $ "Hello " <> who

greet2 :: forall m. Monad m => Rec (DemoInterface m)
greet2 = toCandidService error (\_ x -> return x)
