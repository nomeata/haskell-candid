{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
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

import Codec.Candid

main = defaultMain tests

newtype Peano = Peano (Maybe Peano) deriving (Show, Eq)
type PeanoT = 'OtherT ('Other Peano)

instance Candid Peano where
    type Rep Peano = 'OptT PeanoT
    toCandid (Peano x) = x
    fromCandid = Peano

peano :: Val PeanoT
peano = Peano $ Just $ Peano $ Just $ Peano $ Just $ Peano Nothing

instance Candid a => Candid [a] where
    type Rep [a] = 'OptT ('RecT '[ '( 'H 0, Rep a), '( 'H 1, 'OtherT ('Other [a]))])
    toCandid [] = Nothing
    toCandid (x:xs) = Just (toCandid x, (xs, ()))
    fromCandid Nothing = []
    fromCandid (Just (x,(xs, ()))) = fromCandid x : xs

natList :: [Natural]
natList = [1,2,3,4]

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

roundTripTest :: forall a. (CandidArgs a, Eq a, Show a) => a -> Assertion
roundTripTest v1 = do
  let bytes1 = encode v1
  v2 <- case decode @a bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  assertEqual "values" v1 v2

roundTripProp :: forall ts. (KnownArgs ts, Serial IO (Seq ts), Eq (Seq ts), Show (Seq ts)) => TestTree
roundTripProp = testProperty (show (pretty (types @ts))) $ \v ->
    let exp = CandidSeq @ts v in
    case decode @(CandidSeq ts) (encode exp) of
        Right y | y == exp -> Right ("all good" :: String)
        Right y -> Left $
            show exp ++ " round-tripped to " ++ show y
        Left err -> Left $
            show exp ++ " failed to decode: " ++ err

subTypProp :: forall ts1 ts2.
    (KnownArgs ts1, Serial IO (Seq ts1), Show (Seq ts1)) =>
    KnownArgs ts2 =>
    TestTree
subTypProp = testProperty (show (pretty (types @ts1) <+> "<:" <+> pretty (types @ts2))) $ \v ->
    isRight $ decode @(CandidSeq ts2) (encode (CandidSeq @ts1 v))

subTypeTest' :: forall a b.
    (CandidArgs a, Eq a, Show a) =>
    (CandidArgs b, Eq b, Show b) =>
    a -> b -> Assertion
subTypeTest' v1 v2 = do
  let bytes1 = encode v1
  v2' <- case decode @b bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  v2 @=? v2'

subTypeTest :: forall a b.
    (CandidArgs a, Eq a, Show a) =>
    (CandidArgs b, Eq b, Show b) =>
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
    , roundTripProp @ '[ 'OtherT ('Other SimpleRecord) ]
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
  ]

instance Monad m => Serial m BS.ByteString where
    series = BS.pack <$> series

