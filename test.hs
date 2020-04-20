{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.ByteString.Char8 as B
import Test.Tasty
import Test.Tasty.HUnit
import Numeric.Natural
import GHC.TypeLits


import Codec.Candid

main = defaultMain unitTests


newtype ARecord a = ARecord { foo :: a }
    deriving (Eq, Show)

instance Candid a => Candid (ARecord a) where
    type Rep (ARecord a) = 'RecT '[ '( 'Named "foo", Rep a) ]
    toCandid (ARecord x) = RecV (toCandid x :> EmptyRec)
    fromCandid (RecV (x :> EmptyRec)) = ARecord (fromCandid x)

newtype SingleField (n::Nat) a = SingleField a
    deriving (Eq, Show)

instance (KnownNat n, Candid a) => Candid (SingleField n a) where
    type Rep (SingleField n a) = 'RecT '[ '( 'Hashed n, Rep a) ]
    toCandid (SingleField x) = RecV (toCandid x :> EmptyRec)
    fromCandid (RecV (x :> EmptyRec)) = SingleField (fromCandid x)

newtype JustRight a = JustRight a
    deriving (Eq, Show)

instance Candid a => Candid (JustRight a) where
    type Rep (JustRight a) = 'VariantT '[ '( 'Named "Right", Rep a) ]
    toCandid (JustRight x) = VariantV (This (toCandid x))
    fromCandid (VariantV (This x)) = JustRight (fromCandid x)

roundTripTest :: forall a. (CandidArgs a, Eq a, Show a) => a -> Assertion
roundTripTest v1 = do
  let bytes1 = encode v1
  v2 <- case decode @a bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  assertEqual "values" v1 v2

subTypeTest :: forall a b.
    (CandidArgs a, Eq a, Show a) =>
    (CandidArgs b, Eq b, Show b) =>
    a -> b -> Assertion
subTypeTest v1 v2 = do
  let bytes1 = encode v1
  v2' <- case decode @b bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  v2 @=? v2'


unitTests = testGroup "Unit tests"
  [ testGroup "encode tests"
    [ testCase "empty" $
        encode () @?= B.pack "DIDL\0\0"
    , testCase "bool" $
        encode (Unary True) @?= B.pack "DIDL\0\1\x7e\1"
    ]
  , testGroup "roundtrip"
    [ testCase "empty" $ roundTripTest ()
    , testCase "bool" $ roundTripTest $ Unary True
    , testCase "simple record 1" $ roundTripTest (ARecord True, False)
    , testCase "simple record 2" $ roundTripTest (ARecord (100000 :: Natural), False)
    , testCase "simple variant 1" $ roundTripTest $ Unary (Left True :: Either Bool Bool)
    , testCase "simple variant 2" $ roundTripTest $ Unary (Right False :: Either Bool Bool)
    , testCase "nested record 2" $ roundTripTest (ARecord (True,False), False)
    ]
  , testGroup "subtypes"
    [ testCase "nat/int" $ subTypeTest (Unary (42 :: Natural)) (Unary (42 :: Integer))
    , testCase "null/opt" $ subTypeTest (Unary NullV) (Unary (Nothing @Integer))
    , testCase "rec" $ subTypeTest (ARecord True, True) (RecV EmptyRec, True)
    , testCase "tuple" $ subTypeTest ((42::Integer,-42::Integer), 100::Integer) (RecV EmptyRec, 100::Integer)
    , testCase "variant" $ subTypeTest (Right 42 :: Either Bool Natural, True) (JustRight (42 :: Natural), True)
    , testCase "rec/any" $ subTypeTest (ARecord True, True) (ReservedV, True)
    , testCase "tuple/any" $ subTypeTest ((42::Integer, 42::Natural), True) (ReservedV, True)
    , testCase "tuple/tuple" $ subTypeTest ((42::Integer,-42::Integer,True), 100::Integer) ((42::Integer, -42::Integer), 100::Integer)
    ]
    , testCase "tuple/middle" $ subTypeTest ((42::Integer,-42::Integer,True), 100::Integer) (SingleField (-42) :: SingleField 2 Integer, 100::Integer)
  ]
