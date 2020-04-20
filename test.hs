{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.ByteString.Char8 as B
import Test.Tasty
import Test.Tasty.HUnit

import Codec.Candid

main = defaultMain unitTests


newtype ARecord a = ARecord { foo :: a }
    deriving (Eq, Show)

instance Candid a => Candid (ARecord a) where
    type Rep (ARecord a) = 'RecT '[ '( 'Named "foo", Rep a) ]
    toCandid (ARecord x) = RecV (toCandid x :> EmptyRec)
    fromCandid (RecV (x :> EmptyRec)) = ARecord (fromCandid x)

roundTripTest :: forall a. (CandidArgs a, Eq a, Show a) => a -> Assertion
roundTripTest v1 = do
  let bytes1 = encode v1
  v2 <- case decode @a bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  assertEqual "values" v1 v2


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
    , testCase "simple record" $ roundTripTest $ Unary (ARecord True)
    , testCase "simple variant" $ roundTripTest $ Unary (Left True :: Either Bool Bool)
    , testCase "simple variant" $ roundTripTest $ Unary (Right False :: Either Bool Bool)
    ]
  ]
