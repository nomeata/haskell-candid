{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

import qualified Data.ByteString.Char8 as B
import Test.Tasty
import Test.Tasty.HUnit

import Codec.Candid

main = defaultMain unitTests


roundTripTest :: forall ts. DecodeTypes ts => Seq ts -> Assertion
roundTripTest v1 = do
  let bytes1 = encode v1
  v2 <- case decode @ts bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  assertEqual "values" v1 v2


unitTests = testGroup "Unit tests"
  [ testGroup "encode tests"
    [ testCase "empty" $
        encode args0 @?= B.pack "DIDL\0\0"
    , testCase "bool" $
        encode (args1 (BoolV True)) @?= B.pack "DIDL\0\1\x7e\1"
    ]
  , testGroup "roundtrip"
    [ testCase "empty" $ roundTripTest args0
    , testCase "bool" $ roundTripTest $ args1 (BoolV True)
    , testCase "simple record" $ roundTripTest $ args1
        (RecV (BoolV True :> EmptyRec) :: Val ('RecT '[ '( 'Named "foo", 'BoolT)]))
    , testCase "simple variant" $ roundTripTest $ args1
        (VariantV (This (BoolV True)) :: Val ('VariantT '[ '( 'Named "foo", 'BoolT)]))
    ]
  ]
