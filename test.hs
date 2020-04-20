{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

import qualified Data.ByteString.Char8 as B
import Test.Tasty
import Test.Tasty.HUnit

import Codec.Candid

main = defaultMain unitTests


roundTripTest :: forall fs. DecodeParams fs => Rec fs -> Assertion
roundTripTest v = do
  let bytes1 = encode v
  v2 <- case decode @fs bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  let bytes2 = encode v2
  assertEqual "serialized bytes" bytes1 bytes2


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
    ]
  ]
