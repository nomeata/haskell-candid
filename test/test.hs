{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
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

{-# OPTIONS_GHC -Wno-orphans #-}

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Builder as B
import qualified Data.Vector as V hiding (singleton)
import Test.Tasty
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import qualified Test.Tasty.QuickCheck as QC
import Test.SmallCheck.Series
import Data.Void
import Data.Either
import GHC.Int
import GHC.Word
import Numeric.Natural
import GHC.Generics (Generic)
import Data.Text.Prettyprint.Doc
import Data.Row
import Data.Proxy
import qualified Data.Row.Records as R
import qualified Data.Row.Variants as V

import Codec.Candid
import Codec.Candid.TestExports

import THTests (thTests)
import SpecTests (specTests)

main :: IO ()
main = defaultMainWithRerun tests

newtype Peano = Peano (Maybe Peano)
    deriving (Show, Eq)
    deriving Candid via (Maybe Peano)

peano :: Peano
peano = Peano $ Just $ Peano $ Just $ Peano $ Just $ Peano Nothing

newtype LinkedList a = LinkedList (Maybe (a, LinkedList a))
    deriving (Show, Eq)
    deriving newtype Candid

cons :: a -> LinkedList a -> LinkedList a
cons x y = LinkedList $ Just (x, y)
nil :: LinkedList a
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
    type AsCandid (MiddleField a) = Rec ("_1_" .== a)
    toCandid (MiddleField x) = #_1_ .== x
    fromCandid r = MiddleField (r .! #_1_)

newtype JustRight a = JustRight a
    deriving (Eq, Show)

instance Candid a => Candid (JustRight a) where
    type AsCandid (JustRight a) = Var ("Right" .== a)
    toCandid (JustRight x) = V.singleton (Label @"Right") x
    fromCandid = JustRight . snd . V.unSingleton

data SimpleRecord = SimpleRecord { foo :: Bool, bar :: Word8 }
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

subTypProp :: forall a b.  (CandidArg a, Serial IO a, Show a, CandidArg b) => TestTree
subTypProp = testProperty desc $ \v ->
    isRight $ decode @b (encode @a v)
  where
    desc = show $ pretty (tieKnot (seqDesc @a)) <+> "<:" <+> pretty (tieKnot (seqDesc @b))

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
    Left _err -> return ()
    Right _ -> assertFailure "converse subtype test succeeded"

instance Monad m => Serial m T.Text where
    series = T.pack <$> series

instance (Monad m, Serial m a) => Serial m (V.Vector a) where
    series = V.fromList <$> series

parseTest :: HasCallStack => String -> DidFile -> TestTree
parseTest c e = testCase c $
    case parseDid c of
        Left err -> assertFailure err
        Right s -> s @?= e

printTestType :: forall a. (Candid a, HasCallStack) => String -> TestTree
printTestType e = testCase e $
    show (pretty (typeDesc @a)) @?= e

printTestSeq :: forall a. (CandidArg a, HasCallStack) => String -> TestTree
printTestSeq e = testCase e $
    show (pretty (tieKnot (seqDesc @a))) @?= e

roundTripTestGroup :: String ->
    (forall a. (CandidArg a, Serial IO a, Show a, Eq a) => a -> Either String a) ->
    TestTree
roundTripTestGroup group_desc roundtrip =
    withSomeTypes ("roundtrip (" <> group_desc <> ")") $ \(Proxy :: Proxy a) ->
        let desc = show $ pretty (tieKnot (seqDesc @a)) in
        testProperty desc $ \v ->
            case roundtrip @a v of
                Right y | y == v -> Right ("all good" :: String)
                Right y -> Left $
                    show v ++ " round-tripped to " ++ show y
                Left err -> Left $
                    show v ++ " failed to decode:\n" ++ err

withSomeTypes ::
    String ->
    (forall a. (CandidArg a, Serial IO a, Show a, Eq a) => Proxy a -> TestTree) ->
    TestTree
withSomeTypes groupName mkTest =
    testGroup groupName
    [ mkTest (Proxy @Bool)
    , mkTest (Proxy @Natural)
    , mkTest (Proxy @Word8)
    , mkTest (Proxy @Word16)
    , mkTest (Proxy @Word32)
    , mkTest (Proxy @Word64)
    , mkTest (Proxy @Integer)
    , mkTest (Proxy @Int8)
    , mkTest (Proxy @Int16)
    , mkTest (Proxy @Int32)
    , mkTest (Proxy @Int64)
    , mkTest (Proxy @Float)
    , mkTest (Proxy @Double)
    , mkTest (Proxy @T.Text)
    , mkTest (Proxy @())
    , mkTest (Proxy @Reserved)
    , mkTest (Proxy @Principal)
    , mkTest (Proxy @BS.ByteString)
    , mkTest (Proxy @(Maybe T.Text))
    , mkTest (Proxy @(V.Vector T.Text))
    , mkTest (Proxy @EmptyRecord)
    , mkTest (Proxy @(ARecord T.Text))
    , mkTest (Proxy @(Either Bool T.Text))
    , mkTest (Proxy @SimpleRecord)
    , mkTest (Proxy @(Rec ("a" .== Bool .+ "b" .== Bool .+ "c" .== Bool)))
    , mkTest (Proxy @(V.Var ("upgrade" .== () .+ "reinstall" .== () .+ "install" .== ())))
    , mkTest (Proxy @FuncRef)
    , mkTest (Proxy @ServiceRef)
    ]

tests :: TestTree
tests = testGroup "tests"
  [ specTests
  , testGroup "encode tests"
    [ testCase "empty" $ encode () @?= B.pack "DIDL\0\0"
    , testCase "bool" $ encode (Unary True) @?= B.pack "DIDL\0\1\x7e\1"
    ]
  , testGroup "decode error message"
      [ testCase "simple mismatch" $ fromCandidVals @(Unary ()) (toCandidVals True) @?= Left "Cannot coerce true into null"
      , testCase "missing variant" $ fromCandidVals @(Either () ()) (toCandidVals (V.singleton #foo ())) @?= Left "Unexpected tag foo"
      , testCase "error in variant" $ fromCandidVals @(Either () ()) (toCandidVals (Left @Bool @() True)) @?= Left "Cannot coerce true into null"
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
    , testCase "custom record" $ roundTripTest $ Unary (SimpleRecord True 42)
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
    , testCase "records" $ subTypeTest (Unary (SimpleRecord True 42)) (Unary (ARecord True))
    ]

  , roundTripTestGroup "Haskell → Candid → Haskell" $ \(v :: a) ->
        decode @a (encode @a v)
  , roundTripTestGroup "Haskell → [Value] → Haskell" $ \(v :: a) ->
        fromCandidVals (toCandidVals @a v)
  , roundTripTestGroup "Haskell → [Value] → Candid → Haskell" $ \(v :: a) ->
        encodeDynValues (toCandidVals @a v) >>= decode @a . B.toLazyByteString
  , roundTripTestGroup "Haskell → [Value] → Textual → [Value] → Haskell" $ \(v :: a) ->
        parseValues (show (pretty (toCandidVals @a v))) >>= fromCandidVals @a
  , roundTripTestGroup "Haskell → [Value] → Textual → [Value] → Candid → Haskell" $ \(v :: a) ->
        parseValues (show (pretty (toCandidVals @a v))) >>= encodeDynValues >>= decode @a . B.toLazyByteString

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
  , testGroup "candid type printing" $
    [ printTestType @Bool "bool"
    , printTestType @Integer "int"
    , printTestType @Natural "nat"
    , printTestType @Int8 "int8"
    , printTestType @Word8 "nat8"
    , printTestType @SimpleRecord "record {bar : nat8; foo : bool}"
    , printTestType @(JustRight T.Text) "variant {Right : text}"
    , printTestSeq @() "()"
    , printTestSeq @(Unary ()) "(null)"
    , printTestSeq @(Unary (Bool, Bool)) "(record {0 : bool; 1 : bool})"
    , printTestSeq @((),()) "(null, null)"
    , printTestSeq @(Bool,Bool) "(bool, bool)"
    , printTestSeq @(Bool,(Bool, Bool)) "(bool, record {0 : bool; 1 : bool})"
    , printTestSeq @Bool "(bool)"
    ]
  , testGroup "candid value printing" $
    let t :: Value -> String -> TestTree
        t v e = testCase e $ show (pretty v) @?= e
    in
    [ t (BoolV True) "true"
    , t (BoolV False) "false"
    , t (NatV 1) "1"
    , t (IntV 1) "+1"
    , t (IntV 0) "+0"
    , t (IntV (-1)) "-1"
    , t (Nat8V 1) "(1 : nat8)"
    , t (RecV [("bar", TextV "baz")]) "record {bar = \"baz\"}"
    , t (FuncV (Principal "\xde\xad\xbe\xef") "foo") "func \"psokg-ww6vw-7o6\".\"foo\""
    , t (ServiceV (Principal "\xde\xad\xbe\xef")) "service \"psokg-ww6vw-7o6\""
    , t (PrincipalV (Principal "")) "principal \"aaaaa-aa\""
    , t (PrincipalV (Principal "\xab\xcd\x01")) "principal \"em77e-bvlzu-aq\""
    , t (PrincipalV (Principal "\xde\xad\xbe\xef")) "principal \"psokg-ww6vw-7o6\""
    ]
  , testGroup "candid value printing (via binary) " $
    let t :: forall a. (HasCallStack, CandidArg a) => a -> String -> TestTree
        t v e = testCase e $ do
          let bytes = encode v
          vs <- either assertFailure return $ decodeVals bytes
          show (pretty vs) @?= e
    in
    [ t True "(true)"
    , t (SimpleRecord False 42) "(record {bar = (42 : nat8); foo = false})"
    , t (JustRight (Just (3 :: Natural))) "(variant {gp_jocd = opt 3})"
    , t (JustRight (3 :: Word8)) "(variant {gp_jocd = (3 : nat8)})"
    , t () "()"
    , t (Unary ()) "(null)"
    , t (Unary (True, False)) "(record {true; false})"
    , t (Unary (True, (True, False))) "(record {true; record {true; false}})"
    , t (#_0_ .== True .+ #_1_ .== False) "(record {true; false})"
    ]

  , testGroup "dynamic values (AST)" $
    let t :: forall a. (HasCallStack, CandidArg a, Eq a, Show a) => String -> a -> TestTree
        t s e = testCase s $ do
          bytes <- either assertFailure return $ encodeTextual s
          x <- either assertFailure return $ decode @a bytes
          x @?= e

        t' :: HasCallStack => String -> TestTree
        t' s = testCase ("Bad: " <> s) $ do
          vs <- either assertFailure return $ parseValues s
          case encodeDynValues vs of
            Left _err -> return ()
            Right _ -> assertFailure "Ill-typed value encoded?"
    in
    [ t "true" True
    , t "false" False
    , t "1" (1 :: Natural)
    , t "1 : nat8" (1 :: Word8)
    , t "record { bar = \"baz\" }" (#bar .== ("baz":: T.Text))
    , t "vec {}" (V.fromList [] :: V.Vector Void)
    , t "vec {4; +4}" (V.fromList [4 :: Integer,4])
    , t "vec {4; null : reserved}" (V.fromList [Reserved, Reserved])
    , t "vec {record {}; record {0 = true}}" (V.fromList [R.empty, R.empty])
    , t "vec {variant {a = true}; variant {b = null}}"
        (V.fromList [IsJust #a True, IsJust #b () :: V.Var ("a" V..== Bool V..+ "b" V..== ())])
    , t "\"hello\"" ("hello" :: T.Text)
    , t "blob \"hello\"" ("hello" :: BS.ByteString)
    , t "blob \"\\00\\ff\"" ("\x00\xff" :: BS.ByteString)
    , t "func \"psokg-ww6vw-7o6\".\"foo\""
        (FuncRef (ServiceRef (Principal "\xde\xad\xbe\xef")) "foo")
    , t "func \"psokg-ww6vw-7o6\".foo"
        (FuncRef (ServiceRef (Principal "\xde\xad\xbe\xef")) "foo")
    , t "func \"psokg-ww6vw-7o6\".\"\""
        (FuncRef (ServiceRef (Principal "\xde\xad\xbe\xef")) "")
    , t "service \"psokg-ww6vw-7o6\""
        (ServiceRef (Principal "\xde\xad\xbe\xef"))
    , t "principal \"psokg-ww6vw-7o6\""
        (Principal "\xde\xad\xbe\xef")

    , t' "vec {true; 4}"
    ]

  , testGroup "candid type parsing"
    [ parseTest "service : {}" $
      DidFile [] []
    , parseTest "service : { foo : (text) -> (text) }" $
      DidFile [] [ DidMethod "foo" [TextT] [TextT] ]
    , parseTest "service : { foo : (text,) -> (text,); }" $
      DidFile [] [ DidMethod "foo" [TextT] [TextT] ]
    , parseTest "service : { foo : (x : text,) -> (y : text,); }" $
      DidFile [] [ DidMethod "foo" [TextT] [TextT] ]
    , parseTest "service : { foo : (opt text) -> () }" $
      DidFile [] [ DidMethod "foo" [OptT TextT] []  ]
    , parseTest "service : { foo : (record { text; blob }) -> () }" $
      DidFile [] [ DidMethod "foo" [RecT [(hashedField 0, TextT), (hashedField 1, BlobT)]] []  ]
    , parseTest "service : { foo : (record { x_ : null; 5 : nat8 }) -> () }" $
      DidFile [] [ DidMethod "foo" [RecT [("x_", NullT), (hashedField 5, Nat8T)]] [] ]
    , parseTest "service : { foo : (record { x : null; 5 : nat8 }) -> () }" $
      DidFile [] [ DidMethod "foo" [RecT [("x", NullT), (hashedField 5, Nat8T)]] [] ]
    , parseTest "type t = int; service : { foo : (t) -> (t) }" $
      DidFile [("t", IntT)] [ DidMethod "foo" [RefT "t"] [RefT "t"] ]
    ]
  , thTests
  , testProperty "field name escaping round-tripping" $ \e ->
      let f = either labledField hashedField e in
      let f' = unescapeFieldName (escapeFieldName f) in
      f' == f
  , testGroup "candid hash inversion"
    [ QC.testProperty "small names invert" $
        QC.forAll (QC.choose (0,4)) $ \len ->
        QC.forAll (T.pack <$> QC.vectorOf len (QC.elements ('_':['a'..'z']))) $ \s ->
        candidHash s >= 32 QC.==>
        invertHash (candidHash s) QC.=== Just s
    , QC.testProperty "all hashes find something" $
        QC.forAll QC.arbitraryBoundedIntegral $ \w ->
        w >= 32 QC.==> case invertHash w of
            Nothing -> False
            Just s -> candidHash s == w
    ]
  ]

instance Monad m => Serial m BS.ByteString where
    series = BS.pack <$> series

instance Monad m => Serial m Principal where
    series = Principal <$> series

instance Monad m => Serial m Reserved where
    series = Reserved <$ series @m @()

instance Monad m => Serial m FuncRef where
    series = FuncRef <$> series <*> series

instance Monad m => Serial m ServiceRef where
    series = ServiceRef <$> series

instance (Monad m, Forall r (Serial m), AllUniqueLabels r) => Serial m (Rec r) where
    series = R.fromLabelsA @(Serial m) (\_l -> series)

instance (Monad m, Forall r (Serial m), AllUniqueLabels r) => Serial m (Var r) where
    series = V.fromLabels @(Serial m) (\_l -> series)
