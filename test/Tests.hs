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

module Tests (tests) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V hiding (singleton)
import Test.Tasty
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
import Prettyprinter
import Data.Row
import Data.Proxy
import qualified Data.Row.Records as R
import qualified Data.Row.Variants as V

import Codec.Candid
import Codec.Candid.TestExports

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

subTypeRoundTripProp :: forall a b.  (CandidArg a, Serial IO a, Show a, CandidArg b) => TestTree
subTypeRoundTripProp = testProperty desc $ \v ->
    isRight $ decode @b (encode @a v)
  where
    desc = show $ pretty (tieKnot (seqDesc @a)) <+> "<:" <+> pretty (tieKnot (seqDesc @b))

subTypeRoundTripTest' :: forall a b.
    (CandidArg a, Eq a, Show a) =>
    (CandidArg b, Eq b, Show b) =>
    a -> b -> Assertion
subTypeRoundTripTest' v1 v2 = do
  let bytes1 = encode v1
  v2' <- case decode @b bytes1 of
    Left err -> assertFailure err
    Right v -> return v
  v2 @=? v2'

subTypeRoundTripTest :: forall a b.
    (CandidArg a, Eq a, Show a) =>
    (CandidArg b, Eq b, Show b) =>
    a -> b -> Assertion
subTypeRoundTripTest v1 v2 = do
  subTypeRoundTripTest' v1 v2
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
    testGroup ("roundtrip (" <> group_desc <> ")") $ withSomeTypes $ \(Proxy :: Proxy a) ->
        let desc = show $ pretty (tieKnot (seqDesc @a)) in
        testProperty desc $ \v ->
            case roundtrip @a v of
                Right y | y == v -> Right ("all good" :: String)
                Right y -> Left $
                    show v ++ " round-tripped to " ++ show y
                Left err -> Left $
                    show v ++ " failed to decode:\n" ++ err

withSomeTypes ::
    (forall a. (CandidArg a, Serial IO a, Show a, Eq a) => Proxy a -> b) -> [b]
withSomeTypes mkTest =
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
    , mkTest (Proxy @(FuncRef (Bool, T.Text, AnnFalse, AnnFalse)))
    , mkTest (Proxy @(FuncRef (Bool, T.Text, AnnTrue, AnnFalse)))
    , mkTest (Proxy @(FuncRef (Bool, T.Text, AnnFalse, AnnTrue)))
    , mkTest (Proxy @(ServiceRef Empty))
    ]

tests :: [TestTree]
tests =
  [ testGroup "encode tests"
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
  , testGroup "subtype roundtrips"
    [ testCase "nat/int" $ subTypeRoundTripTest (Unary (42 :: Natural)) (Unary (42 :: Integer))
    , testCase "null/opt" $ subTypeRoundTripTest (Unary ()) (Unary (Nothing @Integer))
    , testCase "rec" $ subTypeRoundTripTest (ARecord True, True) (EmptyRecord, True)
    , testCase "tuple" $ subTypeRoundTripTest ((42::Integer,-42::Integer), 100::Integer) (EmptyRecord, 100::Integer)
    , testCase "variant" $ subTypeRoundTripTest' (JustRight (42 :: Natural), True) (Right 42 :: Either Bool Natural, True)
    , testCase "rec/any" $ subTypeRoundTripTest (ARecord True, True) (Reserved, True)
    , testCase "tuple/any" $ subTypeRoundTripTest ((42::Integer, 42::Natural), True) (Reserved, True)
    , testCase "tuple/tuple" $ subTypeRoundTripTest ((42::Integer,-42::Integer,True), 100::Integer) ((42::Integer, -42::Integer), 100::Integer)
    , testCase "tuple/middle" $ subTypeRoundTripTest ((42::Integer,-42::Integer,True), 100::Integer) (MiddleField (-42) :: MiddleField Integer, 100::Integer)
    , testCase "records" $ subTypeRoundTripTest (Unary (SimpleRecord True 42)) (Unary (ARecord True))
    ]

  , roundTripTestGroup "Haskell → Candid → Haskell" $ \(v :: a) ->
        decode @a (encode @a v)
  , roundTripTestGroup "Haskell → [Value] → Haskell" $ \(v :: a) ->
        fromCandidVals (toCandidVals @a v)
  , roundTripTestGroup "Haskell → [Value] → Textual → [Value] → Haskell" $ \(v :: a) ->
        parseValues (show (pretty (toCandidVals @a v))) >>= fromCandidVals @a

  , testGroup "subtype round trip smallchecks"
    [ subTypeRoundTripProp @Natural @Natural
    , subTypeRoundTripProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8)) @Reserved
    , subTypeRoundTripProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8)) @(Rec ("Hi" .== Reserved))
    , subTypeRoundTripProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8)) @(Rec ("Hi" .== Word8))
    , subTypeRoundTripProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8)) @(Rec ("_1_" .== Word8))
    , subTypeRoundTripProp @(Rec ("Hi" .== Word8 .+ "_1_" .== Word8 .+ "_2_" .== Bool)) @(Rec ("_1_" .== Word8))
    , subTypeRoundTripProp @(Maybe (Rec ("Hi" .== Word8 .+ "_1_" .== Word8 .+ "_0_" .== Bool))) @(Maybe (Bool,Word8))
    , subTypeRoundTripProp @(Var ("Hi" .== Word8)) @(Var ("Hi" .== Word8 .+ "Ho" .== T.Text))
    , subTypeRoundTripProp @(Var ("Ho" .== T.Text)) @(Var ("Hi" .== Word8 .+ "Ho" .== T.Text))
    , subTypeRoundTripProp @Natural @Reserved
    , subTypeRoundTripProp @BS.ByteString @Reserved
    , subTypeRoundTripProp @BS.ByteString @(V.Vector Word8)
    , subTypeRoundTripProp @(V.Vector Word8) @BS.ByteString
    , subTypeRoundTripProp @Principal @Reserved
    ]

  , testGroup "subtype test" $
    [ testGroup "reflexivity" $ concat $ withSomeTypes $ \(Proxy :: Proxy a) ->
        let td = seqDesc @a in
        unrollTypeTable td $ \ts ->
           [ testCase (show (pretty t)) $ assertRight $ t `isSubtypeOf` t | t <- ts ]
    , testGroup "negative tests"
      [ let t1 = typeGraph @Integer
            t2 = typeGraph @Natural
        in testCase (show (pretty t1) ++ " </: " ++ show (pretty t2)) $
           assertLeft $ t1 `isSubtypeOf` t2
      ]
    ]
  , testGroup "candid type printing" $
    [ printTestType @Bool "bool"
    , printTestType @Integer "int"
    , printTestType @Natural "nat"
    , printTestType @Int8 "int8"
    , printTestType @Word8 "nat8"
    , printTestType @SimpleRecord "record {bar : nat8; foo : bool}"
    , printTestType @(JustRight T.Text) "variant {Right : text}"
    , printTestType @(FuncRef (Bool, Unary (), AnnTrue, AnnFalse)) "func (bool) -> (null) query"
    , printTestType @(FuncRef (Bool, T.Text, AnnFalse, AnnTrue)) "func (bool) -> (text) oneway"
    , printTestType @(ServiceRef Empty) "service : {}"
    , printTestType @(ServiceRef ("foo" .== (Bool, T.Text, AnnFalse, AnnTrue) .+ "bar" .== ((),(),AnnFalse, AnnFalse)))
        "service : {bar : () -> (); foo : (bool) -> (text) oneway;}"
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
          (_, vs) <- assertRight $ decodeVals bytes
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
        (FuncRef @((), (), AnnFalse, AnnFalse) (Principal "\xde\xad\xbe\xef") "foo")
    , t "func \"psokg-ww6vw-7o6\".foo"
        (FuncRef @((), (), AnnFalse, AnnFalse) (Principal "\xde\xad\xbe\xef") "foo")
    , t "func \"psokg-ww6vw-7o6\".\"\""
        (FuncRef @((), (), AnnFalse, AnnFalse) (Principal "\xde\xad\xbe\xef") "")
    , t "service \"psokg-ww6vw-7o6\""
        (ServiceRef @Empty (Principal "\xde\xad\xbe\xef"))
    , t "principal \"psokg-ww6vw-7o6\""
        (Principal "\xde\xad\xbe\xef")

    , t' "vec {true; 4}"
    ]

  , testGroup "candid type parsing"
    [ parseTest "service : {}" $
      DidFile [] []
    , parseTest "service : { foo : (text) -> (text) }" $
      DidFile [] [("foo", MethodType [TextT] [TextT] False False)]
    , parseTest "service : { foo : (text,) -> (text,); }" $
      DidFile [] [("foo", MethodType [TextT] [TextT] False False)]
    , parseTest "service : { foo : (x : text,) -> (y : text,); }" $
      DidFile [] [("foo", MethodType [TextT] [TextT] False False)]
    , parseTest "service : { foo : (opt text) -> () }" $
      DidFile [] [("foo", MethodType [OptT TextT] [] False False) ]
    , parseTest "service : { foo : (record { text; blob }) -> () }" $
      DidFile [] [("foo", MethodType [RecT [(hashedField 0, TextT), (hashedField 1, BlobT)]] [] False False) ]
    , parseTest "service : { foo : (record { x_ : null; 5 : nat8 }) -> () }" $
      DidFile [] [("foo", MethodType [RecT [("x_", NullT), (hashedField 5, Nat8T)]] [] False False) ]
    , parseTest "service : { foo : (record { x : null; 5 : nat8 }) -> () }" $
      DidFile [] [("foo", MethodType [RecT [("x", NullT), (hashedField 5, Nat8T)]] [] False False) ]
    , parseTest "service : { foo : (text) -> (text) query }" $
      DidFile [] [("foo", MethodType [TextT] [TextT] True False)]
    , parseTest "service : { foo : (text) -> (text) oneway }" $
      DidFile [] [("foo", MethodType [TextT] [TextT] False True)]
    , parseTest "service : { foo : (text) -> (text) query oneway }" $
      DidFile [] [("foo", MethodType [TextT] [TextT] True True)]
    , parseTest "service : { foo : (text) -> (text) oneway query }" $
      DidFile [] [("foo", MethodType [TextT] [TextT] True True)]
    , parseTest "service : (opt SomeInit) -> { foo : (text) -> (text) oneway query }" $
      DidFile [] [("foo", MethodType [TextT] [TextT] True True)]
    , parseTest "type t = int; service : { foo : (t) -> (t) }" $
      DidFile [("t", IntT)] [("foo", MethodType [RefT "t"] [RefT "t"] False False)]
    ]
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
    , QC.testProperty "long dictionary name" $
        let s = "precriticized" in
        invertHash (candidHash s) QC.=== Just s
    , QC.testProperty "all hashes find something" $
        QC.forAll QC.arbitraryBoundedIntegral $ \w ->
        w >= 32 QC.==> case invertHash w of
            Nothing -> False
            Just s -> candidHash s == w
    ]
  ]

assertRight :: Either String a -> IO a
assertRight = either assertFailure pure

assertLeft :: Either String () -> Assertion
assertLeft = either (const (pure ())) (\() -> assertFailure "unexpected success")

instance Monad m => Serial m BS.ByteString where
    series = BS.pack <$> series

instance Monad m => Serial m Principal where
    series = Principal <$> series

instance Monad m => Serial m Reserved where
    series = Reserved <$ series @m @()

instance Monad m => Serial m (FuncRef mt) where
    series = FuncRef <$> series <*> series

instance Monad m => Serial m (ServiceRef r) where
    series = ServiceRef <$> series

instance (Monad m, Forall r (Serial m), AllUniqueLabels r) => Serial m (Rec r) where
    series = R.fromLabelsA @(Serial m) (\_l -> series)

instance (Monad m, Forall r (Serial m), AllUniqueLabels r) => Serial m (Var r) where
    series = V.fromLabels @(Serial m) (\_l -> series)
