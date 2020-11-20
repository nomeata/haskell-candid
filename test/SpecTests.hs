{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SpecTests (specTests) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment
import System.Directory
import System.FilePath
import System.IO
import System.Exit
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Codec.Candid
import Codec.Candid.TestExports

-- WARNING: Big Template Haskell mess ahead
specTests :: TestTree
specTests = testGroup "Candid spec tests" $(do
    candid_tests <- runIO (lookupEnv "CANDID_TESTS") >>= \case
        Nothing -> runIO $ [] <$ putStrLn "CANDID_TESTS not set, will not run candid spec test"
        Just dir -> do
            files <- runIO $ listDirectory dir
            sequence
              [ do addDependentFile file
                   content <- runIO $ readFile file
                   case parseCandidTests file content of
                       Left err -> runIO $ do
                         hPutStrLn stderr $ "Failed to parse " ++ file ++ ":"
                         hPutStrLn stderr err
                         exitFailure
                       Right x -> return (name, x)
              | basename <- files
              , let file = dir </> basename
              , Just name <- pure $ T.stripSuffix ".test.did" (T.pack basename)
              , name /= "construct" -- for now
              ]
    listE
      [ [| testGroup ("File " ++ $(liftString (T.unpack name))) $(listE
          [ [| testCase name $( case testAssertion of
            CanParse i1 -> [|
                case $(parseInput i1) of
                    Right _  -> return ()
                    Left err -> assertFailure $ "unexpected decoding error:\n" ++ err
                |]
            CannotParse i1 -> [|
                case $(parseInput i1) of
                    Right _ -> assertFailure $ "unexpected decoding success"
                    Left _  -> return ()
                |]
            ParseEq exp i1 i2 -> [|
                case ($(parseInput i1), $(parseInput i2)) of
                    (Right v1, Right v2) ->
                        if exp then assertBool "values differ" (v1 == v2)
                               else assertBool "values do not differ" (v1 /= v2)
                    (Left err, _) ->
                        assertFailure $ "unexpected decoding error (left arg):\n" ++ err
                    (_, Left err) ->
                        assertFailure $ "unexpected decoding error (right arg):\n" ++ err
                |]
          )|]
          | CandidTest{..} <- testTests tests
          , let name = "[l" ++ show testLine ++ "]" ++
                     case testDesc of
                         Nothing -> ""
                         Just dsc -> " " ++ T.unpack dsc
          , let testType' = fmap (error . T.unpack) <$> testType
                parseInput (FromBinary blob) =
                  [| decode @ $(candidTypeQ testType') (BS.pack $(lift (BS.unpack blob))) |]
                parseInput (FromTextual txt) =
                  [| parseValues $(liftString (T.unpack txt)) >>= fromCandidVals @ $(candidTypeQ testType') |]
          ])
        |]
      | (name, tests) <- candid_tests
      ]
  )
