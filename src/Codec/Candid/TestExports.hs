-- | This modules exports internals soley for the purpose of importing them in
-- the test suite
module Codec.Candid.TestExports
    ( module Codec.Candid.Parse
    , module Codec.Candid.TH
    , module Codec.Candid.FieldName
    ) where

import Codec.Candid.Parse
  ( CandidTestFile(..)
  , CandidTest(..)
  , DidFile(..)
  , DidMethod(..)
  , TestInput(..)
  , TestAssertion(..)
  , parseCandidTests
  )

import Codec.Candid.TH
  ( candidTypeQ
  , generateCandidDefs
  )


import Codec.Candid.FieldName
  ( invertHash
  )
