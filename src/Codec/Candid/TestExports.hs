-- | This modules exports internals soley for the purpose of importing them in
-- the test suite
module Codec.Candid.TestExports
    ( module Codec.Candid.Parse
    , module Codec.Candid.TH
    , module Codec.Candid.FieldName
    , module Codec.Candid.TypTable
    , module Codec.Candid.Class
    ) where

import Codec.Candid.Parse
  ( CandidTestFile(..)
  , CandidTest(..)
  , DidFile(..)
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

import Codec.Candid.TypTable
  ( unrollTypeTable
  )

import Codec.Candid.Class
  ( typeGraph
  )
