-- | This modules exports internals soley for the purpose of importing them in
-- the test suite
module Codec.Candid.TestExports
    ( module Codec.Candid.Parse
    , module Codec.Candid.TH
    ) where

import Codec.Candid.Parse
  ( CandidTests
  , CandidTest(..)
  , TestInput(..)
  , TestAssertion(..)
  , parseCandidTests
  )


import Codec.Candid.TH
  ( candidTypeQ
  )


