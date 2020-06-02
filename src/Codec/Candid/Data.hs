-- | A few extra data types
module Codec.Candid.Data where

import qualified Data.ByteString.Lazy as BS

data Reserved = Reserved
 deriving (Eq, Ord, Show)

newtype Principal = Principal { rawPrincipal :: BS.ByteString }
 deriving (Eq, Ord, Show)

