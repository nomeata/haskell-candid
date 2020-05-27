-- | A few extra data types
module Codec.Candid.Data where

import qualified Data.ByteString as BS

data Reserved = Reserved
 deriving (Eq, Show)

newtype Principal = Principal { rawPrincipal :: BS.ByteString }
 deriving (Eq, Show)
