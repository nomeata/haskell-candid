module Codec.Candid.EncodeTextual where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as B
import Control.Monad

import Codec.Candid.Parse
import Codec.Candid.Encode

-- | Encodes a Candid value given in textual form.
--
-- This may fail if the textual form cannot be parsed or has inconsistent
-- types. It does not use the @reserved@ supertype (unless explicitly told to).
encodeTextual :: String -> Either String BS.ByteString
encodeTextual = parseValues >=> encodeDynValues >=> return . B.toLazyByteString

