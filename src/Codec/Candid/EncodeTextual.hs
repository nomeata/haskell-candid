module Codec.Candid.EncodeTextual where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import Control.Monad

import Codec.Candid.Parse
import Codec.Candid.Encode

encodeTextual :: String -> Either String BS.ByteString
encodeTextual = parseValues >=> encodeDynValues >=> return . BSL.toStrict . B.toLazyByteString

