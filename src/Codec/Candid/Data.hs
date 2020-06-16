{-# LANGUAGE OverloadedStrings #-}
-- | A few extra data types
module Codec.Candid.Data where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Text.Hex (encodeHex, decodeHex)
import Data.Digest.CRC
import Data.Digest.CRC8
import Data.Char

data Reserved = Reserved
 deriving (Eq, Ord, Show)

newtype Principal = Principal { rawPrincipal :: BS.ByteString }
 deriving (Eq, Ord, Show)

prettyPrincipal :: Principal -> T.Text
prettyPrincipal (Principal blob) =
    "ic:" <> T.map toUpper (encodeHex (BS.toStrict (blob <> BS.singleton checksum)))
  where
    CRC8 checksum = digest (BS.toStrict blob)

parsePrincipal :: T.Text -> Either String Principal
parsePrincipal t
    | "ic:" `T.isPrefixOf` t
    , Just bs <- BS.fromStrict <$> decodeHex (T.drop 3 t)
    , Just (bs', cs) <- BS.unsnoc bs
    = if CRC8 cs == digest (BS.toStrict bs')
      then Right (Principal bs')
      else Left "Invalid principal: checksum mismatch"
    | otherwise
    = Left "Invalid principal"


