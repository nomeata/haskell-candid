{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- | A few extra data types
module Codec.Candid.Data where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Row.Internal as R
import Data.Digest.CRC32
import Data.ByteString.Base32
import Data.List
import Data.List.Split (chunksOf)
import Data.Bifunctor
import Control.Monad
import Data.Kind

data Reserved = Reserved
 deriving (Eq, Ord, Show)

newtype Principal = Principal { rawPrincipal :: BS.ByteString }
 deriving (Eq, Ord, Show)

prettyPrincipal :: Principal -> T.Text
prettyPrincipal (Principal blob) =
    T.pack $ intercalate "-" $ chunksOf 5 $ base32 $ checkbytes <> blob
  where
    checksum = crc32 (BS.toStrict blob)
    checkbytes = BS.toLazyByteString (BS.word32BE checksum)
    base32 = filter (/='=') . T.unpack . T.toLower . encodeBase32 . BS.toStrict

parsePrincipal :: T.Text -> Either String Principal
parsePrincipal s = do
    all_bytes <- bimap T.unpack BS.fromStrict $
        decodeBase32Unpadded (T.encodeUtf8 (T.filter (/= '-') s))
    unless (BS.length all_bytes >= 4) $
        Left "Too short id"
    let p = Principal (BS.drop 4 all_bytes)
    let expected = prettyPrincipal p
    unless (s == expected) $
        Left $ "Principal id " ++ show s ++ " malformed; did you mean " ++ show expected ++ "?"
    return p

newtype ServiceRef (r :: R.Row Type) = ServiceRef { rawServiceRef :: Principal }
 deriving (Eq, Ord, Show)

data FuncRef r = FuncRef { service :: Principal, method :: T.Text }
 deriving (Eq, Ord, Show)

