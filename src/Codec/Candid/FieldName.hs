{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- This module keeps the FieldName type abstract,
-- to ensure that the field name hash is correct
module Codec.Candid.FieldName
  ( FieldName
  , labledField
  , hashedField
  , fieldHash
  , candidHash
  , unescapeFieldName
  , escapeFieldName
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS
import Data.Text.Prettyprint.Doc
import Data.String
import Data.Word
import Numeric.Natural
import Data.Function
import Text.Read (readMaybe)

-- | A type for a Candid field name. Essentially a 'Word32' with maybe a textual label attached
data FieldName = FieldName
    { fieldHash :: Word32 -- ^ Extract the raw field hash value
    , fieldName :: Maybe T.Text
    }
  deriving Show

-- | Create a 'FieldName' from a label
labledField :: T.Text -> FieldName
labledField s = FieldName (candidHash s) (Just s)

-- | Create a 'FieldName' from the raw hash
hashedField :: Word32 -> FieldName
hashedField h = FieldName h Nothing

-- | The Candid field label hashing algorithm
candidHash :: T.Text -> Word32
candidHash s = BS.foldl (\h c -> (h * 223 + fromIntegral c)) 0 $ BS.fromStrict $ T.encodeUtf8 s

instance Eq FieldName where
    (==) = (==) `on` fieldHash
    (/=) = (/=) `on` fieldHash

instance Ord FieldName where
    compare = compare `on` fieldHash
    (<) = (<) `on` fieldHash
    (>) = (>) `on` fieldHash
    (<=) = (<=) `on` fieldHash
    (>=) = (>=) `on` fieldHash

instance IsString FieldName where
    fromString = labledField . fromString

instance Pretty FieldName where
    pretty (FieldName _ (Just x)) = pretty x
    pretty (FieldName h Nothing) = pretty h


-- | The inverse of 'escapeFieldName'
unescapeFieldName :: T.Text -> FieldName
unescapeFieldName n
    | Just ('_',r') <- T.uncons n
    , Just (r,'_') <- T.unsnoc r'
    , Just (n' :: Natural) <- readMaybe (T.unpack r)
    , n' <= fromIntegral (maxBound :: Word32)
    = hashedField (fromIntegral n')
    | Just (n', '_') <- T.unsnoc n
    = labledField n'
    | otherwise
    = labledField n

-- | Represent a 'FieldName' (which may be numeric) in contexts where only text
-- is allowed, using the same encoding/decoding algorithm as Motoko.
--
-- This used in the 'Codec.Candid.Class.Candid' instance for 'Data.Row.Rec' and
-- 'Data.Row.Vec'
escapeFieldName :: FieldName -> T.Text
escapeFieldName (FieldName _ (Just "")) = ""
escapeFieldName (FieldName _ (Just n)) | T.last n == '_' = n <> "_"
escapeFieldName (FieldName _ (Just n)) = n
escapeFieldName (FieldName h Nothing) = T.singleton '_' <> T.pack (show h) <> T.singleton '_'


