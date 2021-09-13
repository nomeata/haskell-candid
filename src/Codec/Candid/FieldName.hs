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
  , invertHash
  , unescapeFieldName
  , escapeFieldName
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS
import Prettyprinter
import Data.String
import Data.Maybe
import Data.Word
import Data.Char
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
candidHash s = BS.foldl (\h c -> h * 223 + fromIntegral c) 0 $ BS.fromStrict $ T.encodeUtf8 s

-- | Inversion of the Candid field label hash
invertHash :: Word32 -> Maybe T.Text
invertHash w32 | w32 < 32 = Nothing
    -- leave small numbers alone, tend to be tuple indicies
invertHash w32 = listToMaybe guesses
  where
    x = fromIntegral w32 :: Word64
    chars = ['a'..'z'] ++ ['_']
    ords = 0 : map (fromIntegral . ord) chars
    non_mod x = x - (x `mod` 2^(32::Int))
    guesses =
        [ T.pack $ reverse guess
        | c8 <- ords, c7 <- ords, c6 <- ords, c5 <- ords
        -- It seems that 8 characters are enough to invert anything
        -- (based on quickchecking)
        -- Set up so that short guesses come first
        , let high_chars = c5 * 223^(4::Int) + c6 * 223^(5::Int) + c7 * 223^(6::Int) + c8 * 223^(7::Int)
        , let guess = simple $ x + non_mod high_chars
        , all (`elem` chars) guess
        ]

    -- inverts the Hash if the hash was created without modulos
    -- returns string in reverse order
    simple :: Word64 -> String
    simple 0 = ""
    simple x = chr (fromIntegral b) : simple a
      where (a, b) = x `divMod` 223


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
    pretty (FieldName h Nothing)
        | Just x <- invertHash h  = pretty x
        | otherwise               = pretty h


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


