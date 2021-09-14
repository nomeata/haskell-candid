{-# LANGUAGE TemplateHaskell #-}
-- | The hash algorithm used for Candid field names
--
-- Also includes a function that tries to reverse the hash, first using an
-- English word list, and then a brute force approach.
module Codec.Candid.Hash
  ( candidHash
  , invertHash
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.IntMap as M
import Data.Maybe
import Data.Char
import Data.Word
import Data.FileEmbed

-- | The Candid field label hashing algorithm
candidHash :: T.Text -> Word32
candidHash s = BS.foldl (\h c -> h * 223 + fromIntegral c) 0 $ BS.fromStrict $ T.encodeUtf8 s

-- | Inversion of the Candid field label hash
invertHash :: Word32 -> Maybe T.Text
invertHash w32 | w32 < 32 = Nothing
    -- leave small numbers alone, tend to be tuple indicies
invertHash w32 | Just t <- M.lookup (fromIntegral w32) m  = Just t
    -- try the word list
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

-- Word list obtained from https://github.com/dwyl/english-words
ws :: T.Text
ws = $(embedStringFile "words.txt")

m :: M.IntMap T.Text
m = M.fromList [ (fromIntegral (candidHash w), w) | w <- T.lines ws]
