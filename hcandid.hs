{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Text.Hex as T
import Options.Applicative
import Control.Monad
import Codec.Candid
import Data.Char
import System.IO
import System.Exit
import Prettyprinter
import Prettyprinter.Util

err :: String -> IO b
err s = hPutStr stderr s >> exitFailure

fromHex :: String -> IO BS.ByteString
fromHex = maybe (err "Invalid hex data") (return . BS.fromStrict) . T.decodeHex . T.pack . filter (not . isSpace)

fromRust :: String -> IO BS.ByteString
fromRust = go
  where
    go :: String -> IO BS.ByteString
    go "" = return mempty
    go ('\\':'x':h1:h2:xs)
      | Just b <- T.decodeHex (T.pack [h1,h2])
      = (BS.fromStrict b <>) <$> go xs
    go ('\\':c:xs)
      | ord c <= 0xff
      = (BS.singleton (fromIntegral (ord c)) <>) <$> go xs
    go (c:xs)
      | ord c <= 0xff
      = (BS.singleton (fromIntegral (ord c)) <>) <$> go xs
    go xs = err $ "Stuck parsing rust string at\n" <> xs

decodeCandid :: BS.ByteString -> IO ()
decodeCandid b = case decodeVals b of
  Left msg -> err msg
  Right (_, vs) -> putDocW 80 (pretty vs) >> putStrLn ""

encodeCandid :: String -> IO ()
encodeCandid txt = case parseValues txt of
  Left msg -> err msg
  Right vs -> case encodeDynValues vs of
    Left msg -> err msg
    Right b -> BS.putStr (BS.toLazyByteString b)

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Candid tooling"
  -- <> progDesc "A stand-alone local reference implementation of the Internet Computer"
  )
  where
    parser :: Parser (IO ())
    parser =
      (decodeCandid =<<) <$> (
        flag' () (long "decode" <> help "Decode bytes to Candid")
        *> (
            fromHex <$> strOption (long "hex" <> help "parse hex data")
        <|> fromRust <$> strOption (long "rust" <> help "parse text with \\xFF escapes")
        <|> flag' BS.getContents (long "stdin" <> help "read data from stdin")
        )
      ) <|>
      encodeCandid <$> strOption (long "encode" <> help "encode Candid textual form, at inferred type)")
