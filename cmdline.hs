{-# LANGUAGE TupleSections #-}

import Data.Text.Prettyprint.Doc (pretty)
import qualified Data.Text as T
import Options.Applicative
import Control.Monad (join)
import System.IO
import System.Exit
import Data.Foldable
import Data.Monoid
import Data.Void

import Codec.Candid


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info parser
  (  fullDesc
  <> header "Command-line front-end to candid programs "
  <> forwardOptions
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> strOption
            (  long "candid"
            <> metavar "FILE"
            <> help "Candid file to read"
            )
        <*> optional ( strArgument ( metavar "METHOD" ) )
        <*> many ( strArgument ( metavar "ARGS..." ) )

field :: (FieldName, Type Void) -> Parser (FieldName, Value)
field (fn, TextT) = (fn,) . TextV <$> strOption (long (show (pretty fn)) <> metavar "STR")
field (fn, NatT) = (fn,) . NatV <$> option auto (long (show (pretty fn)) <> metavar "NAT")

arg :: Type Void -> Parser Value
arg TextT = TextV <$> strArgument ( metavar "STR" )
arg (RecT fs) = RecV <$> traverse field fs

args :: [Type Void] -> Parser [Value]
args [] = pure []
args [t] = (:[]) <$> arg t
args xs = error "Only unary functions supported for now"

service :: DidFile -> Parser (T.Text, [Value])
service did = hsubparser $ mconcat
    [ command (T.unpack m) (info ((m,) <$> args a) mempty)
    | (m, a, _) <- did
    ]


work :: String -> Maybe String -> [String] -> IO ()
work file method_name args = do
    candid <- readFile file
    did <- case parseDid candid of
        Left err -> do
            hPutStrLn stderr $ "Could not parse " <> show candid <> ":"
            hPutStrLn stderr err
            exitFailure
        Right did -> return did

    (m,x) <- handleParseResult $ execParserPure
        (prefs showHelpOnError)
        (info (helper <*> service did) fullDesc)
        (toList method_name ++ args)
    print $ pretty (m,x)
