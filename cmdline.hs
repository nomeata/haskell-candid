{-# LANGUAGE TupleSections #-}

import Data.Text.Prettyprint.Doc (pretty)
import qualified Data.Text as T
import qualified Data.Vector as V
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

val :: Type Void -> (String, ReadM Value)
val TextT = ("STR", TextV <$> str)
val NatT = ("NAT", NatV <$> auto)
val t = ("CANDID", eitherReader parseValue)

plainFieldArg :: FieldName -> Type Void -> Parser Value
plainFieldArg fn t = option p (long (show (pretty fn)) <> metavar mv <> help ("type: " <> show (pretty t)))
  where (mv, p) = val t

fieldArg :: FieldName -> Type Void -> Parser Value
fieldArg fn BoolT = BoolV <$> switch (long (show (pretty fn)) <> help "type: bool")
fieldArg fn (VecT t) = VecV . V.fromList <$> many (plainFieldArg fn t)
fieldArg fn (OptT t) = OptV <$> optional (plainFieldArg fn t)
fieldArg fn t = plainFieldArg fn t

field :: (FieldName, Type Void) -> Parser (FieldName, Value)
field (fn, t) = (fn,) <$> fieldArg fn t

arg :: Type Void -> Parser Value
arg (RecT fs) = RecV <$> traverse field fs
arg t = argument p ( metavar mv <> help ("type: " <> show (pretty t)) )
  where (mv, p) = val t

args :: [Type Void] -> Parser [Value]
args = traverse arg

service :: DidFile -> Parser (T.Text, [Value])
service did = hsubparser $ mconcat
    [ command (T.unpack m) $ info ((m,) <$> args a) $
        progDesc ("Invoke " <> T.unpack m)
        <> fullDesc
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
        (prefs (showHelpOnError <> showHelpOnEmpty))
        (info (helper <*> service did) (
            fullDesc <> progDesc "Invoke methods on a canister"
        ))
        (toList method_name ++ args)
    print $ pretty (m,x)
