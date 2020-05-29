module Codec.Candid.Parse where

import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String
import Data.Bifunctor
import Data.Char
import Data.Functor
import Numeric.Natural
import Data.Void

import Codec.Candid.Types

-- | A candid service, as a list of methods with argument and result types
--
-- (no support for annotations like query yet)
type DidFile = [ (T.Text, [Type Void], [Type Void]) ]

-- | Parses a Candid description (@.did@) from a string
parseDid :: String -> Either String DidFile
parseDid = first show . parse (allInput fileP) "Candid service"

parseDidType :: String -> Either String (Type Void)
parseDidType = first show . parse (allInput dataTypeP) "Candid type"

parseValue :: String -> Either String Value
parseValue = first show . parse (allInput valueP) "Candid value"

parseValues :: String -> Either String [Value]
parseValues = first show . parse (allInput valuesP) "Candid values (argument sequence)"


allInput :: Parser a -> Parser a
allInput = between spaces eof

fileP :: Parser DidFile
fileP = many defP *> actorP

defP :: Parser ()
defP = typeP <|> importP

typeP :: Parser ()
typeP = s "type" *> fail "type definitions not yet supported"

importP :: Parser ()
importP = s "import" *> fail "imports not yet supported"

actorP :: Parser DidFile
actorP = s "service" *> optional idP *> s ":" *> actorTypeP -- TODO could be a type id

actorTypeP :: Parser DidFile
actorTypeP = braceSemi methTypeP

methTypeP :: Parser (T.Text, [Type Void], [Type Void])
methTypeP = do
    n <- nameP
    s ":"
    (ts1, ts2) <- funcTypeP  -- TODO could be a type id
    return (n, ts1, ts2)

funcTypeP :: Parser ([(Type Void)], [(Type Void)])
funcTypeP = (,) <$> seqP <* s "->" <*> seqP <* many funcAnnP

funcAnnP :: Parser () -- TODO: Annotations are dropped
funcAnnP = s "oneway" <|> s "query"

nameP :: Parser T.Text
nameP = textP <|> T.pack <$> idP <?> "name"

textP :: Parser T.Text -- TODO: Escape sequence
textP = T.pack <$> l (between (char '"') (char '"') (many1 (noneOf "\""))) <?> "text"

seqP :: Parser [Type Void]
seqP = parenComma argTypeP

argTypeP :: Parser (Type Void)
argTypeP = dataTypeP <|> (nameP *> s ":" *> dataTypeP)

dataTypeP :: Parser (Type Void)
dataTypeP = primTypeP <|> constTypeP -- TODO: Ids, reftypes

primTypeP :: Parser (Type Void)
primTypeP = choice
    [ NatT <$ k "nat"
    , Nat8T <$ k "nat8"
    , Nat16T <$ k "nat16"
    , Nat32T <$ k "nat32"
    , Nat64T <$ k "nat64"
    , IntT <$ k "int"
    , Int8T <$ k "int8"
    , Int16T <$ k "int16"
    , Int32T <$ k "int32"
    , Int64T <$ k "int64"
    , Float32T <$ k "float32"
    , Float64T <$ k "float64"
    , BoolT <$ k "bool"
    , TextT <$ k "text"
    , NullT <$ k "null"
    , ReservedT <$ k "reserved"
    , EmptyT <$ k "empty"
    , BlobT <$ k "blob"
    , PrincipalT <$ k "principal"
    ]

constTypeP :: Parser (Type Void)
constTypeP = choice
  [ OptT <$ k "opt" <*> dataTypeP
  , VecT <$ k "vec" <*> dataTypeP
  , RecT <$ k "record" <*> braceSemi fieldTypeP
  , VariantT <$ k "variant" <*> braceSemi fieldTypeP
  ]

fieldTypeP :: Parser (FieldName, Type Void)
fieldTypeP = choice -- TODO : variant shorthands
  [ (,) <$> (escapeFieldHash . fromIntegral <$> natP) <* s ":" <*> dataTypeP
  , (,) <$> (escapeFieldName <$> nameP) <* s ":" <*> dataTypeP
  ]

idP :: Parser String
idP = l ((:)
  <$> satisfy (\c -> isAscii c && isLetter c || c == '_')
  <*> many (satisfy (\c -> isAscii c && isAlphaNum c || c == '_'))
  ) <?> "id"

valuesP :: Parser [Value]
valuesP = (parenComma annValueP <?> "argument sequence")
       <|> ((:[]) <$> annValueP) -- for convenience

annValueP :: Parser Value
annValueP = do
  v <- valueP
  s ":" *> do
        t <- dataTypeP
        smartAnnV v t
   <|> return v

smartAnnV :: Value -> Type Void -> Parser Value
smartAnnV (NatV n) Nat8T = return $ Nat8V (fromIntegral n)
smartAnnV (NatV n) Nat16T = return $ Nat16V (fromIntegral n)
smartAnnV (NatV n) Nat32T = return $ Nat32V (fromIntegral n)
smartAnnV (NatV n) Nat64T = return $ Nat64V (fromIntegral n)
smartAnnV (IntV n) Int8T = return $ Int8V (fromIntegral n)
smartAnnV (IntV n) Int16T = return $ Int16V (fromIntegral n)
smartAnnV (IntV n) Int32T = return $ Int32V (fromIntegral n)
smartAnnV (IntV n) Int64T = return $ Int64V (fromIntegral n)
smartAnnV v ReservedT = return $ AnnV v ReservedT
smartAnnV _ _ = fail "Annotations are only supported around number literals"


valueP :: Parser Value
valueP = choice
  [ parens annValueP
  , IntV . fromIntegral <$> (char '+' *> natP)
  , IntV . negate . fromIntegral <$> (char '-' *> natP)
  , NatV <$> natP
  -- TODO: Floats
  , BoolV True <$ k "true"
  , BoolV False <$ k "false"
  , TextV <$> textP
  , NullV <$ k "null"
  , OptV . Just <$ k "opt" <*> valueP
  , VecV . V.fromList <$ k "vec" <*> braceSemi annValueP
  , RecV <$ k "record" <*> braceSemi fieldValP
  , uncurry VariantV <$ k "variant" <*> braces fieldValP
  -- TODO: Principal
  -- TODO: Blob
  ]

fieldValP :: Parser (FieldName, Value)
fieldValP = choice -- TODO : variant shorthands
  [ (,) <$> (escapeFieldHash . fromIntegral <$> natP) <* s "=" <*> annValueP
  , (,) <$> (escapeFieldName <$> nameP) <* s "=" <*> annValueP
  ]

-- A lexeme
l :: Parser a -> Parser a
l x = x <* spaces

-- a symbol
s :: String -> Parser ()
s str = void (l (string str)) <?> str

-- a keyword
k :: String -> Parser ()
k str = try (void (l (string str <* no)) <?> str)
  where
    no = notFollowedBy (satisfy (\c -> isAscii c && isAlphaNum c || c == '_'))

natP :: Parser Natural
natP = l (read <$> many1 digit <?> "number")

braces :: Parser a -> Parser a
braces = between (s "{") (s "}")
braceSemi :: Parser a -> Parser [a]
braceSemi p = braces $ sepEndBy p (s ";")
parens :: Parser a -> Parser a
parens = between (s "(") (s ")")
parenComma :: Parser a -> Parser [a]
parenComma p = parens $ sepEndBy p (s ",")
