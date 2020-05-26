module Codec.Candid.Parse where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Data.Bifunctor
import Data.Word
import Data.Char
import Data.Functor
import qualified Data.HashMap.Strict as HM

import Codec.Candid.Core

type Service = HM.HashMap T.Text ([Type], [Type])

parseService :: T.Text -> Either String Service
parseService = first show . parse fileP "Candid service"

fileP :: Parser Service
fileP = spaces *> many defP *> actorP

defP :: Parser ()
defP = typeP <|> importP

typeP :: Parser ()
typeP = s "type" *> fail "type definitions not yet supported"

importP :: Parser ()
importP = s "import" *> fail "imports not yet supported"

actorP :: Parser Service
actorP = s "service" *> optional idP *> s ":" *> actorTypeP -- TODO could be a type id

actorTypeP :: Parser Service
actorTypeP = HM.fromList <$> braceSemi methTypeP

methTypeP :: Parser (T.Text, ([Type], [Type]))
methTypeP = (,) <$> nameP <* s ":" <*> funcTypeP -- TODO could be a type id

funcTypeP :: Parser ([Type], [Type])
funcTypeP = (,) <$> seqP <* s "->" <*> seqP <* many funcAnnP

funcAnnP :: Parser () -- TODO: Annotations are dropped
funcAnnP = s "oneway" <|> s "query"

nameP :: Parser T.Text
nameP = T.pack <$> idP -- TODO: Quoted strings

seqP :: Parser [Type]
seqP = parenComma argTypeP

argTypeP :: Parser Type
argTypeP = dataTypeP <|> (nameP *> s ":" *> dataTypeP)

dataTypeP :: Parser Type
dataTypeP = primTypeP <|> constTypeP -- TODO: Ids, reftypes

primTypeP :: Parser Type
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
    ]

constTypeP :: Parser Type
constTypeP = choice
  [ OptT <$ k "opt" <*> dataTypeP
  , VecT <$ k "vec" <*> dataTypeP
  , RecT <$ k "record" <*> braceSemi fieldTypeP
  , VariantT <$ k "variant" <*> braceSemi fieldTypeP
  ]

fieldTypeP :: Parser (FieldName, Type)
fieldTypeP = choice -- TODO : variant shorthands
  [ (,) <$> (H' <$> natP) <* s ":" <*> dataTypeP
  , (,) <$> (N' <$> nameP) <* s ":" <*> dataTypeP
  ]

idP :: Parser String
idP = l ((:)
  <$> satisfy (\c -> isAscii c && isLetter c || c == '_')
  <*> many (satisfy (\c -> isAscii c && isAlphaNum c || c == '_'))
  ) <?> "id"


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

natP :: Parser Word32
natP = l (read <$> many1 digit <?> "number")

braceSemi :: Parser a -> Parser [a]
braceSemi p = between (s "{") (s "}") $ sepEndBy p (s ";")
parenComma :: Parser a -> Parser [a]
parenComma p = between (s "(") (s ")") $ sepEndBy p (s ",")
