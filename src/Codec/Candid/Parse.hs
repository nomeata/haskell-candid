{-# LANGUAGE DeriveTraversable #-}
module Codec.Candid.Parse
  ( DidFile(..)
  , DidDef
  , TypeName
  , parseDid
  , parseDidType
  , parseValue
  , parseValues
  , CandidTestFile(..)
  , CandidTest(..)
  , TestInput(..)
  , TestAssertion(..)
  , parseCandidTests
  )  where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Permutations
import Data.Bifunctor
import Data.Char
import Data.Functor
import Data.Word
import Numeric.Natural
import Numeric
import Control.Monad
import Data.Void
import Text.Read (readMaybe)
import Data.Scientific

import Codec.Candid.Data
import Codec.Candid.Types
import Codec.Candid.FieldName

type Parser = Parsec Void String

-- | Parses a Candid description (@.did@) from a string
parseDid :: String -> Either String DidFile
parseDid = first errorBundlePretty . parse (allInput fileP) "Candid service"

parseDidType :: String -> Either String (Type TypeName)
parseDidType = first errorBundlePretty . parse (allInput dataTypeP) "Candid type"

-- | Parses a Candid textual value from a string
parseValue :: String -> Either String Value
parseValue = first errorBundlePretty . parse (allInput valueP) "Candid value"

-- | Parses a sequence of  Candid textual values from a string
parseValues :: String -> Either String [Value]
parseValues = first errorBundlePretty . parse (allInput valuesP) "Candid values (argument sequence)"

allInput :: Parser a -> Parser a
allInput = between theVoid eof

fileP :: Parser DidFile
fileP = DidFile <$> defsP <*> actorP

defsP :: Parser [DidDef TypeName]
defsP = concat <$> many defP

defP :: Parser [DidDef TypeName]
defP = (typeP <|> importP) <* s ";"

typeP :: Parser [DidDef TypeName]
typeP = fmap (:[]) $
    (,) <$ k "type" <*> idP <* s "=" <*> dataTypeP

importP :: Parser [DidDef TypeName]
importP = withPredicate (const (Left "imports not yet supported")) $
    [] <$ k "import"

-- NOTE: This discards "init" arguments:
-- https://github.com/dfinity/candid/blob/master/spec/Candid.md#core-grammar
-- See also https://github.com/nomeata/haskell-candid/issues/16
actorP :: Parser (DidService TypeName)
actorP = k "service" *> optional idP *> s ":" *> optional (seqP *> s"->") *> actorTypeP -- TODO could be a type id

actorTypeP :: Parser (DidService TypeName)
actorTypeP = braceSemi methP

methP :: Parser (T.Text, MethodType TypeName)
methP = (,) <$> nameP <* s ":" <*> funcTypeP

funcTypeP :: Parser (MethodType TypeName)
funcTypeP = do
    ts1 <- seqP
    s "->"
    ts2 <- seqP
    (q,cq,o) <- runPermutation $
         (,,) <$> toPermutationWithDefault False (True <$ s "query")
              <*> toPermutationWithDefault False (True <$ s "composite_query")
              <*> toPermutationWithDefault False (True <$ s "oneway")
    return $ MethodType ts1 ts2 q cq o

nameP :: Parser T.Text
nameP = textP <|> idP <?> "name"

textP :: Parser T.Text
textP = T.pack <$> l (between (char '"') (char '"') (many stringElem)) <?> "text"

blobP :: Parser BS.ByteString
blobP = BS.concat <$> l (between (char '"') (char '"') (many blobElem)) <?> "blob"

blobElem :: Parser BS.ByteString
blobElem = choice
    [ try (char '\\' *> lookAhead hexdigit) *> do
        raw <- replicateM 2 hexdigit
        case readHex raw of
            [(n,"")] -> return (BS.singleton (fromIntegral (n::Integer)))
            _ -> fail "Internal parsing error parsing hex digits"
    , BS.fromStrict . T.encodeUtf8 . T.singleton <$> stringElem
    ]

stringElem :: Parser Char
stringElem = (char '\\' *> go) <|> noneOf "\""
  where
    go :: Parser Char
    go = choice
        [ '\t' <$ char 't'
        , '\n' <$ char 'n'
        , '\r' <$ char 'r'
        , '\"' <$ char '\"'
        , '\'' <$ char '\''
        , '\\' <$ char '\\'
        , between (string "u{") (string "}") hexnum
        ]

    hexnum :: Parser Char
    hexnum = do
        raw <- concat <$> some (replicateM 2 hexdigit)
        case readHex raw of
            [(n,"")] -> return (chr n)
            _ -> fail $ "Invalid hex string " ++ show raw

hexdigit :: Parser Char
hexdigit = oneOf "0123456789ABCDEFabcdef" <|> char '_' *> hexdigit -- slightly too liberal: allows leading _

seqP :: Parser [Type TypeName]
seqP = parenComma argTypeP

argTypeP :: Parser (Type TypeName)
argTypeP = (try (nameP *> s ":") *> dataTypeP) <|> dataTypeP

dataTypeP :: Parser (Type TypeName)
dataTypeP = primTypeP <|> constTypeP <|> refTypeP <|> (RefT <$> idP)-- TODO: reftypes

primTypeP :: Parser (Type TypeName)
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

constTypeP :: Parser (Type TypeName)
constTypeP = choice
  [ OptT <$ k "opt" <*> dataTypeP
  , VecT <$ k "vec" <*> dataTypeP
  , RecT . resolveShorthand <$ k "record" <*> braceSemi recordFieldTypeP
  , VariantT <$ k "variant" <*> braceSemi variantFieldTypeP
  ]

refTypeP :: Parser (Type TypeName)
refTypeP = choice
    [ FuncT <$ k "func" <*> funcTypeP
    , ServiceT <$ k "service" <*> actorTypeP
    ]

fieldLabelP :: Parser FieldName
fieldLabelP  =
    hashedField . fromIntegral <$> natP <|>
    labledField <$> nameP

variantFieldTypeP :: Parser (FieldName, Type TypeName)
variantFieldTypeP =
  (,) <$> fieldLabelP <*> ((s ":" *> dataTypeP) <|> pure NullT)

resolveShorthand :: [Word32 -> (FieldName, a)] -> [(FieldName, a)]
resolveShorthand = go 0
  where
    go _ [] = []
    go n (f:fs) =
        let f' = f n in
        f' : go (succ (fieldHash (fst f'))) fs

recordFieldTypeP :: Parser (Word32 -> (FieldName, Type TypeName))
recordFieldTypeP = choice
  [ try $ do
    l <- fieldLabelP
    s ":"
    t <- dataTypeP
    return $ const (l,t)
  , do
    t <- dataTypeP
    return $ \next -> (hashedField next, t)
  ]

idP :: Parser T.Text
idP = T.pack <$> l ((:)
  <$> satisfy (\c -> isAscii c && isLetter c || c == '_')
  <*> many (satisfy (\c -> isAscii c && isAlphaNum c || c == '_'))
  ) <?> "id"

valuesP :: Parser [Value]
valuesP = (parenComma annValueP <?> "argument sequence")
       <|> ((:[]) <$> annValueP) -- for convenience

annValueP :: Parser Value
annValueP =
  parens annValueP <|> do -- this parser allows extra parentheses
      v <- valueP
      s ":" *> do
            t <- dataTypeP
            smartAnnV v t
       <|> return v

smartAnnV :: Value -> Type TypeName -> Parser Value
smartAnnV (NumV n) Nat8T = Nat8V <$> toBounded n
smartAnnV (NumV n) Nat16T = Nat16V <$> toBounded n
smartAnnV (NumV n) Nat32T = Nat32V <$> toBounded n
smartAnnV (NumV n) Nat64T = Nat64V <$> toBounded n
smartAnnV (NumV n) Int8T = Int8V <$> toBounded n
smartAnnV (NumV n) Int16T = Int16V <$> toBounded n
smartAnnV (NumV n) Int32T = Int32V <$> toBounded n
smartAnnV (NumV n) Int64T = Int64V <$> toBounded n
smartAnnV (NumV n) Float32T = return $ Float32V $ toRealFloat n
smartAnnV (NumV n) Float64T = return $ Float64V $ toRealFloat n
smartAnnV v ReservedT = return $ AnnV v ReservedT
smartAnnV _ _ = fail "Annotations are only supported around number literals"

toBounded :: (Integral a, Bounded a) => Scientific -> Parser a
toBounded v = maybe err return $ toBoundedInteger v
  where err = fail $ "Number literal out of bounds: " ++ show v

numP :: Parser Scientific
numP = l p >>= conv <?> "number"
  where
    p =(:) <$> oneOf "-+0123456789" <*> many (oneOf "-+.0123456789eE_")
    conv raw = case readMaybe (filter (/= '_') (handle_trailing_perdiod raw)) of
        Nothing -> fail $ "Invald number literal: " ++ show raw
        Just s -> return s
    -- 1. is allowed by candid, but not by scientific
    handle_trailing_perdiod s =
        if not (null s) && last s == '.' then s ++ "0" else s

valueP :: Parser Value
valueP = choice
  [ parens annValueP
  , NumV <$> numP
  , BoolV True <$ k "true"
  , BoolV False <$ k "false"
  , TextV <$> textP
  , NullV <$ k "null"
  , OptV . Just <$ k "opt" <*> valueP
  , VecV . V.fromList <$ k "vec" <*> braceSemi annValueP
  , RecV . resolveShorthand <$ k "record" <*> braceSemi recordFieldValP
  , uncurry VariantV <$ k "variant" <*> braces variantFieldValP
  , FuncV <$ k "func" <*> withPredicate parsePrincipal textP <* s "." <*> nameP
  , ServiceV <$ k "service" <*> withPredicate parsePrincipal textP
  , PrincipalV <$ k "principal" <*> withPredicate parsePrincipal textP
  , BlobV <$ k "blob" <*> blobP
  ]

variantFieldValP :: Parser (FieldName, Value)
variantFieldValP = (,) <$> fieldLabelP <*> ((s "=" *> annValueP) <|> pure NullV)

recordFieldValP :: Parser (Word32 -> (FieldName, Value))
recordFieldValP = choice
  [ try $ do
    l <- fieldLabelP
    s "="
    v <- annValueP
    return $ const (l,v)
  , do
    v <- annValueP
    return $ \next -> (hashedField next, v)
  ]

-- A lexeme
l :: Parser a -> Parser a
l x = x <* theVoid

-- The space between a lexeme
theVoid :: Parser ()
theVoid = void $ many (space1 <|> comment)

comment :: Parser ()
comment = lineComment <|> multiLineComment

-- a parser for nested multi-line comments. there might be a nicer way
multiLineComment :: Parser ()
multiLineComment = between (string "/*") (string "*/") $
    void $ many $
        multiLineComment <|>
        try (try $ char '*' *> notFollowedBy (char '/')) <|>
        void (anySingleBut '*')

lineComment :: Parser ()
lineComment = do
    void (string "//")
    void (takeWhileP (Just "character") (/= '\n'))
    void (char '\n')

-- a symbol
s :: String -> Parser ()
s str = void (l (string str)) <?> str

-- a keyword
k :: String -> Parser ()
k str = try (void (l (string str <* no)) <?> str)
  where
    no = notFollowedBy (satisfy (\c -> isAscii c && isAlphaNum c || c == '_'))

natP :: Parser Natural
natP = l (read <$> some digitChar <?> "number")

braces :: Parser a -> Parser a
braces = between (s "{") (s "}")
braceSemi :: Parser a -> Parser [a]
braceSemi p = braces $ sepEndBy p (s ";")
parens :: Parser a -> Parser a
parens = between (s "(") (s ")")
parenComma :: Parser a -> Parser [a]
parenComma p = parens $ sepEndBy p (s ",")


-- from https://markkarpov.com/tutorial/megaparsec.html#parse-errors
withPredicate :: (a -> Either String b) -> Parser a -> Parser b
withPredicate f p = do
  o <- getOffset
  r <- p
  case f r of
    Left msg -> parseError (FancyError o (Set.singleton (ErrorFail msg)))
    Right x -> return x


-- | A candid test file
--
-- (no support for type definitions yet)
data CandidTestFile = CandidTestFile
    { testDefs :: [ DidDef TypeName ]
    , testTests ::  [ CandidTest TypeName ]
    }

data CandidTest a = CandidTest
    { testLine :: Int
    , testAssertion :: TestAssertion
    , testType :: [Type a]
    , testDesc :: Maybe T.Text
    }
  deriving (Functor, Foldable, Traversable)

data TestInput
    = FromTextual T.Text
    | FromBinary BS.ByteString

data TestAssertion
    = CanParse TestInput
    | CannotParse TestInput
    | ParseEq Bool TestInput TestInput

-- | Parses a candid spec test file from a string
parseCandidTests :: String -> String -> Either String CandidTestFile
parseCandidTests source = first errorBundlePretty . parse (allInput testFileP) source

testFileP :: Parser CandidTestFile
testFileP = CandidTestFile <$> defsP <*> sepEndBy testP (s ";")

testP :: Parser (CandidTest TypeName)
testP = CandidTest
    <$> (unPos . sourceLine <$> getSourcePos)
    <*  k "assert"
    <*> testAssertP
    <*> seqP
    <*> optional textP

testAssertP :: Parser TestAssertion
testAssertP = do
    input1 <- testInputP
    choice
        [ CanParse input1 <$ s ":"
        , CannotParse input1 <$ s "!:"
        , ParseEq True input1 <$ s "==" <*> testInputP <* s ":"
        , ParseEq False input1 <$ s "!=" <*> testInputP <* s ":"
        ]

testInputP :: Parser TestInput
testInputP = FromTextual <$> textP <|> FromBinary <$> (k "blob" *> blobP)
