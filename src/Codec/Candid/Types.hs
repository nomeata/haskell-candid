{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Codec.Candid.Types where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Word
import Data.Int
import Data.Maybe
import Text.Read
import Numeric.Natural
import Control.Monad
import Data.Bifunctor

import Data.Text.Prettyprint.Doc

import Codec.Candid.Data

data Type a
    -- prim types
    = NatT | Nat8T | Nat16T | Nat32T | Nat64T
    | IntT | Int8T | Int16T | Int32T | Int64T
    | Float32T | Float64T
    | BoolT
    | TextT
    | NullT
    | ReservedT
    | EmptyT
    -- constructors
    | OptT (Type a)
    | VecT (Type a)
    | RecT (Fields a)
    | VariantT (Fields a)
    -- reference
    | PrincipalT
    -- short-hands
    | BlobT
      -- ^ a short-hand for 'VecT' 'Nat8T'
    -- for recursive types
    | RefT a -- ^ A reference to a named type
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

tupT :: [Type a] -> Type a
tupT = RecT . zipWith (\n t -> (escapeFieldHash n, t)) [0..]

instance Applicative Type where
    pure = RefT
    (<*>) = ap

instance Monad Type where
    return = pure
    NatT >>= _ = NatT
    Nat8T >>= _ = Nat8T
    Nat16T >>= _ = Nat16T
    Nat32T >>= _ = Nat32T
    Nat64T >>= _ = Nat64T
    IntT >>= _ = IntT
    Int8T >>= _ = Int8T
    Int16T >>= _ = Int16T
    Int32T >>= _ = Int32T
    Int64T >>= _ = Int64T
    Float32T >>= _ = Float32T
    Float64T >>= _ = Float64T
    BoolT >>= _ = BoolT
    TextT >>= _ = TextT
    NullT >>= _ = NullT
    ReservedT >>= _ = ReservedT
    EmptyT >>= _ = EmptyT
    BlobT >>= _ = BlobT
    PrincipalT >>= _ = PrincipalT
    OptT t >>= f = OptT (t >>= f)
    VecT t >>= f = VecT (t >>= f)
    RecT fs >>= f = RecT (map (second (>>= f)) fs)
    VariantT fs >>= f = VariantT (map (second (>>= f)) fs)
    RefT x >>= f = f x

type Fields a = [(FieldName, Type a)]

type Args a = [Type a]

instance Pretty a => Pretty (Type a) where
    pretty NatT = "nat"
    pretty Nat8T = "nat8"
    pretty Nat16T = "nat16"
    pretty Nat32T = "nat32"
    pretty Nat64T = "nat64"
    pretty IntT = "int"
    pretty Int8T = "int8"
    pretty Int16T = "int16"
    pretty Int32T = "int32"
    pretty Int64T = "int64"
    pretty Float32T = "float"
    pretty Float64T = "float"
    pretty BoolT = "bool"
    pretty TextT = "text"
    pretty NullT = "null"
    pretty ReservedT = "reserved"
    pretty EmptyT = "empty"
    pretty (OptT t) = "opt" <+> pretty t
    pretty (VecT t) = "vec" <+> pretty t
    pretty (RecT fs) = "record" <+> prettyFields False fs
    pretty (VariantT fs) = "variant" <+> prettyFields True fs
    pretty (RefT a) = pretty a
    pretty BlobT = "blob"
    pretty PrincipalT = "principal"

    prettyList = encloseSep lparen rparen (comma <> space) . map pretty

prettyFields :: Pretty a => Bool -> Fields a -> Doc ann
prettyFields in_variant fs = braces $ hsep $ punctuate semi $ map (prettyField in_variant) fs

prettyField :: Pretty a => Bool -> (FieldName, Type a) -> Doc ann
prettyField True (f, NullT) = pretty f
prettyField _ (f, t) = pretty f <+> colon <+> pretty t -- TODO: encode field names

data Value
  = NatV Natural
  | Nat8V Word8
  | Nat16V Word16
  | Nat32V Word32
  | Nat64V Word64
  | IntV Integer
  | Int8V Int8
  | Int16V Int16
  | Int32V Int32
  | Int64V Int64
  | Float32V Float
  | Float64V Double
  | BoolV Bool
  | TextV T.Text
  | NullV
  | ReservedV
  | OptV (Maybe Value)
  | VecV (V.Vector Value)
  | RecV [(FieldName, Value)]
  | VariantV FieldName Value
  | PrincipalV Principal
  | BlobV BS.ByteString
  deriving (Eq, Ord, Show)

instance Pretty Value where
  pretty _ = "TODO: pretty Value"

tupV :: [Value] -> Value
tupV = RecV . zipWith (\n t -> (escapeFieldHash n, t)) [0..]

newtype FieldName = N T.Text
  deriving (Eq, Ord, Show)

instance Pretty FieldName where
    pretty (N n) = dquotes (pretty n) -- TODO: Escape field names

hashFieldName :: FieldName -> Word32
hashFieldName f = either id candidHash $ unescapeFieldName f

candidHash :: T.Text -> Word32
candidHash s = BS.foldl (\h c -> (h * 223 + fromIntegral c)) 0 $ T.encodeUtf8 s

lookupField :: FieldName -> [(FieldName, a)] -> Maybe a
lookupField fn fs = listToMaybe
    [ x | (f, x) <- fs, hashFieldName f == hashFieldName fn ]

unescapeFieldName :: FieldName -> Either Word32 T.Text
unescapeFieldName (N n)
    | T.length n > 3
    , T.head n == '_'
    , T.last n == '_'
    , Just (n' :: Natural) <- readMaybe (T.unpack (T.drop 1 (T.dropEnd 1 n)))
    , n' <= fromIntegral (maxBound :: Word32)
    = Left (fromIntegral n')
    | T.last n == '_'
    = Right (T.drop 1 n)
    | otherwise = Right n

escapeFieldName :: T.Text -> FieldName
escapeFieldName n | T.last n == '_' = N $ n <> "_"
escapeFieldName n = N n

escapeFieldHash :: Word32 -> FieldName
escapeFieldHash n =  N $ "_" <> T.pack (show n) <> "_"
