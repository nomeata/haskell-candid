{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Codec.Candid.Types where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word
import Data.Int
import Numeric.Natural
import Control.Monad
import Data.Bifunctor
import Data.Void
import Data.Scientific
import Data.Char
import Numeric

import Data.Text.Prettyprint.Doc

import Codec.Candid.Data
import Codec.Candid.FieldName

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
tupT = RecT . zipWith (\n t -> (hashedField n, t)) [0..]

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
    pretty Float32T = "float32"
    pretty Float64T = "float64"
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
prettyFields in_variant fs = prettyBraceSemi $ map (prettyField in_variant) fs

prettyBraceSemi :: [Doc ann] -> Doc ann
prettyBraceSemi = braces . hsep . punctuate semi

prettyField :: Pretty a => Bool -> (FieldName, Type a) -> Doc ann
prettyField True (f, NullT) = pretty f
prettyField _ (f, t) = pretty f <+> colon <+> pretty t -- TODO: encode field names

data Value
  = NumV Scientific -- used when parsing at an unknown numeric type
  | NatV Natural
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
  | TupV [Value]
  | VariantV FieldName Value
  | PrincipalV Principal
  | BlobV BS.ByteString
  | AnnV Value (Type Void)
  deriving (Eq, Ord, Show)

instance Pretty Value where
  pretty (NumV v) = pretty (show v)
  pretty (NatV v) = pretty v
  pretty (IntV v) | v >= 0 = "+" <> pretty v
                  | otherwise = pretty v
  pretty (Nat8V v) = prettyAnn v Nat8T
  pretty (Nat16V v) = prettyAnn v Nat16T
  pretty (Nat32V v) = prettyAnn v Nat32T
  pretty (Nat64V v) = prettyAnn v Nat64T
  pretty (Int8V v) = prettyAnn v Int8T
  pretty (Int16V v) = prettyAnn v Int16T
  pretty (Int32V v) = prettyAnn v Int32T
  pretty (Int64V v) = prettyAnn v Int64T
  pretty (Float32V v) = prettyAnn v Float32T
  pretty (Float64V v) = prettyAnn v Float64T
  pretty (BoolV True) = "true"
  pretty (BoolV False) = "false"
  pretty (TextV v) = prettyText v
  pretty NullV = "null"
  pretty ReservedV = prettyAnn ("null"::T.Text) ReservedT
  pretty (PrincipalV b) = "service" <+> prettyText (prettyPrincipal b)
  pretty (BlobV b) = "blob" <+> prettyBlob b
  pretty (OptV Nothing) = pretty NullV
  pretty (OptV (Just v)) = "opt" <+> pretty v
  pretty (VecV vs) = "vec" <+> prettyBraceSemi (map pretty (V.toList vs))
  pretty (TupV vs) = "record" <+> prettyBraceSemi (map pretty vs)
  pretty (RecV vs) = "record" <+> prettyBraceSemi (map go vs)
    where go (fn, v) = pretty fn <+> "=" <+> pretty v
  pretty (VariantV f NullV) = "variant" <+> braces (pretty f)
  pretty (VariantV f v) = "variant" <+> braces (pretty f <+> "=" <+> pretty v)
  pretty (AnnV v t) = prettyAnn v t

  prettyList = encloseSep lparen rparen (comma <> space) . map pretty

prettyAnn :: Pretty a => a -> Type Void -> Doc ann
prettyAnn v t = parens $ pretty v <+> ":" <+> pretty t

prettyBlob :: BS.ByteString -> Doc ann
prettyBlob = dquotes . pretty . T.concat . map go . BS.unpack
  where
    go b | fromIntegral b == ord '\t' = "\\t"
    go b | fromIntegral b == ord '\n' = "\\n"
    go b | fromIntegral b == ord '\r' = "\\r"
    go b | fromIntegral b == ord '"'  = "\\\""
    go b | fromIntegral b == ord '\'' = "\\\'"
    go b | fromIntegral b == ord '\\' = "\\\\"
    go b | b >= 0x20 && b < 0x80 = T.singleton (chr (fromIntegral b))
    go b | b < 0x10 = "\\0" <> T.pack (showHex b "")
    go b = "\\0" <> T.pack (showHex b "")

prettyText :: T.Text -> Doc ann
prettyText = dquotes . pretty . T.concatMap go
  where
    go '\t' = "\\t"
    go '\n' = "\\n"
    go '\r' = "\\r"
    go '"'  = "\\\""
    go '\'' = "\\\'"
    go '\\' = "\\\\"
    go c | isControl c = "\\u{" <> T.pack (showHex (ord c) "") <> "}"
    go c = T.singleton c

tupV :: [Value] -> Value
tupV = RecV . zipWith (\n t -> (hashedField n, t)) [0..]


-- Put here because used for both decoding and encoding
primTyp :: Integer -> Maybe (Type a)
primTyp (-1)  = Just NullT
primTyp (-2)  = Just BoolT
primTyp (-3)  = Just NatT
primTyp (-4)  = Just IntT
primTyp (-5)  = Just Nat8T
primTyp (-6)  = Just Nat16T
primTyp (-7)  = Just Nat32T
primTyp (-8)  = Just Nat64T
primTyp (-9)  = Just Int8T
primTyp (-10) = Just Int16T
primTyp (-11) = Just Int32T
primTyp (-12) = Just Int64T
primTyp (-13) = Just Float32T
primTyp (-14) = Just Float64T
primTyp (-15) = Just TextT
primTyp (-16) = Just ReservedT
primTyp (-17) = Just EmptyT
primTyp (-24) = Just PrincipalT
primTyp _     = Nothing

