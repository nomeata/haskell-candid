-- | A model of (untyped) Candid values
module Codec.Candid.Value where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Word
import Data.Int
import Data.Void
import Numeric.Natural

import Codec.Candid.Data

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
  | RecV (M.Map FieldName Value)
  | VariantV FieldName Value
  | PrincipalV Principal
  | BlobV BS.ByteString
  deriving (Eq, Ord, Show)


data FieldName
   = N T.Text -- ^ A named field
   | H Word32 -- ^ A field number (also used for tuples)
  deriving (Eq, Ord, Show)
