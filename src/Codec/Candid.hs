-- Reexport and documentation

module Codec.Candid
 ( Type(..)
 , Other(Other)
 , Fields
 , FieldName(N, H)
 , Candid(..)
 , CandidArgs(..)
 , Unary(..)
 , encode
 , encodeBuilder
 , decode
 , Val
 , Fix(..)
 , Rec
 , Variant
 , Seq
 , KnownType
 , KnownArgs
 , KnownFields
 , types
 , AsRecord(..)
 , CandidVal(..)
 , CandidSeq(..)
 ) where

import Codec.Candid.Core
import Codec.Candid.Generic
import Codec.Candid.Wrappers

