{-|

This module provides preliminary Haskell supprot for decoding and encoding the __Candid__ data format. See <https://github.com/dfinity/candid/blob/master/spec/Candid.md> for the official Candid specification.

__Warning:__ The interface of this library is still in flux, as we are yet learning the best idioms around Candid and Haskell.

-}

{-# LANGUAGE CPP #-}

module Codec.Candid
 (
-- * Tutorial

{- |

Candid is inherently typed, so before encoding or decoding, you have to indicate the types to use. In most cases, you can use Haskell types for that:

-}

-- ** Haskell types

-- $haskell_types

-- ** Custom types

-- $own_type

-- ** Generic types

-- $generic

-- ** Candid services

-- $services

-- ** Importing Candid

-- $import
-- $import2
-- $import3

-- ** Dynamic use

-- $dynamic

-- ** Missing features

{- |

* Generating interface descriptions (.did files) from Haskell functions
* Future types
* Parsing the textual representation dynamically against an expected type

-}

-- * Reference

-- ** Encoding and decoding

   encode
 , encodeBuilder
 , decode

-- ** Type classes

 , Candid(..)
 , CandidRow
 , CandidArg
 , CandidVal
 , seqDesc
 , SeqDesc
 , tieKnot
 , typeDesc

 -- ** Special types

 , Unary(..)
 , Principal(..)
 , prettyPrincipal
 , parsePrincipal
 , Reserved(..)
 , FuncRef(..)
 , AnnTrue
 , AnnFalse
 , ServiceRef(..)

-- ** Generics

 , AsRecord(..)
 , AsVariant(..)

-- ** Candid services

 , CandidService
 , RawService
 , toCandidService
 , fromCandidService

-- ** Meta-programming

 , candid
 , candidFile
 , candidType

-- ** Types and values

 , Type(..)
 , MethodType(..)
 , Fields
 , FieldName
 , labledField
 , hashedField
 , fieldHash
 , escapeFieldName
 , unescapeFieldName
 , candidHash
 , Value(..)

-- ** Dynamic use

 , decodeVals
 , fromCandidVals
 , toCandidVals
 , encodeDynValues
 , encodeTextual
 , DidFile
 , parseDid
 , parseValue
 , parseValues

-- Convenience re-exports
-- not useful due to https://github.com/haskell/haddock/issues/698#issuecomment-632328837
-- , Generic

 ) where

import Codec.Candid.Data
import Codec.Candid.Types
import Codec.Candid.FieldName
import Codec.Candid.Tuples
import Codec.Candid.Class
import Codec.Candid.Generic
import Codec.Candid.Service
import Codec.Candid.Parse
import Codec.Candid.TH
import Codec.Candid.TypTable
import Codec.Candid.Decode
import Codec.Candid.Encode
import Codec.Candid.EncodeTextual

-- $setup
-- >>> :set -dppr-cols=200
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as T
-- >>> import Data.Void (Void)
-- >>> import Prettyprinter (pretty)
-- >>> import qualified Data.ByteString.Lazy.Char8 as BS
-- >>> :set -XScopedTypeVariables

{- $haskell_types

The easiest way is to use this library is to use the canonical Haskell types. Any type that is an instance of 'Candid' can be used:

>>> encode ([True, False], Just 100)
"DIDL\STXm~n|\STX\NUL\SOH\STX\SOH\NUL\SOH\228\NUL"
>>> decode (encode ([True, False], Just 100)) == Right ([True, False], Just 100)
True

Here, no type annotations are needed, the library can infer them from the types of the Haskell values. You can see the Candid types used using `typeDesc` and `seqDesc`:

>>> :type +d ([True, False], Just 100)
([True, False], Just 100) :: ([Bool], Maybe Integer)
>>> :set -XTypeApplications
>>> pretty (tieKnot (seqDesc @([Bool], Maybe Integer)))
(vec bool, opt int)

This library is integrated with the @row-types@ library, so you can use their
records directly:

>>> :set -XOverloadedLabels
>>> import Data.Row
>>> encode (#foo .== [True, False] .+ #bar .== Just 100)
"DIDL\ETXl\STX\211\227\170\STX\SOH\134\142\183\STX\STXn|m~\SOH\NUL\SOH\228\NUL\STX\SOH\NUL"
>>> :set -XDataKinds -XTypeOperators
>>> pretty (typeDesc @(Rec ("bar" .== Maybe Integer .+ "foo" .== [Bool])))
record {bar : opt int; foo : vec bool}

-}

{- $own_type

If you want to use your own types directly, you have to declare an instance of the 'Candid' type class. In this instance, you indicate a canonical Haskell type to describe how your type should serialize, and provide conversion functions to the corresponding 'AsCandid'.

>>> :set -XTypeFamilies
>>> newtype Age = Age Integer
>>> :{
instance Candid Age where
    type AsCandid Age = Integer
    toCandid (Age i) = i
    fromCandid = Age
:}

>>> encode (Age 42)
"DIDL\NUL\SOH|*"

This is more or less the only way to introduce recursive types:

>>> data Peano = N | S Peano deriving (Show, Eq)
>>> :{
instance Candid Peano where
    type AsCandid Peano = Maybe Peano
    toCandid N = Nothing
    toCandid (S p) = Just p
    fromCandid Nothing = N
    fromCandid (Just p) = S p
:}

>>> peano = S (S (S N))
>>> encode peano
"DIDL\SOHn\NUL\SOH\NUL\SOH\SOH\SOH\NUL"
-}

{- $generic

Especially for Haskell record types, you can use magic involving generic types to create the 'Candid' instance automatically. The best way is using the @DerivingVia@ langauge extension, using the 'AsRecord' newtype to indicate that this strategy should be used:

>>> :set -XDerivingVia -XDeriveGeneric -XUndecidableInstances
>>> import GHC.Generics (Generic)
>>> :{
data SimpleRecord = SimpleRecord { foo :: [Bool], bar :: Maybe Integer }
    deriving Generic
    deriving Candid via (AsRecord SimpleRecord)
:}

>>> pretty (typeDesc @SimpleRecord)
record {bar : opt int; foo : vec bool}
>>> encode (SimpleRecord { foo = [True, False], bar = Just 100 })
"DIDL\ETXl\STX\211\227\170\STX\SOH\134\142\183\STX\STXn|m~\SOH\NUL\SOH\228\NUL\STX\SOH\NUL"

Unfortunately, this feature requires @UndecidableInstances@.

This works for variants too:

>>> :{
data Shape = Point () | Sphere Double | Rectangle (Double, Double)
    deriving Generic
    deriving Candid via (AsVariant Shape)
:}

>>> pretty (typeDesc @Shape)
variant {Point; Rectangle : record {0 : float64; 1 : float64}; Sphere : float64}
>>> encode (Rectangle (100,100))
"DIDL\STXk\ETX\176\200\244\205\ENQ\DEL\143\232\190\218\v\SOH\173\198\172\140\SIrl\STX\NULr\SOHr\SOH\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NULY@\NUL\NUL\NUL\NUL\NUL\NULY@"

Because data constructors are capitalized in Haskell, you cannot derive enums or variants with lower-case names. Also, nullary data constructors are not supported by @row-types@, and thus here, even though they would nicely map onto variants with arguments of type '@null@.

-}

{- $services

Very likely you want to either implement or use whole Candid interfaces. In order to apply the encoding/decoding in one go, you can use 'fromCandidService' and 'toCandidService'. These convert between a raw service ('RawService', takes a method name and bytes, and return bytes), and a typed 'CandidService' (expressed as an 'Data.Row.Rec' record).

Let us create a simple service:

>>> :set -XOverloadedLabels
>>> import Data.Row
>>> import Data.Row.Internal
>>> import Data.IORef
>>> c <- newIORef 0
>>> let service = #get .== (\() -> readIORef c) .+ #inc .== (\d -> modifyIORef c (d +))
>>> service .! #get $ ()
0
>>> service .! #inc $ 5
>>> service .! #get $ ()
5

For convenience, we name its type

>>> :t service
service :: Rec ('R '[ "get" ':-> (() -> IO Integer), "inc" ':-> (Integer -> IO ())])
>>> :set -XTypeOperators -XDataKinds -XFlexibleContexts
>>> type Interface = 'R '[ "get" ':-> (() -> IO Integer), "inc" ':-> (Integer -> IO ())]

Now we can turn this into a raw service operating on bytes:

>>> let raw = fromCandidService (error . show) error service
>>> raw (T.pack "get") (BS.pack "DUDE")
*** Exception: Failed reading: Expected magic bytes "DIDL", got "DUDE"
...
>>> raw (T.pack "get") (BS.pack "DIDL\NUL\NUL")
"DIDL\NUL\SOH|\ENQ"
>>> raw (T.pack "inc") (BS.pack "DIDL\NUL\SOH|\ENQ")
"DIDL\NUL\NUL"
>>> service .! #get $ ()
10

And finally, we can turn this raw function  back into a typed interface:

>>> let service' :: Rec Interface = toCandidService error raw
>>> service .! #get $ ()
10
>>> service .! #inc $ 5
>>> service .! #get $ ()
15

In a real application you would more likely pass some networking code to 'toCandidService'.

-}

{- $import

In the example above, we wrote the type of the service in Haskell. But very
likely you want to talk to a service whose is given to you in the form of a
@.did@ files, like

> service : {
>   get : () -> (int);
>   inc : (int) -> ();
> }

You can parse such a description:

>>> either error pretty $ parseDid "service : { get : () -> (int); inc : (int) -> (); }"
service : {get : () -> (int); inc : (int) -> ();}

And you can even, using Template Haskell, turn this into a proper Haskell type. The 'candid' antiquotation produces a type, and expects a free type variable @m@ for the monad you want to use.

-}

#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
{- $import2
>>> :set -XQuasiQuotes
>>> import Data.Row.Internal
>>> type Counter m = [candid| service : { get : () -> (int); inc : (int) -> (); } |]
>>> :info Counter
type Counter :: (* -> *) -> Row *
type Counter m = ("get" .== (() -> m Integer)) .+ ("inc" .== (Integer -> m ())) :: Row *
...
-}
#else
{- $import2
>>> :set -XQuasiQuotes
>>> import Data.Row.Internal
>>> type Counter m = [candid| service : { get : () -> (int); inc : (int) -> (); } |]
>>> :info Counter
type Counter (m :: * -> *) = ("get" .== (() -> m Integer)) .+ ("inc" .== (Integer -> m ())) :: Row *
...
-}
#endif

{- $import3
You can then use this with 'toCandidService' to talk to a service.

If you want to read the description from a @.did@ file, you can use 'candidFile'.

If this encounters a Candid type definition, it will just inline them. This means that cyclic type definitions are not supported.


-}

{- $dynamic

Sometimes one needs to interact with Candid in a dynamic way, without static type information.

This library allows the parsing and pretty-printing of candid values. The binary value was copied from above:

>>> import Data.Row
>>> :set -XDataKinds -XTypeOperators
>>> let bytes = encode (#bar .== Just 100 .+ #foo .== [True,False])
>>> let Right (_typs, vs) = decodeVals bytes
>>> pretty vs
(record {bar = opt +100; foo = vec {true; false}})

If you know Candid well you might be surprised to see the fieldnames here, because the Candid binary format does actually transmit the field name, but only a hash. This library tries to invert this hash, trying to find the shortest field name consisting of lower case letters and underscores that is equivalent to it. It does not work always:

>>> let Right (_typs, vs) = decodeVals $ encode (#stopped .== True .+ #canister_id .== Principal (BS.pack []))
>>> pretty vs
(record {stopped = true; hymijyo = principal "aaaaa-aa"})

Future versions of this library will allow you to specify the (dynamic) 'Type' at which you want to decode these values, in which case the field name would be taken from there.

Conversely, you can encode from the textual representation:

>>> let Right bytes = encodeTextual "record { foo = vec { true; false }; bar = opt 100 }"
>>> bytes
"DIDL\ETXl\STX\211\227\170\STX\STX\134\142\183\STX\SOHm~n}\SOH\NUL\SOHd\STX\SOH\NUL"
>>> decode @(Rec ("bar" .== Maybe Integer .+ "foo" .== [Bool])) bytes
Right (#bar .== Just 100 .+ #foo .== [True,False])

This function does not support the full textual format yet; in particular type annotations can only be used around number literals.

-}

