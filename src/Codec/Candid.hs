{-|

This module provides preliminary Haskell supprot for decoding and encoding the __Candid__ data format.

__Warning:__ The interface of this library is still in flux, as we are yet learning the best idioms around Candid and Haskell.

-}

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

-- ** Dynamic use

-- $dynamic

-- ** Missing features

{- |

* Reading or writing interface descriptions
* Service and function types
* Future types

-}

-- * Reference

-- ** Encoding and decoding

   encode
 , encodeBuilder
 , decode

-- ** Type classes

 , Candid(..)
 , CandidArg
 , CandidVal
 , seqDesc
 , SeqDesc
 , tieKnot
 , typeDesc

 -- ** Special types

 , Unary(..)
 , Principal(..)
 , Reserved(..)

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
 , Value(..)
 , Fields
 , FieldName(..)
 , candidHash
 , lookupField

-- ** Dynamic candid use

 , decodeVals
 , DidFile
 , parseDid


-- Convenience re-exports
-- not useful due to https://github.com/haskell/haddock/issues/698#issuecomment-632328837
-- , Generic

 ) where

import Codec.Candid.Data
import Codec.Candid.Types
import Codec.Candid.Tuples
import Codec.Candid.Class
import Codec.Candid.Generic
import Codec.Candid.Service
import Codec.Candid.Parse
import Codec.Candid.TH
import Codec.Candid.TypTable
import Codec.Candid.Decode

-- $setup
-- >>> :set -dppr-cols=200
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as T
-- >>> import Data.Void (Void)
-- >>> import Data.Text.Prettyprint.Doc (pretty)
-- >>> import qualified Data.ByteString.Char8 as BS
-- >>> :set -XScopedTypeVariables

{- $haskell_types

The easiest way is to use this library is to use the canonical Haskell types. Any type that is an instance of 'Candid' can be used:

>>> encode ([True, False], Just 100)
"DIDL\STXm~n|\STX\NUL\SOH\STX\SOH\NUL\SOH\228\NUL"
>>> decode (encode ([True, False], Just 100)) == Right ([True, False], Just 100)
True

Here, no type annotations are needed, the library can infer them from the types of the Haskell values. You can see the Candid types used using `typeDesc` and `seqDesc`:

(TODO: fix pretty-printing of a TypeDesc)

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

If you want to use your own types directly, you have to declare an instance of the 'Candid' type class. In this instance, you indicate a canonical Haskel type to describe how your type should serialize, and provide conversion functions to the corresponding 'AsCandid'.

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

Especially for Haskell record types, you can use magic involving generic types to create the 'Candid' instance automatically. The best way is using the @DerivingVia@ langauge extension,using the 'AsRecord' new type to indicate that this strategy should be used:

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
variant {Point; Rectangle : record {0 : float; 1 : float}; Sphere : float}
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
*** Exception: Failed reading: Expected magic bytes "DIDL"
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

>>> pretty <$> parseDid "service : { get : () -> (int); inc : (int) -> (); }"
Right [(get, (), (int)), (inc, (int), ())]

And you can even, using Template Haskell, turn this into a proper Haskell type. The 'candid' antiquotation produces a type, and expects a free type variable @m@ for the monad you want to use.

>>> :set -XQuasiQuotes
>>> import Data.Row.Internal
>>> type Counter m = [candid| service : { get : () -> (int); inc : (int) -> (); } |]
>>> :info Counter
type Counter (m :: * -> *) = 'R '[ "get" ':-> (() -> m Integer), "inc" ':-> (Integer -> m ())] :: Row *
...

You can then use this with 'toCandidService' to talk to a service.

If you want to read the description from a @.did@ file, you can use 'candidFile'.


-}

{- $dynamic

TODO

-}
