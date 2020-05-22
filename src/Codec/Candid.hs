{-|

This module provides preliminary Haskell supprot for decoding and encoding the __Candid__ data format.

__Warning 1:__ The interface of this library is still in flux, as we are yet learning the best idioms around Candid and Haskell.

__Warning 2:__ The author couldn’t resist turning this into a little Haskell-type-wizardry playground, without having too much expertise in this area. Do not use it to learn best practices.

-}

module Codec.Candid
 (
-- * Overview

-- $overview

-- * Candid Types
   encodeT
 , encodeBuilderT
 , decodeT
 , Type(..)
 , OtherT
 , Fields
 , FieldName(N, H)

-- * Custom types

 , encode
 , encodeBuilder
 , decode
 , Candid(..)
 , CandidArg
 , Unary(..)

-- * Generic programming utilities

 , AsRecord(..)
 , CandidVal(..)

-- * Candid services

{- |
Very likely you want to either implement or use whole Candid interfaces. In order to apply the encoding/decoding in one go, you can use 'fromCandidService' and 'toCandidService'. These convert between a raw service ('RawService', takes a method name and bytes, and return bytes), and a typed 'CandidService' (expressed as an 'Data.Row.Rec' record).

Let us create a simple service:

>>> :set -XOverloadedLabels
>>> import Data.Row
>>> import Data.Row.Internal
>>> import Data.IORef
>>> c <- newIORef 0
>>> let service = #get .== (\() -> readIORef c) .+ #inc .== (\d -> modifyIORef c (d +))
>>> (service .! #get) ()
0
>>> (service .! #inc) 5
>>> (service .! #get) ()
5

For convenience, we name its type

>>> :t service
service
  :: Rec
       ('R
          '[ "get" ':-> (() -> IO Integer), "inc" ':-> (Integer -> IO ())])
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
>>> (service .! #get) ()
10

And finally, we can turn this raw function  back into a typed interface:

>>> let service' :: Rec Interface = toCandidService error raw
>>> (service .! #get) ()
10
>>> (service .! #inc) 5
>>> (service .! #get) ()
15

In a real application you would more likely pass some networking code to 'toCandidService'.


-}

 , CandidService
 , RawService
 , toCandidService
 , fromCandidService

-- Convenience re-exports
-- not useful due to https://github.com/haskell/haddock/issues/698#issuecomment-632328837
-- , Generic

-- * Mostly plumbing
--
-- | These exports are usually not interesting

 , Other(Other)
 , KnownType
 , KnownFields
 , KnownArgs
 , Val
 , Record
 , Variant
 , Seq
 , types
 , CandidMethod(..)
--
 ) where

import Codec.Candid.Core
import Codec.Candid.Generic
import Codec.Candid.Wrappers
import Codec.Candid.Service

-- $setup
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as T
-- >>> import Data.Void (Void)
-- >>> import Data.Text.Prettyprint.Doc (pretty)
-- >>> import qualified Data.ByteString.Char8 as BS
-- >>> :set -XScopedTypeVariables

{- $overview

Candid is inherently typed, so before encoding or decoding, we have to declare the Candid type to use. There are multiple ways of doing that:

== Using 'Type'

This is useful when you want to quickly target a specific Candid type that does not use recursion (and ideally also no records or variants).

The 'Type' type directly matches the types presented in the Candid specification. We can pass these types directly to the `encodeT` and `decodeT` functions, using `TypeApplication`.

>>> :set -XTypeApplications -XDataKinds -XOverloadedStrings
>>> encodeT @[BoolT, OptT TextT] (True, (Just "Hello", ()))
"DIDL\SOHnq\STX~\NUL\SOH\SOH\ENQHello"
>>> type MyType = [BoolT, OptT TextT]
>>> decodeT @MyType (encodeT @MyType (True, (Just "Hello", ())))
Right (True,(Just "Hello",()))

In the second line we used a normal type alias to give a name to this type.

As you can see the type of Haskell values you pass in depends on the Candid type description. This is implemented by the type families 'Val' and (for argument sequences) 'Seq':

>>> :kind! Seq [BoolT, OptT TextT]
Seq [BoolT, OptT TextT] :: *
= (Bool, (Maybe Text, ()))

This uses nested pairs to express Candid argument sequences.

Records and variants are represented a bit unwieldy, as nested pairs or 'Either's:

>>> :kind! Val (RecT '[ '(N "foo", BoolT), '(N "bar", OptT TextT)])
Val (RecT '[ '(N "foo", BoolT), '(N "bar", OptT TextT)]) :: *
= (Bool, (Maybe Text, ()))

>>> :kind! Val (VariantT '[ '(N "foo", BoolT), '(N "bar", OptT TextT)])
Val (VariantT '[ '(N "foo", BoolT), '(N "bar", OptT TextT)]) :: *
= Either Bool (Either (Maybe Text) Void)

This is not very convenient, so consider the following alternative ways.

== Using existing Haskell types

Instead of using Candid types ('Type') to declare the interface, you can use normal Haskell types. Any type that is an instance of 'Candid' can be used:

>>> encode ([True, False], Just 100)
"DIDL\STXm~n|\STX\NUL\SOH\STX\SOH\NUL\SOH\228\NUL"
>>> decode (encode ([True, False], Just 100)) == Right ([True, False], Just 100)
True

Here, no type annotations are needed, the library can infer them from the types of the Haskell values. The type families 'Rep' (for values) and 'ArgRep' (for argument sequences) indicate the corresponding Candid types:

>>> :type +d ([True, False], Just 100)
([True, False], Just 100) :: ([Bool], Maybe Integer)
>>> :kind! ArgRep ([Bool], Maybe Integer)
ArgRep ([Bool], Maybe Integer) :: [Type]
= '[ 'VecT 'BoolT, 'OptT 'IntT]
>>> pretty (types @(ArgRep ([Bool], Maybe Integer)))
(vec bool, opt int)

This library is integrated with the @row-types@ library, so you can use their
records directly:

>>> :set -XOverloadedLabels
>>> import Data.Row ((.==),(.+))
>>> encode (#foo .== [True, False] .+ #bar .== Just 100)
"DIDL\ETXl\STX\211\227\170\STX\SOH\134\142\183\STX\STXn|m~\SOH\NUL\SOH\228\NUL\STX\SOH\NUL"

== Using your own types

If you want to use your own types directly, you have to declare an instance of the 'Candid' type class. In this instance, you indicate which Candid 'Type' your type should serialize to, and provide conversion functions to the corresponding 'Val'.

>>> :set -XTypeFamilies
>>> newtype Age = Age Integer
>>> :{
instance Candid Age where
    type Rep Age = 'IntT
    toCandid (Age i) = i
    fromCandid = Age
:}

>>> encode (Age 42)
"DIDL\NUL\SOH|*"

This is more or less the only way to introduce recursive types. In order to refer to them inside the 'Rep' equation, you can wrap it in 'OtherT':

>>> newtype Peano = Peano (Maybe Peano) deriving (Show, Eq)
>>> type PeanoT = OtherT Peano
>>> :{
instance Candid Peano where
    type Rep Peano = 'OptT PeanoT
    toCandid (Peano x) = x
    fromCandid = Peano
:}

>>> peano = Peano $ Just $ Peano $ Just $ Peano $ Just $ Peano Nothing
>>> encode peano
"DIDL\SOHn\NUL\SOH\NUL\SOH\SOH\SOH\NUL"

== Using your own types, generically

Especially for Haskell record types, you can use magic involving generic types to create the 'Candid' instance automatically. The best way is using the @DerivingVia@ langauge extension,using the 'AsRecord' new type to indicate that this strategy should be used:

>>> :set -XDerivingVia -XDeriveGeneric -XUndecidableInstances
>>> import GHC.Generics (Generic)
>>> :{
data SimpleRecord = SimpleRecord { foo :: [Bool], bar :: Maybe Integer }
    deriving Generic
    deriving Candid via (AsRecord SimpleRecord)
:}

>>> encode (SimpleRecord { foo = [True, False], bar = Just 100 })
"DIDL\ETXl\STX\211\227\170\STX\SOH\134\142\183\STX\STXn|m~\SOH\NUL\SOH\228\NUL\STX\SOH\NUL"

Unfortunately, this feature requires @UndecidableInstances@.

= Missing features

* Reading or writing interface descriptions
* Service and function types
* Future types
* Generic variants
* Support for converting whole services (name sets of methods with argument and result types)
-}
