{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Codec.Candid.TH (candid, candidFile, candidType) where

import Data.Row.Internal
import qualified Data.Row.Records as R
import qualified Data.Row.Variants as V
import qualified Data.Text as T
import qualified Data.Vector as V
import Numeric.Natural
import Data.Word
import Data.Int
import Data.Void
import qualified Data.ByteString as BS

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (lookupTypeName)

import Codec.Candid.Core
import Codec.Candid.Parse
import Codec.Candid.Data 

-- | This quasi-quoter turns a Candid description into a Haskell type. It assumes a type variable @m@ to be in scope.
candid :: QuasiQuoter
candid = QuasiQuoter { quoteExp = err, quotePat = err, quoteDec = err, quoteType = quoteCandidService }
  where err _ = fail "[candid| … |] can only be used as a type"

-- | As 'candid', but takes a filename
candidFile :: QuasiQuoter
candidFile = quoteFile candid

-- | This quasi-quoter turns works on individual candid types, e.g.
--
-- > type InstallMode = [candidType| variant {install : null; reinstall : null; upgrade : null}; |]
candidType :: QuasiQuoter
candidType = QuasiQuoter { quoteExp = err, quotePat = err, quoteDec = err, quoteType = quoteCandidType }
  where err _ = fail "[candid| … |] can only be used as a type"

quoteCandidService :: String -> TypeQ
quoteCandidService s = case parseDid s of
  Left err -> fail err
  Right s -> do
    Just m <- lookupTypeName "m"
    appT (promotedT 'R) $ typListT
        [ [t|  $(litT (strTyLit (T.unpack meth)) )
               ':-> ($(args ts1) -> $(varT m) $(args ts2)) |]
        | (meth, ts1, ts2) <- s
        ]

quoteCandidType :: String -> TypeQ
quoteCandidType s = case parseDidType s of
  Left err -> fail err
  Right t -> typ t

typListT :: [TypeQ] -> TypeQ
typListT = foldr (appT . appT promotedConsT) promotedNilT

args :: [Type] -> TypeQ
args [] = [t| () |]
args [t] = typ t
args ts = foldl appT (tupleT (length ts)) (map typ ts)


row :: TypeQ -> TypeQ -> TypeQ -> Fields -> TypeQ
row eq add = foldr (\(fn, t) rest -> [t|
    $add ($eq $(fieldName fn) $(typ t)) $rest
  |])

fieldName :: FieldName -> TypeQ
fieldName (N' s) = litT (strTyLit (T.unpack s))
fieldName (N _) = fail "N in term"
fieldName (H' _) = error "Cannot handle numeric record field names"
fieldName (H _) = fail "H in term"

typ :: Type -> TypeQ
typ NatT = [t| Natural |]
typ Nat8T = [t| Word8 |]
typ Nat16T = [t| Word16 |]
typ Nat32T = [t| Word32 |]
typ Nat64T = [t| Word64 |]
typ IntT = [t| Integer |]
typ Int8T = [t| Int8 |]
typ Int16T = [t| Int16 |]
typ Int32T = [t| Int32 |]
typ Int64T = [t| Int64 |]
typ Float32T = [t| Float |]
typ Float64T = [t| Double |]
typ BoolT = [t| Bool |]
typ TextT = [t| T.Text |]
typ NullT = [t| () |]
typ ReservedT = [t| Reserved |]
typ EmptyT = [t| Void |]
typ PrincipalT = [t| Principal |]
typ BlobT = [t| BS.ByteString|]
typ (OptT t) = [t| Maybe $( typ t ) |]
typ (VecT t) = [t| V.Vector $( typ t ) |]
typ (RecT fs) = [t| R.Rec $(row [t| (R..==) |] [t| (R..+) |] [t| R.Empty |] fs) |]
typ (VariantT fs) = [t| V.Var $(row [t| (V..==) |] [t| (V..+) |] [t| V.Empty |] fs) |]
typ (OtherT_ _) = error "Unexpected OtherT"
