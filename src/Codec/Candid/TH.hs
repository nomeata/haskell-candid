{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Codec.Candid.TH
 ( candid, candidFile, candidType, candidTypeQ
 , generateCandidDefs
 ) where

import qualified Data.Map as M
import qualified Data.Row.Records as R
import qualified Data.Row.Variants as V
import qualified Data.Text as T
import qualified Data.Vector as V
import Numeric.Natural
import Data.Word
import Data.Int
import Data.Void
import Data.Traversable
import qualified Data.ByteString.Lazy as BS

import qualified Language.Haskell.TH.Syntax as TH (Name)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (Q, lookupTypeName, newName)

import Codec.Candid.Parse
import Codec.Candid.Data
import Codec.Candid.Tuples
import Codec.Candid.Types
import Codec.Candid.FieldName

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

generateCandidDefs :: [DidDef TypeName] -> Q (DecsQ, TypeName -> Q TH.Name)
generateCandidDefs defs = do
    -- generate fresh names for each type types
    assocs <- for defs $ \(tn, _) -> do
        thn <- newName ("Candid_" ++ T.unpack tn)
        return (tn, thn)

    let m = M.fromList assocs
    let resolve tn =  case M.lookup tn m of
            Just thn -> return thn
            Nothing -> fail $ "Could not find type " ++ T.unpack tn
    let decls = for defs $ \(tn, t) -> do
          t' <- traverse resolve t
          n <- resolve tn
          tySynD n [] (typ t')
    return (decls, resolve)

quoteCandidService :: String -> TypeQ
quoteCandidService s = case parseDid s of
  Left err -> fail err
  Right DidFile{ defs = _:_} ->
    fail "Type definitions not supported yet"
  Right DidFile{ service = []} -> [t|R.Empty|]
  Right DidFile{ service = s} -> do
    Just m <- lookupTypeName "m"
    s' <- traverse (traverse (\n -> fail $ "Unbound type definition" ++ T.unpack n)) s
    foldl1 (\a b -> [t|$(a) R..+ $(b)|])
        [ [t|  $(litT (strTyLit (T.unpack methodName)))
               R..== ($(candidTypeQ methodParams) -> $(varT m) $(candidTypeQ methodResults)) |]
        | DidMethod{..} <- s'
        ]

quoteCandidType :: String -> TypeQ
quoteCandidType s = case parseDidType s of
  Left err -> fail err
  Right t -> typ (err <$> t)
   where
     err s = error $ "Type occurrences not supported: " ++ T.unpack s

candidTypeQ :: [Type TH.Name] -> TypeQ
candidTypeQ [] = [t| () |]
candidTypeQ [NullT] = [t| Unary () |]
candidTypeQ [t] = typ t
candidTypeQ ts = foldl appT (tupleT (length ts)) (map typ ts)


row :: TypeQ -> TypeQ -> TypeQ -> Fields TH.Name -> TypeQ
row eq add = foldr (\(fn, t) rest -> [t|
    $add ($eq $(fieldName fn) $(typ t)) $rest
  |])

fieldName :: FieldName -> TypeQ
fieldName f = litT (strTyLit (T.unpack (escapeFieldName f)))

typ :: Type TH.Name -> TypeQ
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
typ (RefT v) = varT v
