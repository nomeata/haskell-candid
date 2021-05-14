{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Foldable
import Data.Traversable
import Data.List
import Data.Graph (stronglyConnComp, SCC(..))
import Control.Monad
import qualified Data.ByteString.Lazy as BS

import qualified Language.Haskell.TH.Syntax as TH (Name)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (Q, lookupTypeName, newName, Dec)

import Codec.Candid.Parse
import Codec.Candid.Data
import Codec.Candid.Tuples
import Codec.Candid.Types
import Codec.Candid.FieldName
import Codec.Candid.Class (Candid)

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

-- | Turns all candid type definitions into newtypes
-- Used, so far, only in the Candid test suite runner
generateCandidDefs :: [DidDef TypeName] -> Q ([Dec], TypeName -> Q TH.Name)
generateCandidDefs defs = do
    assocs <- for defs $ \(tn, _) -> do
        thn <- newName ("Candid_" ++ T.unpack tn)
        return (tn, thn)

    let m = M.fromList assocs
    let resolve tn = case M.lookup tn m of
            Just thn -> return thn
            Nothing -> fail $ "Could not find type " ++ T.unpack tn
    decls <- for defs $ \(tn, t) -> do
          t' <- traverse resolve t
          n <- resolve tn
          dn <- newName ("Candid_" ++ T.unpack tn)
          newtypeD (cxt []) n [] Nothing
            (normalC dn [bangType (bang noSourceUnpackedness noSourceStrictness) (typ t')])
            [derivClause Nothing [conT ''Candid, conT ''Eq]]
    return (decls, resolve)

-- | Inlines all candid type definitions, after checking for loops
inlineDefs :: forall k.  (Show k, Ord k) => [DidDef k] -> Q (k -> Q (), k -> Type Void)
inlineDefs defs = do
    for_ sccs $ \scc ->
        fail $ "Cyclic type definitions not supported: " ++ intercalate ", " (map show scc)
    for_ defs $ \(_, t) -> for_ t checkKey
    return (checkKey, f)
  where
    sccs = [ tns | CyclicSCC tns <-
        stronglyConnComp [ (tn, tn, toList t) | (tn, t) <- defs ] ]
    f :: k -> Type Void
    f k = m M.! k
    m :: M.Map k (Type Void)
    m = (>>= f) <$> M.fromList defs
    checkKey tn = unless (tn `M.member` m) $ unboundErr tn
    unboundErr k = fail $ "Unbound type: " ++ show k


quoteCandidService :: String -> TypeQ
quoteCandidService s = case parseDid s of
  Left err -> fail err
  Right DidFile{ service = []} -> [t|R.Empty|]
  Right DidFile{ defs = ds, service = s} -> do
    Just m <- lookupTypeName "m"
    (check, inline) <- inlineDefs ds
    for_ s $ \m -> for_ m check
    foldl1 (\a b -> [t|$(a) R..+ $(b)|])
        [ [t|  $(litT (strTyLit (T.unpack methodName)))
               R..== ($(candidTypeQ params) -> $(varT m) $(candidTypeQ results)) |]
        | DidMethod{..} <- s
        , let params = map ((absurd <$>) . (>>= inline)) methodParams
        , let results = map ((absurd <$>) . (>>= inline)) methodResults
        ]

quoteCandidType :: String -> TypeQ
quoteCandidType s = case parseDidType s of
  Left err -> fail err
  Right t -> typ (err <$> t)
   where
     err s = error $ "Type name in stand-alone Candid type: " ++ T.unpack s

candidTypeQ :: [Type TH.Name] -> TypeQ
candidTypeQ [] = [t| () |]
candidTypeQ [NullT] = [t| Unary () |]
candidTypeQ [t@(RecT fs)] | isTuple fs = [t| Unary $(typ t) |]
candidTypeQ [t] = typ t
candidTypeQ ts = mkTupleT (map typ ts)


row :: TypeQ -> TypeQ -> TypeQ -> Fields TH.Name -> TypeQ
row eq add = foldr (\(fn, t) rest -> [t|
    $add ($eq $(fieldName fn) $(typ t)) $rest
  |])

mkTupleT :: [TypeQ] -> TypeQ
mkTupleT ts = foldl appT (tupleT (length ts)) ts

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
typ (RecT fs)
 | isTuple fs = mkTupleT (map (typ . snd) fs)
 | otherwise = [t| R.Rec $(row [t| (R..==) |] [t| (R..+) |] [t| R.Empty |] fs) |]
typ (VariantT fs) = [t| V.Var $(row [t| (V..==) |] [t| (V..+) |] [t| V.Empty |] fs) |]
typ (FuncT _ _) = [t| FuncRef |]
typ (ServiceT _) = [t| ServiceRef |]
typ (RefT v) = conT v

isTuple :: [(FieldName, b)] -> Bool
isTuple fs = length fs > 1 && and (zipWith (==) (map fst fs) (map hashedField [0..]))
