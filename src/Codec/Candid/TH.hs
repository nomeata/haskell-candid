{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Codec.Candid.TH (candid, candidFile) where

import Data.Row.Internal
import qualified Data.Text as T

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

import Codec.Candid.Core
import Codec.Candid.Parse

candid :: QuasiQuoter
candid = QuasiQuoter { quoteExp = err, quotePat = err, quoteDec = err, quoteType = quoteCandidType }
  where err _ = fail "[candid| … |] can only be used as a type"

candidFile :: QuasiQuoter
candidFile = quoteFile candid

quoteCandidType :: String -> TypeQ
quoteCandidType s = case parseDid s of
  Left err -> fail err
  Right s -> appT (promotedT 'R) $ typListT
    [ [t|  $(litT (strTyLit (T.unpack m)) ) ':-> '($(args ts1), $(args ts2)) |]
    | (m, ts1, ts2) <- s
    ]

typListT :: [TypeQ] -> TypeQ
typListT = foldr (appT . appT promotedConsT) promotedNilT

args :: [Type] -> TypeQ
args = typListT . map typ


typ :: Type -> TypeQ
typ NatT = [t| 'NatT |]
typ Nat8T = [t| 'Nat8T |]
typ Nat16T = [t| 'Nat16T |]
typ Nat32T = [t| 'Nat32T |]
typ Nat64T = [t| 'Nat64T |]
typ IntT = [t| 'IntT |]
typ Int8T = [t| 'Int8T |]
typ Int16T = [t| 'Int16T |]
typ Int32T = [t| 'Int32T |]
typ Int64T = [t| 'Int64T |]
typ Float32T = [t| 'Float32T |]
typ Float64T = [t| 'Float64T |]
typ BoolT = [t| 'BoolT |]
typ TextT = [t| 'TextT |]
typ NullT = [t| 'NullT |]
typ ReservedT = [t| 'ReservedT |]
typ EmptyT = [t| 'EmptyT |]
typ PrincipalT = [t| 'PrincipalT |]
typ BlobT = [t| 'BlobT |]
typ (OptT t) = [t| 'OptT $( typ t ) |]
typ (VecT t) = [t| 'VecT $( typ t ) |]
typ (RecT fs) = [t| 'RecT $( typListT $ map field fs ) |]
typ (VariantT fs) = [t| 'VariantT $( typListT $ map field fs ) |]
typ (OtherT_ _) = error "Unexpected OtherT"

field :: (FieldName, Type) -> TypeQ
field (N' s, t) = [t| '( 'N $(litT (strTyLit (T.unpack s))) , $(typ t) ) |]
field (N _, _) = error "N in term"
field (H' s, t) = [t| '( 'H $(litT (numTyLit (fromIntegral s))), $(typ t) ) |]
field (H _, _) = error "H in term"



