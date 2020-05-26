{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Codec.Candid.TH (candid, candidFile) where

import Data.Row.Internal
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

candid :: QuasiQuoter
candid = QuasiQuoter { quoteExp = err, quotePat = err, quoteDec = err, quoteType = quoteCandidType }
  where err _ = fail "[candid| … |] can only be used as a type"

candidFile :: QuasiQuoter
candidFile = quoteFile candid

quoteCandidType :: String -> TypeQ
quoteCandidType s = case parseDid s of
  Left err -> fail err
  Right s -> do
    Just m <- lookupTypeName "m"
    appT (promotedT 'R) $ typListT
        [ [t|  $(litT (strTyLit (T.unpack meth)) )
               ':-> ($(args ts1) -> $(varT m) $(args ts2)) |]
        | (meth, ts1, ts2) <- s
        ]

typListT :: [TypeQ] -> TypeQ
typListT = foldr (appT . appT promotedConsT) promotedNilT

args :: [Type] -> TypeQ
args [] = [t| () |]
args [t] = typ t
args ts = foldl appT (tupleT (length ts)) (map typ ts)


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
typ NullT = error "TODO" -- [t| 'NullT |]
typ ReservedT = error "TODO" -- [t| 'ReservedT |]
typ EmptyT = [t| Void |]
typ PrincipalT = error "TODO" -- [t| 'PrincipalT |]
typ BlobT = [t| BS.ByteString|]
typ (OptT t) = [t| Maybe $( typ t ) |]
typ (VecT t) = [t| V.Vector $( typ t ) |]
typ (RecT fs) = error "TODO" -- [t| Rec $( typListT $ map field fs ) |]
typ (VariantT fs) = error "TODO" -- [t| 'VariantT $( typListT $ map field fs ) |]
typ (OtherT_ _) = error "Unexpected OtherT"

field :: (FieldName, Type) -> TypeQ
field (N' s, t) = [t| '( 'N $(litT (strTyLit (T.unpack s))) , $(typ t) ) |]
field (N _, _) = error "N in term"
field (H' s, t) = [t| '( 'H $(litT (numTyLit (fromIntegral s))), $(typ t) ) |]
field (H _, _) = error "H in term"



