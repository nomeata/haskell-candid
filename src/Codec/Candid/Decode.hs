{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Codec.Candid.Decode where

import Numeric.Natural
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Data.List
import Data.Void
import Data.Serialize.LEB128.Lenient
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.IEEE754 as G

import Codec.Candid.Data
import Codec.Candid.TypTable
import Codec.Candid.Types
import Codec.Candid.FieldName

-- | Decode to value representation
decodeVals :: BS.ByteString -> Either String [Value]
decodeVals bytes = G.runGet go (BS.toStrict bytes)
  where
    go = do
        decodeMagic
        arg_tys <- decodeTypTable
        arg_tys <- either fail pure $ resolvePreServiceT arg_tys
        vs <- mapM decodeVal (tieKnot (voidEmptyTypes arg_tys))
        G.remaining >>= \case
            0 -> return vs
            n -> fail $ "Unexpected " ++ show n ++ " left-over bytes"

decodeVal :: Type Void -> G.Get Value
decodeVal BoolT = G.getWord8 >>= \case
    0 -> return $ BoolV False
    1 -> return $ BoolV True
    _ -> fail "Invalid boolean value"
decodeVal NatT = NatV <$> getLEB128
decodeVal Nat8T = Nat8V <$> G.getWord8
decodeVal Nat16T = Nat16V <$> G.getWord16le
decodeVal Nat32T = Nat32V <$> G.getWord32le
decodeVal Nat64T = Nat64V <$> G.getWord64le
decodeVal IntT = IntV <$> getSLEB128
decodeVal Int8T = Int8V <$> G.getInt8
decodeVal Int16T = Int16V <$> G.getInt16le
decodeVal Int32T = Int32V <$> G.getInt32le
decodeVal Int64T = Int64V <$> G.getInt64le
decodeVal Float32T = Float32V <$> G.getFloat32le
decodeVal Float64T = Float64V <$> G.getFloat64le
decodeVal TextT = TextV <$> decodeText
decodeVal NullT = return NullV
decodeVal ReservedT = return ReservedV
decodeVal (OptT t) = G.getWord8 >>= \case
    0 -> return $ OptV Nothing
    1 -> OptV . Just <$> decodeVal t
    _ -> fail "Invalid optional value"
decodeVal (VecT Nat8T) = BlobV <$> decodeBytes
decodeVal (VecT t) = do
    n <- getLEB128Int
    VecV . V.fromList <$> replicateM n (decodeVal t)
decodeVal (RecT fs)
    | isTuple   = TupV <$> mapM (\(_,t) -> decodeVal t) fs'
    | otherwise = RecV <$> mapM (\(fn, t) -> (fn,) <$> decodeVal t) fs'
  where
    fs' = sortOn fst fs
    isTuple = and $ zipWith (==) (map fst fs') (map hashedField [0..])
decodeVal (VariantT fs) = do
    i <- getLEB128Int
    unless (i < length fs) $ fail "variant index out of bound"
    let (fn, t) = fs' !! i
    VariantV fn <$> decodeVal t
  where
    fs' = sortOn fst fs
decodeVal (FuncT _ _) = do
    referenceByte
    referenceByte
    FuncV <$> decodePrincipal <*> decodeText
decodeVal (PreServiceT _) = error "PreServiceT"
decodeVal (ServiceT _) = do
    referenceByte
    ServiceV <$> decodePrincipal
decodeVal PrincipalT = do
    referenceByte
    PrincipalV <$> decodePrincipal

decodeVal BlobT = error "shorthand encountered while decoding"
decodeVal EmptyT = fail "Empty value"
decodeVal (RefT v) = absurd v

referenceByte :: G.Get ()
referenceByte = G.getWord8 >>= \case
    0 -> fail "reference encountered"
    1 -> return ()
    _ -> fail "invalid reference tag"

decodeBytes :: G.Get BS.ByteString
decodeBytes = getLEB128Int >>= G.getLazyByteString

decodeText :: G.Get T.Text
decodeText = do
    bs <- decodeBytes
    case T.decodeUtf8' (BS.toStrict bs) of
        Left err -> fail $ "Invalid utf8: " ++ show err
        Right t -> return t

decodePrincipal :: G.Get Principal
decodePrincipal = Principal <$> decodeBytes

decodeMagic :: G.Get ()
decodeMagic = do
    magic <- G.getBytes 4
    unless (magic == T.encodeUtf8 (T.pack "DIDL")) $
        fail $ "Expected magic bytes \"DIDL\", got " ++ show magic

getLEB128Int :: Integral a => G.Get a
getLEB128Int = fromIntegral <$> getLEB128 @Natural

-- eagerly detect overshoot
checkOvershoot :: Natural -> G.Get ()
checkOvershoot n = void (G.lookAhead $ G.ensure $ fromIntegral n)

decodeSeq :: G.Get a -> G.Get [a]
decodeSeq act = do
    len <- getLEB128Int
    checkOvershoot (fromIntegral len)
    replicateM len act

decodeTypTable :: G.Get SeqDesc
decodeTypTable = do
    len <- getLEB128
    checkOvershoot len
    table <- replicateM (fromIntegral len) (decodeTypTableEntry len)
    ts <- decodeSeq (decodeTypRef len)
    let m = M.fromList (zip [0..] table)
    return $ SeqDesc m ts

decodeTypTableEntry :: Natural -> G.Get (Type Int)
decodeTypTableEntry max = getSLEB128 @Integer >>= \case
    -18 -> OptT <$> decodeTypRef max
    -19 -> VecT <$> decodeTypRef max
    -20 -> RecT <$> decodeTypFields max
    -21 -> VariantT <$> decodeTypFields max
    -22 -> FuncT <$>
        decodeSeq (decodeTypRef max) <*>
        decodeSeq (decodeTypRef max) <*
        decodeSeq decodeFuncAnn
    -23 -> do
        m <- decodeSeq ((,) <$> decodeText <*> decodeFuncTypRef max)
        unless (isOrdered (map fst m)) $
            fail "Service methods not in strict order"
        return (PreServiceT m)
    _ -> fail "Unknown structural type"

decodeTypRef :: Natural -> G.Get (Type Int)
decodeTypRef max = do
    i <- getSLEB128
    when (i >= fromIntegral max) $ fail "Type reference out of range"
    if i < 0
    then case primTyp i of
        Just t -> return t
        Nothing -> fail  $ "Unknown prim typ " ++ show i
    else return $ RefT (fromIntegral i)

decodeFuncTypRef :: Natural -> G.Get Int
decodeFuncTypRef max = do
    i <- getSLEB128
    when (i >= fromIntegral max) $ fail "Type reference out of range"
    if i < 0
    then case primTyp i of
        Just _ -> fail "Primitive type as method type in service type"
        Nothing -> fail  $ "Unknown prim typ " ++ show i
    else return $ fromIntegral i

decodeFuncAnn :: G.Get ()
decodeFuncAnn = G.getWord8 >>= \case
    1 -> return ()
    2 -> return ()
    _ -> fail "invalid function annotation"


isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered [_] = True
isOrdered (x:y:xs) = x < y && isOrdered (y:xs)

decodeTypFields :: Natural -> G.Get (Fields Int)
decodeTypFields max = do
    fs <- decodeSeq (decodeTypField max)
    unless (isOrdered (map fst fs)) $
        fail "Fields not in strict order"
    return fs

decodeTypField :: Natural -> G.Get (FieldName, Type Int)
decodeTypField max = do
    h <- getLEB128
    t <- decodeTypRef max
    return (hashedField h, t)
