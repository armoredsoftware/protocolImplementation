{-# LANGUAGE RecordWildCards #-}

module ProtoMonad where

import ArmoredTypes
import VChanUtil
import CommTools(sendG')

import Prelude hiding (lookup)
import Data.Map hiding (foldl)
import qualified Control.Monad.Trans.Reader as T
import Control.Monad.Error --import Control.Monad.Except
import qualified Control.Monad.Trans.Error as ET
import Control.Monad

type Proto = T.ReaderT ProtoEnv (ErrorT String IO)

runProto :: (Proto a) -> ProtoEnv ->  IO (Either String a)
runProto proto env = ET.runErrorT $ T.runReaderT proto env

runWithLinks :: [(Int, Int)] -> (Proto a) -> Proto a
runWithLinks links proto = T.local (linkEnv links) proto


data ProtoEnv = ProtoEnv {
  me :: EntityId,
  myPriKey :: PrivateKey,
  entities :: Map EntityId EntityInfo,
  publicKeys :: Map EntityId PublicKey,
  --privateKeys :: Map Int PrivateKey,
  packScheme :: Int,
  encScheme :: Int,
  signScheme :: Int,
  protoId :: Int
}

protoIs :: Proto Int
protoIs = do
  id <- T.asks protoId
  return id

getEntityChannel :: EntityId -> Proto Channel
getEntityChannel id = do
  eInfo <- getEntityInfo id
  return $ chan eInfo

getEntityInfo :: EntityId -> Proto EntityInfo
getEntityInfo i = do
  infos <- T.asks entities
  let maybeInfo = lookup i infos
  case maybeInfo of
    Nothing -> throwError ("No known EntityInfo for Entity with id: "
                           ++ (show i) )
    Just info -> return info

getEntityPubKey :: EntityId -> Proto PublicKey
getEntityPubKey i = do
  pubKeys <- T.asks publicKeys
  let maybePubKey = lookup i pubKeys
  case maybePubKey of
    Nothing -> throwError ("No known PublicKey for Entity with id: "
                           ++ (show i) )
    Just pubKey -> return pubKey

linkEnv :: [(Int, Int)] -> ProtoEnv -> {-Proto-} ProtoEnv
linkEnv links oldEnv= let
  --oldEnv <- T.ask
  newEnv = foldl linkEnv' oldEnv links in
  newEnv
  --return newEnv

linkEnv' :: ProtoEnv -> (Int, Int) -> {-Proto-} ProtoEnv
linkEnv' ProtoEnv{..} (targetId, currentId) =
  let
    maybeInfo = lookup currentId entities
    eInfo = case maybeInfo of
        Nothing -> error $ "No entity with id: " ++ (show currentId) ++ " defined in current environment"
        Just e -> e{-<- case maybeInfo of
    Nothing -> throwError $ "No entity with id: " ++ (show currentId) ++ " defined in current environment"
    Just e -> return e -}

    maybePubKey = lookup currentId publicKeys
    pubKey = case maybePubKey of
      Nothing -> error $ "No pubKey for entity: " ++ (show currentId) ++ "defined in current environment"
      Just p -> p

    newEntities = insert targetId eInfo entities
    newPubKeys = insert targetId pubKey publicKeys in
  ProtoEnv{entities = newEntities, publicKeys = newPubKeys, ..}
