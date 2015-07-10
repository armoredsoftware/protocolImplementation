module AttesterMain where --AttesterMain where

import CAProtoMain (caEntity_Att)
import ProtoMonad
import ProtoTypesA
import ProtoActions
import VChanUtil
import TPMUtil
import Keys


import Prelude
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random

attCommInit :: [Int] -> IO ProtoEnv
attCommInit domidS = do
  ekPub <- takeInit --Taking ownership of TPM
  --exportEK exportEKFileName ekPub  -- <--This is for provisioning
  appChan <- server_init (domidS !! 0)
  caChan <- client_init (domidS !! 1)
  let myInfo = EntityInfo "Attester" 11 appChan
      appInfo = EntityInfo "Appraiser" 22 appChan
      caInfo = EntityInfo "Certificate Authority" 33 caChan
      mList = [(0, myInfo), (1, appInfo), (2, caInfo)]
      ents = M.fromList mList
  (appPub,myPri) <- generateArmoredKeyPair -- Currently not used
  --appPub <- getBPubKey
  --caPub <- getBPubKey
  let caPub = appPub --Not used
      pubs = M.fromList [(1,appPub), (2, caPub)]


  return $ ProtoEnv 0 myPri ents pubs 0 0 0 1


--main = attmain' [1, 4]

attmain' :: [Int] -> IO String
attmain' chans = do
  putStrLn "Main of entity Attestation"
  env <- attCommInit chans --[1, 4]--[appId, caId]
  eitherResult <- runProto caEntity_Att env
  let str = case eitherResult of
             Left s -> "Error occured: " ++ s
             Right _ ->"End of Attestation"
  putStrLn str
  return str
