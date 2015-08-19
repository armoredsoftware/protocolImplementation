module Attestation where --AttesterMain where

import CAProtoMain (caEntity_Att)
import ProtoMonad
import ArmoredTypes
import ProtoActions
import VChanUtil
import TPMUtil
import Keys
import MeasurerComm


import Prelude
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random
import Network.Socket

attCommInit :: Channel -> Int -> Socket -> IO ProtoEnv
attCommInit chan protoId sock {-domidS-} = do
  debugPrint "BEFORE INVOKING TPM!!!"
  ekPub <- takeInit --Taking ownership of TPM
  debugPrint "AFTER INVOKING TPM!!!!!"
  --exportEK exportEKFileName ekPub  -- <--This is for provisioning
  {-appChan <- server_init (domidS !! 0)
  caChan <- client_init (domidS !! 1)
  appChan <- server_init (chans !! 0)
  caChan <- client_init (chans !! 1) -}
  let appChan = chan
      caChan = chan
      myInfo = EntityInfo "Attester" 11 appChan
      appInfo = EntityInfo "Appraiser" 22 appChan
      caInfo = EntityInfo "Certificate Authority" 33 caChan
      mList = [(0, myInfo), (1, appInfo), (2, caInfo)]
      ents = M.fromList mList
  (appPub,myPri) <- generateArmoredKeyPair -- Currently not used
  --appPub <- getBPubKey
  --caPub <- getBPubKey
  let caPub = appPub --Not used
      pubs = M.fromList [(1,appPub), (2, caPub)]

  {-host <- getMyIPString
  port <- getPort
  sock <- MeasurerComm.getSocket host {-"10.100.0.249"-} port -}

  return $ ProtoEnv 0 myPri ents pubs 0 0 0 protoId (Just sock)


--main = attmain' [1, 4]

attmain' :: Int -> Channel -> Socket -> IO String
attmain' protoId chan sock = do
  putStrLn "Main of entity Attestation"
  env <- attCommInit chan protoId sock --[1, 4]--[appId, caId]
  eitherResult <- runProto caEntity_Att env
  let str = case eitherResult of
             Left s -> "Error occured: " ++ s
             Right _ ->"End of Attestation"
  putStrLn str
  return str
