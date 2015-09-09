{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module AttesterProtocol where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Protocol
import Network.Http.Client
import ArmoredTypes
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.State.Strict
import CommunicationNegotiator
import Control.Concurrent
import Data.List
import Control.Concurrent.STM
import CommTools (whoAmI, getMyIPString)
import MeasurerComm (getSocket)
import Network.Socket (Socket)
import ArmoredConfig.Environment (getPort)

import AbstractedCommunication hiding (send, receive)
import qualified Attestation as AttSubProto (attmain')
import CommunicationMonad
import DefaultComm
attest = do
  clearLogf
  putStrLn "Appraise be to Attester"
  let knownguys = [att,pCA]
  let emptyvars = []
  emptychans <- newTMVarIO []
          --s <- fancyGetSocket
  host <- getMyIPString
  port <- getPort
  debugPrint "BEFORE.. CONNECTING... TO. MEASURER. SOCKET!!!"
  s <- MeasurerComm.getSocket host {-"10.100.0.249"-} port

  


  debugPrint "AFTER CONNECTING TO MEASURER SOCKET!!!"
  declareDefaultComm' (myProto' s)
  --me <- whoAmI Attester
  --t <- newMVar (AttState me s)

  --let s0 = ArmoredState emptyvars me knownguys [] (Just c) t emptychans

  

{-
myProto =    --CreateChannel (AChannel "chan") Requester
	     Receive (Var "request") (AChannel "chan")
	   $ ComputeCounterOffer (Var "counterOffer") (Var "request")
	   $ Send (Var "counterOffer") (AChannel "chan")
           $ Receive (Var "theirFinalChoice") (AChannel "chan")
           $ CheckFinalChoice (Var "finalAgreement") (Var "theirFinalChoice")
           $ Send (Var "finalAgreement") (AChannel "chan")
           $ HandleFinalChoice (Var "result") (Var "finalAgreement")
	   $ Result (Var "result")
-}
myProto' :: Socket -> Converse ()
myProto' sock = do
  req <- receive :: Converse NRequest
  let req' = convertNReq req 
  liftIO $ putStrLn $ "calling: " ++ (show req') ++ "`liesin`" ++ (show privacyPol) ++ "~~~~~~~~~~~~~~~~~~~~~~~~~~"
  let nresp = createCounterOffer req' privacyPol -- :: NResponse
  liftIO $ putStrLn $ "Here is my counter offer: " ++ (show nresp)
  send nresp
  theirFinalChoice <- receive :: Converse NRequest
  let finalagreement = (case theirFinalChoice `completelyAbidesBy` privacyPol of
                      False -> ReqLS [] -- No
                      True  -> theirFinalChoice)
  send finalagreement
  case finalagreement of
    (ReqLS []) -> do
      let str = "Could not come to an agreement. No attestation to take place."
      liftIO $ putStrLn str
      liftIO$ logf str
      return ()
    (RequestItem ProtocolItem (IntProperty i)) -> do
      let str = "Negotiation complete. About to perform Attester sub protocol for: " ++ (show req)
      liftIO $ putStrLn str
      liftIO $ logf str
      chan <- getChannel
      subResult <- liftIO $ AttSubProto.attmain' i chan sock
      let str2 = "sub protocol complete."
      liftIO $ putStrLn str2
      liftIO $ logf str2
      liftIO $ print subResult
    x@_ -> do
      let str = "The result of negotiation is currently unimplemented. The result was: " ++ (show x)
      liftIO $ putStrLn str
      liftIO $ logf str
      return ()


privacyPol = [Reveal [(ProtocolItem, [IntProperty 1])] FREE,
                 Reveal [(ProtocolItem, [IntProperty 2])] FREE   ] --condOS
condFree = FREE
condOS = Equals OS Name (ValString "McAffee")
--	      -}

