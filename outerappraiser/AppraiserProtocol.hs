{-# LANGUAGE OverloadedStrings #-}

module AppraiserProtocol where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Protocol
import Network.Http.Client
import ArmoredTypes
import Data.Aeson
import Control.Concurrent.STM.TMVar
import CommunicationNegotiator
import Control.Concurrent
import Control.Monad
import Control.Monad.State.Strict
import Web.Scotty hiding (get, put)
import qualified Web.Scotty as Scotty
import qualified ArmoredTypes as AD
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import CommTools (whoAmI)
import qualified Appraisal   as AppSubProto (appmain')

import DefaultComm
import CommunicationMonad
appraiseReqPort = 55555	      
{-appraise = do
	    putStrLn "Appraise be to Attester"
	    let knownguys = [att,pCA]
	    let emptyvars = []
	    emptyTMVarChans <- newTMVarIO []
            t <- newEmptyMVar
	    me <- whoAmI Appraiser -- app
            --putStrLn $ "This is who I am!!: " ++ (show me)
            --putStrLn $ "This is who I used to be: " ++ (show app)
	    let s0 = ArmoredState emptyvars me knownguys [] Nothing t emptyTMVarChans
	    forkIO ( do 
	    		runStateT negotiator s0
	    		return ()
	    	   )
	    --threadDelay 1000000
	    --runExecute' myProto s0
            awaitAppraisalReq s0
-}
appraise = awaitAppraisalReq
awaitAppraisalReq :: IO ()
awaitAppraisalReq = do
  Scotty.scotty (fromIntegral appraiseReqPort) $ do
	    Scotty.get "/" $ Scotty.text "serving\n"  --a quick way to check if is 
	    				 	--serving, client should see "serving" 
	    				 	

	    Scotty.post "/" $ do
              liftIO $ putStrLn ("IN POST")
	      --reads in "potential" request (parsing it could fail). 
	      --Note: no en/de-crypting is yet taking place.
	      a <- (param "request") :: ActionM LazyText.Text  
	     -- myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
	     -- myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
	     -- myprint' ("Data received on port: " ++ (show port)) 1
	      liftIO $ putStrLn ("thingy received: " ++ (show a))
	      --first converts the Text to UTF8, then then attempts to read a CARequest
	      let jj = eitherDecode (LazyEncoding.encodeUtf8 a) :: Either String FormalRequest
	      case jj of 
                (Left err) -> text (LazyText.pack ("ERROR Improper Request: " ++ err))
                (Right (FormalRequest target nreq)) -> do 
                   case entityIp target of
                     Nothing -> do
                       let str = "Error. No IP given for the target. Currently this is an error due to negotiation channel being HttpTuna."
                       liftIO $ putStrLn str
                       liftIO $ logf str
                       text (LazyText.pack str)
                     Just ipaddr -> do
                       eitherUnit <- liftIO $ talkTo ipaddr (myProto' nreq)
                       case eitherUnit of
                         Left err -> do
                           liftIO $ putStrLn err
                           liftIO $ logf err
                           text (LazyText.pack err)
                         Right () -> do
                           let str = "Successfully reached the end of appraisal"
                           liftIO $ putStrLn str
                           liftIO $ logf str
                           text (LazyText.pack str)
                  --let mvar = getInternalStateMVar s
                  --maybeE <- liftIO $ tryTakeMVar mvar
                  --liftIO $ putStrLn $ "Target is: " ++ (show target)
                  --liftIO $ putMVar mvar (AppState target (convertNReq nreq))
                  --(proc,armoredstate) <- liftIO $ runExecute' myProto s
                  --text (LazyText.pack (show proc))
                  --return ()
                  

myProto =    -- CreateChannel (AChannel "attesterChan") Target	      
 	      Send ANRequest (AChannel "attesterChan")
	    $ Receive (Var "counterOffer") (AChannel "attesterChan")
            $ CalculateFinalRequest (Var "finalReq") ANRequest (Var "counterOffer")
            $ Send (Var "finalReq") (AChannel "attesterChan")
            $ Receive (Var "finalConfirmation") (AChannel "attesterChan")
            $ Case (Var "finalConfirmation") [(Var "finalReq")]
                 (HandleFinalChoice (Var "result") (Var "finalReq")
                 (Result (Var "result")))
                 
                 (Stuck "finalConfirmation and finalReq did not match!!")

myProto' :: NRequest -> Converse ()
myProto' nreq = do
  send nreq
  (CounterOffer ls) <- receive :: Converse NResponse
  let finalItemPropPairs = foldr (\((i,p),mNreq) acc -> case mNreq of
                                       Nothing -> (i,p):acc -- then we get for free!!
                                       Just r  -> case r `liesin` privacyPolicy of
                                                   [] -> acc --my privacy policy says I won't give you that.
                                                   a@_ -> (i,p):acc) [] ls
      finalReq = pairsToN1Req finalItemPropPairs
      str = "calculated final Request: " ++ (show finalReq)
  send finalReq
  finalConfirmation <- receive :: Converse NRequest
  if finalConfirmation == finalReq then
    case finalConfirmation of
      (ReqLS []) -> do
        let str = "Could not come to an agreement. No attestation to take place."
        liftIO $ putStrLn str
        liftIO $ logf str
        return ()
      (RequestItem ProtocolItem (IntProperty i)) -> do
        let str = "Negotiation complete. About to perform Appraiser sub protocol for: " ++ (show nreq)
        liftIO $ putStrLn str
        liftIO $ logf str
        chan <- getChannel 
        subResult <- liftIO $ AppSubProto.appmain' i chan
        liftIO $ print subResult
      x@_ -> do
        let str = "The result of negotiation is currently unimplemented. The result was: " ++ (show x)
        liftIO $ putStrLn str
        liftIO $ logf str
        return ()
  else do
    let str = "FINALCONFIRMATION AND FINALREQUEST DID NOT MATCH"
    liftIO $ putStrLn str
    liftIO $ logf str
    return ()
privacyPolicy = [] --give nothing to no one	   
	  -- -}
{-

{"NRequest":{"Num":3,"ProtoNum":"NRequest"},"Entity":{"EntityName":"Attester","EntityRole":"Attester","EntityIp":"MTAuMTAwLjAuMjI5","EntityNote":"Just an attestered here to do your bidding","EntityId":null}}

-}
