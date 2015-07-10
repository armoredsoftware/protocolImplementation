{-# LANGUAGE OverloadedStrings #-}

module AppraiserProtocol where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Protocol
import ProtoTypes
import Network.Http.Client
import Demo3Shared hiding (Result)
import Control.Concurrent.STM.TMVar
import CommunicationNegotiator
import Control.Concurrent
import Control.Monad
import Control.Monad.State.Strict
import Web.Scotty hiding (get, put)
import qualified Web.Scotty as Scotty
import qualified Demo3Shared as AD
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import CommTools (whoAmI)
appraiseReqPort = 55555	      
appraise = do
            clearLogf
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
awaitAppraisalReq :: ArmoredState -> IO ()
awaitAppraisalReq s = do
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
	      let jj = AD.jsonEitherDecode (LazyEncoding.encodeUtf8 a) :: Either String FormalRequest
	      case jj of 
                (Left err) -> text (LazyText.pack ("ERROR Improper Request: " ++ err))
                (Right (FormalRequest target nreq)) -> do 
                  let mvar = getInternalStateMVar s
                  maybeE <- liftIO $ tryTakeMVar mvar
                  liftIO $ putStrLn $ "Target is: " ++ (show target)
                  liftIO $ putMVar mvar (AppState target (convertNReq nreq))
                  (proc,armoredstate) <- liftIO $ runExecute' myProto s
                  text (LazyText.pack (show proc))
                  return ()
                  

myProto =     CreateChannel (AChannel "attesterChan") Target	      
 	    $ Send ANRequest (AChannel "attesterChan")
	    $ Receive (Var "counterOffer") (AChannel "attesterChan")
            $ CalculateFinalRequest (Var "finalReq") ANRequest (Var "counterOffer")
            $ Send (Var "finalReq") (AChannel "attesterChan")
            $ Receive (Var "finalConfirmation") (AChannel "attesterChan")
            $ Case (Var "finalConfirmation") [(Var "finalReq")]
                 (HandleFinalChoice (Var "result") (Var "finalReq")
                 (Result (Var "result")))
                 
                 (Stuck "finalConfirmation and finalReq did not match!!")
                 
                 	     
	   
	  -- -}
{-

{"NRequest":{"Num":3,"ProtoNum":"NRequest"},"Entity":{"EntityName":"Attester","EntityRole":"Attester","EntityIp":"MTAuMTAwLjAuMjI5","EntityNote":"Just an attestered here to do your bidding","EntityId":null}}

-}
