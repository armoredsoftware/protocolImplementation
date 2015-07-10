{-# LANGUAGE OverloadedStrings #-}

module ProtocolDefinitions where


import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Protocol
import ProtoTypes hiding (attAddress, pCAAddress)
import Network.Http.Client
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
	            	      
appProtocol = Let (Var "pcrsel") (ArmoredPCRSel [0..23]) 
	     (Let (Var "nonce") ArmoredCreateNonce 
	     (Let (Var "desiredEvidence") (ArmoredCreateDesiredEvidence [0..2])
	     (Let (Var "request") (ArmoredRequesetForAttest (Var "pcrsel") 
	     	  				            (Var "nonce")
	     					            (Var "desiredEvidence"))
	     (CreateChannel (AChannel "attesterChan") (AEntity att)	      
 	     (Send (Var "request") (AChannel "attesterChan")
	     (Receive (Var "response") (AChannel "attesterChan")
	     (Let (Var "finalResult") (ArmoredEvaluate (Var "request") (Var "response"))
	     (Result (Var "finalResult"))
	      )))))))
	      
	      
attProtocol = Receive (Var "receivedRequest") (AChannel "appChannel")
	     (Let (Var "caCertReq") (ArmoredCreateCACertReq AMyself)
             (Send (Var "caCertReq") ArmoredCAChannel
             (Receive (Var "CACert") ArmoredCAChannel
	     (Let (Array "emptyArray") ArmoredEmptyArray
	     (Let (Array "desiredEvidence") ArmoredExtractDesiredEvidence
	     (ForEach (Array "desiredEvidence")
	        (Send ArmoredArrayItem ArmoredMeasurerChan 
	        (Receive (Var "evidence") ArmoredMeasurerChan
	        (AppendArray (Array "emptyArray") (Var "evidence") Stop)))
	     (Let (Var "quote") ArmoredCreateQuote
	     (Let (Var "responseToApp") ArmoredCreateAppResponse
	     (Send (Var "responseToApp") (AChannel "appChannel")
	      Stop
	      )))))))))
	      
	      
	      
	        
	   
	   
