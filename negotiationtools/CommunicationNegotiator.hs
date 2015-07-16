{-# LANGUAGE OverloadedStrings #-}

module CommunicationNegotiator where 


import Web.Scotty hiding (get, put)
import qualified Web.Scotty as Scotty
import qualified ArmoredTypes as AD
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import qualified Data.Text.Lazy as LazyText
import Network.Http.Client hiding (get)
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import qualified Network.Http.Client as HttpClient
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
--import Control.Monad
import Control.Monad.State.Strict
--import ProtoTypes
import ArmoredTypes
import CommTools hiding (killChannel)
import Control.Monad.STM
import System.Random
import VChanUtil
import Data.Time
import Control.Concurrent
import System.Timeout
import Data.Aeson (eitherDecode, encode)

import Data.Word (Word16)

import qualified Data.List as List
negotiationport = 3000


channelExistsWith :: [ChannelEntry] -> Entity -> IO (Maybe Channel)
channelExistsWith [] e     = return Nothing
channelExistsWith (x:xs) e = do
			  let c = channelEntryChannel x
			  let ent = channelEntity c
			  if ent == e then return (Just c)
			  	      else channelExistsWith xs e
			  	      	    
vChanSendAndListen :: LibXenVChan -> Shared -> (MVar (Either String Shared)) -> IO ()
vChanSendAndListen vchan message mvar = do
  putStrLn $ "about to send: " ++ (show message) ++ "on vchan"
  sendShared' vchan message
  putStrLn "send message on vChan"
  yield
  eitherShared <- receiveShared vchan
  putMVar mvar eitherShared
   	
vChanMVarListener :: LibXenVChan -> MVar (Either String Shared) -> IO ()
vChanMVarListener rChan mvar = do
				yield
				putStrLn "IN VCHANLISTENER"
                                --receiveShared :: IO (Either String Shared)
                                maybeEitherShared <- timeout 1000000 (receiveShared rChan)
--				eitherShared <- receiveShared rChan
                                case maybeEitherShared of 
                                  Nothing -> do 
                                    putMVar mvar (Left "TIMEOUT from System.Timeout I wish I found sooner")  
                                  Just eitherShared -> do
                                    putStrLn $ "I received somethings on vChannel!!: " ++ (show eitherShared)
                                    putMVar mvar eitherShared
                                
				--putMVar mvar eitherShared
				return ()
				  			    
pingVChannel :: LibXenVChan -> Integer -> Integer -> IO Bool
pingVChannel chan n1 n2= do
		     respMVar <- newEmptyMVar :: IO (MVar (Either String Shared))
                     putStrLn "about to fork vChanSendAndListen" 
		     forkIO $ vChanSendAndListen chan (WNonce (n1 + 1)) respMVar	
                     putStrLn $ "forked vChanSendAndListen"	      
		     curTime <- getCurrentTime
                     putStrLn $ "Current time: " ++ (show curTime)
		     let seconds = utctDayTime curTime
		     waitPatiently respMVar seconds
		     --mvar should have been filled with something at this point
		     eitherStringShared <- takeMVar respMVar
		     case eitherStringShared of
		     	(Left err) -> do
		     			putStrLn err
		     			return False
		     	(Right (WNonce shouldBeN2plus1)) -> do
		     				if shouldBeN2plus1 == (n2 +1)
		     				  then return True
		     				  else do
		     				   putStrLn "Wronge respNONCE"
		     				   return False
			(Right _)	   -> do
						putStrLn "Received something weird instead of nonce response"
						return False

vChanTimeout = 1
waitPatiently :: (MVar (Either String Shared)) -> DiffTime -> IO ()
waitPatiently mvar startSeconds = do
			putStrLn "waiting patiently..."
  			isEmpt <- isEmptyMVar mvar
  			putStrLn $ "is mvar empty? " ++ (show isEmpt)
  			case isEmpt of
  			  False -> return () --something is there! we are done!
  			  True  -> do
				curTime <- getCurrentTime
				let curSeconds = utctDayTime curTime
				if (curSeconds - startSeconds) > vChanTimeout 
				  then do
				  	isEmpt <- isEmptyMVar mvar --check again if it's empty
				  	if isEmpt then putMVar mvar (Left "Timeout") -- if still empty,
				  		  else return ()		     -- if not empty, yay! 
				  	
				  else do -- we haven't timed out yet.
				  	yield 
                                        threadDelay 10000
				  	waitPatiently mvar startSeconds
		     	    
attemptVChanContactR :: Int ->Integer -> Integer-> IO (Maybe LibXenVChan)
attemptVChanContactR id n1 n2 = do
                           yield
			   putStrLn $ "Doing attemptVChanContactR with n1= " ++ (show n1) ++ "and n2 = " ++ (show n2)
			 --  receiveChan <- server_init id -justin says baaaaad/not needed. 
                           putStrLn $ "Maybe_Client_Init with id: " ++ (show id)
			   maybeSendChan <-  maybe_client_init id --client_init' id False
			   --remember, to get to this point means the other guy sent a request which means he is listening on his little vchannel
			   case maybeSendChan of
			     (Nothing)   -> do
			     		    putStrLn $ "mabye_client_init id has failed."                                    
			     		    return Nothing --then we def know not local guy
			     (Just chan) -> do
			     	 putStrLn "successfully openned vchannel for sending!"
			         success <- pingVChannel chan n1 n2
			         case success of
			           False -> return Nothing
			           True ->  return (Just chan)			         

--maybeCreateVChannelWith
--this is a funny method...
--this will return nothing if VChan fails...
--returns the vchannel if the vchannel exists with entity
--returns the HTTPCHANNEL if the channel exists with entity
--if a channel is found to exist (regardless of vchan or http) the channel is renamed here
--to what the user expects.
--Hence the createChannel command should always be done before using a channel.
--I think there is an easy way to do a receive any, but I'm not there yet.
maybeCreateVChannelWith :: Entity -> String -> ArmoredStateTMonad (Maybe Channel)
maybeCreateVChannelWith ent chanName = do
  s <- get
  let chanETMVar = getChannelEntriesTMVar s
  let me = getExecutor s
  chanEls <- liftIO $ atomically $ takeTMVar chanETMVar
  chanExists <- liftIO $ channelExistsWith chanEls ent
  case chanExists of
    (Just channel) -> do
    			liftIO $ putStrLn "found that channel already exists."
    			liftIO $ atomically ( putTMVar chanETMVar (
                               (ChannelEntry chanName channel):
                                             (List.deleteBy (\a b -> (channelEntryChannel a) == (channelEntryChannel b)) 
                                               (ChannelEntry "stupidworthlessname" channel) chanEls
                                             )
                                                                  )  
                                            )
                         --above line is important. we place a channel in the state that has the name we are expecting.
                         -- instead of the name autogenerated by the negotiator
    			return (Just channel)
    (Nothing)      -> do
    			liftIO $ putStrLn $ "I do not already have a channel with this entity. attempting to make Vchan contact"
    			mVChannel <- liftIO $ tryCreateVChannel me ent    			
                        case mVChannel of
                          Just vc -> do 
                            liftIO $ atomically $ putTMVar chanETMVar ((ChannelEntry chanName vc) : chanEls)
                            return (Just vc)
                          Nothing -> do
                            liftIO $ atomically $ putTMVar chanETMVar chanEls
                            return Nothing --liftIO $ tryCreateHttpChannel ent 

tryCreateHttpChannel :: Entity -> String -> ArmoredStateTMonad ( Maybe Channel)
tryCreateHttpChannel ent chanName = do
  case entityIp ent of 
    Nothing    -> return Nothing
    Just entIP -> do  
      s <- get
      let chanETMVar = getChannelEntriesTMVar s 
      let me = getExecutor s
      chanls <- liftIO $ atomically $ takeTMVar chanETMVar -------------------------------------------------------------
      let myPort = getFreePort chanls
      nonce1 <- liftIO ( randomIO :: IO Integer )
      let portRequest =  PortRequest me myPort nonce1 
      armoredlsTMVar <- liftIO ( newTMVarIO [] :: IO (TMVar [Armored]))
      unitTMVar <- liftIO (newEmptyTMVarIO  :: IO (TMVar ()) ) --empty since obviously no messages yet.
      let httpChanInfo = HttpInfo Nothing myPort Nothing entIP Nothing armoredlsTMVar unitTMVar
      expectedNonceMVar <- liftIO $ newEmptyMVar
      
      threadID <- liftIO $ forkIO $  httpExpectNonce httpChanInfo expectedNonceMVar                          
      connWithNegotiation <-liftIO $ sendHttp (WCommRequest portRequest) entIP negotiationport
      curTime <- liftIO $ getCurrentTime
      liftIO $ putStrLn $ "Current time: " ++ (show curTime)
      let seconds = utctDayTime curTime                      
      liftIO $ waitPatiently expectedNonceMVar seconds
      eithershared <- liftIO $ takeMVar expectedNonceMVar
      case eithershared of
        (Left err) -> do
          liftIO $ putStrLn ("ERROR in attempting http comm: " ++ err)
          liftIO $ atomically $ putTMVar chanETMVar (chanls) ------------------
          return Nothing
        (Right shared) -> do
          case shared of 
            (WNonce shouldBeN1p1) -> do --do good channel creating here.
              if shouldBeN1p1 /= (nonce1+1) 
               then do
                 liftIO $ putStrLn "error. nonce value received is no good."
                 liftIO $ atomically $ putTMVar chanETMVar (chanls) -------------------
                 return Nothing
               else do
                 eitherS  <- liftIO $ receiveHttp connWithNegotiation
                 case eitherS of --this should be the port they are serving on.
                  (Left err) -> do
                    liftIO $ putStrLn $ "Error in response from other negotiator: " ++ (show err)
                    liftIO $ atomically $ putTMVar chanETMVar (chanls) ------------------
                    return Nothing
                  (Right (HttpSuccess theirPort)) -> do --everything went nicely if we got to here. 
      	  	    let httpInfo' = HttpInfo (Just threadID) myPort (Just theirPort) entIP (Nothing) armoredlsTMVar unitTMVar
      	  	    let channel = Channel ent httpInfo'
      	  	    let channelEntry = ChannelEntry (chanName) channel
      	  	    liftIO $ atomically $ putTMVar chanETMVar (channelEntry:chanls)
                    return (Just channel)
                  (Right anythingElse) -> do
                    liftIO $ putStrLn $ "Error trying to create HTTPChannel. I wanted an HttpSuccess. Instead I got: " ++ (show anythingElse)
                    return Nothing 
                    
            a@_                    -> do
              liftIO $ putStrLn $ "Error. received wrong type back when expected nonce in attempt http chan contact: " ++ (show a)
              return Nothing 

--somewhere about 50,000 are the reserved temp ports. this is pie, but with the 5 in the wrong place.
defaultport=53141

getFreePort :: [ChannelEntry] -> Port 
getFreePort [] = defaultport 
getFreePort chanEs = let ports = harvestPorts chanEs in 
                       if Prelude.length ports == 0 
                         then defaultport 
                         else (maximum ports) + 1

eitherReceiveSharedWTimeout :: LibXenVChan -> IO (Either String Shared)
eitherReceiveSharedWTimeout rchan = do
				mvar <- newEmptyMVar :: IO (MVar (Either String Shared))
				putStrLn $ "Beginning to listen on VChan for something. I am expecting a nonce back of course. the one I sent in vchanreq"
				forkIO $ vChanMVarListener rchan mvar
				putStrLn $ "successfully forked VChan listener"
				curTime <- getCurrentTime
		     		let seconds = utctDayTime curTime
		     		waitPatiently mvar seconds
		     		yield
                                putStrLn "about to block by taking mvar that should have a nonce in it or an error."
		     		eitherStringShared <- takeMVar mvar --at this point guarenteed to not be empty
		     		case eitherStringShared of
		     		 (Left err)  -> do
		     		 		 putStrLn err
		     		 		 return (Left err)
		     		 (Right shared) -> return (Right shared)
		     		 
		     		
--this gets called when executor (first entity) does NOT have a channel with the other entity yet.
--However, the other entity MAY already have a channel established with executor. 				      			        
tryCreateVChannel :: Entity -> Entity -> IO (Maybe Channel)
tryCreateVChannel me ent = do
			 let mID = entityId ent
			 case mID of
			   (Nothing) -> return Nothing -- entity has no id, can't use vChan!
			   (Just id) -> do
			   		  putStrLn $ "Other entity has ID: " ++ (show id)  ++ " init-ing listener on chan" 
			   		  rChan <- server_init id
			   		  putStrLn $ "Now listening!"
			   		  let mIP = entityIp ent
			   		  case mIP of
			   		   Nothing -> return Nothing
			   		   (Just ip) -> do 
			   		      conn <-liftIO $ openConnection ip negotiationport
			   		      n1 <- randomIO :: IO Integer
			   		      let vchanReq = VChanRequest me n1
                                              putStrLn "about to send VChannel request"
			   		      sendHttp' (WCommRequest vchanReq) conn
                                              putStrLn "sent vChannelRequest, about to attempt to receive back over HTTP"
			   		      eitherSharedBack <- receiveHttp conn  --this should be the challenge nonce for vchan comm.
                                              putStrLn $ "received over http. I got this: " ++ (show eitherSharedBack) 
			   		      case eitherSharedBack of
			   		       (Left err) -> do
			   		       		      putStrLn err
			   		       		      return Nothing
					       (Right shared) -> do
					       		      case shared of
					       		        (WNonce n2) -> do   --this should be the challenge nonce for vchan comm. 
					       		        	  putStrLn $ "RECEIVED CHALENGE NONCE N2: " ++ (show n2)
					       		        	  eitherShared <- eitherReceiveSharedWTimeout rChan
					       		        	  case eitherShared of
					       		        	    (Left err) -> do
					       		        	    		   putStrLn err
					       		        	    		   return Nothing
					       		        	    (Right shared) -> case shared of
					       		        	    			(WNonce shouldBeN1plus1) ->
					       		        	    			  if shouldBeN1plus1 == (n1 + 1) then do
					       		        	    			    --send back the nonce received over http
					       		        	    			    sendShared' rChan (WNonce (n2+1))
					       		        	    			    let vchanInfo = VChanInfo (Just rChan)
					       		        	    			    let channel = Channel ent vchanInfo
					       		        	    			    return (Just channel) --yay!!!!!!!!!!! 
					       		        	    			   else do
					       		        	    			     putStrLn "Error. That's not the nonce I wanted!"
					       		        	    			     return Nothing
					       		        	    			(_) -> do
					       		        	    				putStrLn "received unexpected thing from VChannel"
					       		        	    				return Nothing
					       		        x   -> do
					       		                  putStrLn ("Unexpected response type in vchanrequest over http: " ++ (show x))
					       		                  return Nothing			   		       		      
			   		      
			   		  --mSendChan <- maybe_client_init id	        
negotiator :: ArmoredStateTMonad ()
negotiator = do
    s <- get
    let chanETMVar =  getChannelEntriesTMVar s
    let me = getExecutor s
    liftIO $ Scotty.scotty (fromIntegral negotiationport) $ do
	    Scotty.get "/" $ Scotty.text "serving\n"  --a quick way to check if the CA is 
	    				 	--serving, client should see "foobar" 
	    				 	--(Note: unqualified get is ambiguous).

	    Scotty.post "/" $ do
	      --reads in "potential" request (parsing it could fail). 
	      --Note: no en/de-crypting is yet taking place.
	      a <- (param "request") :: ActionM LazyText.Text  
	     -- myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
	     -- myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
	     -- myprint' ("Data received on port: " ++ (show port)) 1
	      
	      --first converts the Text to UTF8, then then attempts to read a CARequest
	      let jj = eitherDecode (LazyEncoding.encodeUtf8 a) :: Either String Shared
	      liftIO $ putStrLn $ "Negotiator received: " ++ (show jj)
	      liftIO $ logf "Negotiator received: " ++ (show jj)
	      case jj of
	      	(Left err) -> text (LazyText.pack "ERROR: Improper request.")
	      	(Right (WCommRequest (VChanRequest entity n1))) -> do
                  liftIO $ putStrLn $ "I have received a vChan request. Beginning negotiation."
	      	--1. check if channel already exists, do nothing if it does
	      	--2. if we need to make a channel..
	      	
	      	--first lock on the channels
	      	  chanls <- liftIO $ atomically $ takeTMVar chanETMVar
	      	  chanExists <- liftIO $ channelExistsWith chanls entity
	      	  case chanExists of
	      	  	(Just chan)  -> do
                                  liftIO $ putStrLn "I have found that I already have this channel. Doing nothing"
	      	  		  liftIO $ atomically $ putTMVar chanETMVar chanls
	      	  		  json (VChanSuccess "I already have a channel with you.") --do nuthin'
	      	  	Nothing      -> do -- interesting stuff
      	  		  case (entityId entity) of 
      	  				    (Nothing)  -> do
      	  				    		  liftIO $ putStrLn "entityId entity gave back a Nothing."
      	  				    		  --return Nothing
                                                          json (VChanFailure "enititID entity gave back a Nothing.")
      	  				    (Just id)  -> do
                                                           liftIO $ putStrLn $ "NEGORTIATOR: Other entity has id: " ++ (show (entityId entity))
                                                           liftIO $ putStrLn "NEGOTIATOR: about to send n2 as response to request"
                                                           n2 <- liftIO (randomIO :: IO Integer)
                                                           liftIO $ forkIO $ do 
                                                                           yield
                                                                           threadDelay 10000
                                                                           maybeVChan <- attemptVChanContactR id n1 n2
                                                                           case maybeVChan of
      	  		                                                     (Just vchan) -> do
      	  		   			                               let chan = Channel entity (VChanInfo (Just vchan)) 
      	  		   	                                               	--armoredlsTMVar <- liftIO ( newTMVarIO [] :: IO (TMVar [Armored])) --shouldn't need this at all for vChan
      	  		   			                                --unitTMVar <- liftIO $ newTMVarIO () 
      	  		   			                               let chanEntry = ChannelEntry ("ChannelWith" ++ (entityName entity)) chan 
      	  		   						     --armoredlsTMVar
      	  		   						     --unitTMVar
      	  		   			                               liftIO $ atomically $ putTMVar chanETMVar (chanEntry:chanls)
      	  		   			           --                    json (VChanSuccess "success yayyy")
      	  		                                                     Nothing              -> do --then we can't do vchan and must do http.
      	  		   			                               liftIO $ atomically $ putTMVar chanETMVar chanls
      	  		   			                               --json (VChanFailure "vchanfail estab shoot.") 
      	  				    		   json (WNonce n2)
                                                           --liftIO $ putStrLn (show x)
                          
      	        (Right (WCommRequest (PortRequest entity portSuggestion nonce))) -> do
      	           chanls <- liftIO $ atomically $ takeTMVar chanETMVar
	      	   mchan <- liftIO $ channelExistsWith chanls entity
	      	   case mchan of
	      	   	(Just channel) -> do
	      	   		let chanInfo = channelInfo channel
	      	   		let respThingy = case chanInfo of
	      	   				  (VChanInfo vchan ) -> case (vchan) of
	      	   				  				Just _ -> VChanSuccess "yaaay"
	      	   				  				_      -> VChanFailure "woooohooo"
	      	   				  (HttpInfo _ myPort _ _ _ _ _) -> HttpSuccess myPort
	      	   				  				
	      	   		liftIO $ atomically $ putTMVar chanETMVar chanls
	      	  		json $ respThingy
	      	  		--note possible attack here. send entity you want to be and it will respond with listening port.
			Nothing        -> do
				case entityIp entity of
				  Nothing -> do
				  	      liftIO $ atomically $ putTMVar chanETMVar chanls
				  	      liftIO $ putStrLn "no sender IP"
				  	      json (HttpFailure "crap")
				  (Just ip) -> do
				     let takenPorts = harvestPorts chanls
				     let myport = (if portSuggestion `elem` takenPorts 
				  	            then ((maximum takenPorts) + 1)  --possible security issue. 
				  	            else portSuggestion )
				     conn <-liftIO $ openConnection ip portSuggestion
                                     liftIO $ sendHttp' (WNonce (nonce + 1)) conn
				     --TODO send nonce + 1 here.
				     armoredlsTMVar <- liftIO ( newTMVarIO [] :: IO (TMVar [Armored]))
      	  		   	     unitTMVar <- liftIO (newEmptyTMVarIO  :: IO (TMVar ()) ) --empty since obviously no messages yet.
      	  		   	     let httpInfo = HttpInfo Nothing myport (Just portSuggestion) ip (Just conn) armoredlsTMVar unitTMVar
                                     threadID <- liftIO $ forkIO (httpServe httpInfo)
                                     let httpInfo' = HttpInfo (Just threadID) myport (Just portSuggestion) ip (Just conn) armoredlsTMVar unitTMVar
      	  		   	     let channel = Channel entity httpInfo'
      	  		   	     let channelEntry = ChannelEntry ("ChannelWith" ++ (entityName entity)) channel
      	  		   	     
      	  		   	     liftIO $ atomically $ putTMVar chanETMVar (channelEntry:chanls)
                                     liftIO $ putStrLn "Added channel to canETMVar list of channels"
      	  		   	     json $ HttpSuccess myport	
    return ()

httpExpectNonce :: ChannelInfo -> MVar (Either String Shared) -> IO ()
httpExpectNonce (HttpInfo _ myport theirPort theirIP maybeConn msglsTMVar unitTMVar) mvar = do
  let intPort = (fromIntegral (myport) :: Int)
  Scotty.scotty intPort $ do
    Scotty.post "/" $ do
      a <- (param "request") :: ActionM LazyText.Text    
      let jj = eitherDecode (LazyEncoding.encodeUtf8 a) :: Either String Shared
      case jj of
	(Left err)     -> text (LazyText.pack "ERROR: Improper request.")
	(Right shared) -> do
          case shared of
            (WNonce n1) -> do
              liftIO $ putStrLn "n1 received as expected. sending back n1+1 as response."
              liftIO $ tryPutMVar mvar (Right (WNonce n1))
              json (WNonce (n1 +1))
            _         -> do 
	      let armored = sharedToArmored shared
	      liftIO $ atomically $ do 
                chanls <- takeTMVar msglsTMVar
	        let chanls' = chanls ++ [armored]
	        putTMVar msglsTMVar chanls'
	        putTMVar unitTMVar ()  -- definitely has something now. 
	      json (HttpSuccess (myport)) -- don't know why it won't let me put an empty string in there.
		  
  return ()


httpServe :: ChannelInfo -> IO ()
httpServe (HttpInfo _ myport theirPort theirIP maybeConn msglsTMVar unitTMVar) = do
  let intPort = (fromIntegral(myport) :: Int)
  Scotty.scotty intPort $ do
  	    Scotty.get "/" $ Scotty.text "serving\n"  --a quick way to check if the CA is 
	    				 	--serving, client should see "foobar" 
	    				 	--(Note: unqualified get is ambiguous).

	    Scotty.post "/" $ do
	      --reads in "potential" msg (parsing it could fail). 
	      --Note: no en/de-crypting is yet taking place.
	      a <- (param "request") :: ActionM LazyText.Text  
	     -- myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
	     -- myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
	     -- myprint' ("Data received on port: " ++ (show port)) 1
	      
	      --first converts the Text to UTF8, then then attempts to read a CARequest
	      let jj = eitherDecode (LazyEncoding.encodeUtf8 a) :: Either String Shared
	      case jj of
		(Left err)     -> text (LazyText.pack "ERROR: Improper request.")
		(Right shared) -> do
		  let armored = sharedToArmored shared
		  liftIO $ atomically $ do 
		  			 chanls <- takeTMVar msglsTMVar
		        		 let chanls' = chanls ++ [armored]
		        		 putTMVar msglsTMVar chanls'
		        		 putTMVar unitTMVar ()  -- definitely has something now. 
		  json (HttpSuccess (myport)) -- don't know why it won't let me put an empty string in there.
		  
  return ()
  
harvestPorts :: [ChannelEntry] -> [HttpClient.Port]
harvestPorts []     = []
harvestPorts (x:xs) = case (channelInfo (channelEntryChannel x)) of
			(VChanInfo _)          -> harvestPorts xs
			h@(HttpInfo _ _ _ _ _ _ _) -> (httpInfoMyServingPort h) : (harvestPorts xs)



killChannels :: ArmoredStateTMonad ()
killChannels = do
  s <- get
  let chanETMVar = getChannelEntriesTMVar s
  chanELS <- liftIO $ atomically ( do 
                                     val <- takeTMVar chanETMVar
                                     putTMVar chanETMVar [] 
                                     return val)
  liftIO $ sequence $ map killChannel chanELS
--  liftIO $ atomically $
  return ()


killChannel :: ChannelEntry -> IO ()
killChannel chanE  = do 
  let chanInfo = channelInfo $ channelEntryChannel chanE
  case chanInfo of
    (HttpInfo mTid myPort mTheirPort theirIP mConn msgTMVar unitTMVar) -> do
      case mTid of
       Nothing -> 
         return ()
       Just tid -> 
         killThread tid
      case mConn of
        Nothing ->
          return ()
        Just conn -> do 
          closeConnection conn
    (VChanInfo mChan) -> do
      case mChan of
        Nothing -> do
          return ()
        Just c  -> do
          close c

 
