{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module HttpChannel where

import AbstractedCommunication 
import Data.Aeson 
import qualified Network.Http.Client as HttpClient
import qualified Web.Scotty as Scotty
import CommTools hiding (sendHttp, receiveHttp)
import Control.Concurrent.MVar
import Control.Concurrent
import Web.Scotty hiding ( put)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import Control.Applicative
import Data.ByteString.Lazy hiding (putStrLn,length,map)
import Control.Monad
import Control.Monad.State.Strict
import Demo3Shared as AD
import System.IO.Error (tryIOError)
import System.Timeout


--jsut for testing
import VChanComm
data HttpChannel = HttpChannel {
    httpchanThreadID         :: MVar ThreadId,
    httpchanMyServingPort    :: HttpClient.Port,
    httpchanTheirServingPort :: Maybe HttpClient.Port,
    httpchanTheirIp 	     :: Maybe HttpClient.Hostname,
    httpchanMaybeConnection  :: (Maybe HttpClient.Connection),
    mvarMess                 :: (MVar Value)
 --   httpchanTMVarMsgList     :: (IsMessage a) => TMVar [a],
--    httpchanTMVarUnit        :: TMVar ()
   }
sendHttp :: (IsMessage a) => a -> HttpChannel -> IO Bool
sendHttp mess c = do
  case (httpchanTheirServingPort c, httpchanTheirIp c) of 
   (Just p,Just ipp) -> do 
    putStrLn $ "Sending to: " ++ (show ipp) ++ " and port: " ++ (show p)
    c <- HttpClient.openConnection ipp p
			    
    q <- HttpClient.buildRequest $ do
      HttpClient.http HttpClient.POST "/"
      HttpClient.setAccept "text/html/json"
      HttpClient.setContentType "application/x-www-form-urlencoded"
     --Prelude.putStrLn ( "Request: " ++ (show req))
    let nvs = [("request", (toStrict (AD.jsonEncode mess)))]
    --Prelude.putStrLn "about to send request"
    let x = HttpClient.encodedFormBody nvs
    --print "Made it here yaaaaaaaaaaaay"
    HttpClient.sendRequest c q (x)
    return True
   (_,_) -> do 
    putStrLn $ "Error!!! can't send anything! found nothing for 'their' port or IP"
    return False
   

receiveHttp :: (IsMessage a) => HttpChannel -> IO (Result a)
receiveHttp c = do 
  val <- takeMVar (mvarMess c)
  putStrLn $ "Receiving: " ++ (show val)
  return (fromJSON val)



httpServe :: HttpChannel -> IO ()
httpServe c = do
  let myport = httpchanMyServingPort c 
  let intPort = (fromIntegral myport :: Int)
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
	      let jj = AD.jsonEitherDecode (LazyEncoding.encodeUtf8 a) :: Either String Value
              liftIO $ putStrLn $ "RECEIVED: " ++ (show jj) ++ " on port: " ++ (show intPort)
	      case jj of
		(Left err)     -> text (LazyText.pack "ERROR: Could not even parse as Value.")
		(Right mess) -> do
		  liftIO $ forkIO $ putMVar (mvarMess c) mess 
		  Scotty.json (HttpSuccess (myport)) -- don't know why it won't let me put an empty string in there.
		  
  return ()

defaultport = 55555
maxport =     55655
instance IsChannel HttpChannel where
  send hc mess = sendHttp mess hc 
  receive hc = receiveHttp hc 
  initialize hc = do
    let mv = httpchanThreadID hc 
    b <- isEmptyMVar mv 
    if b then return ()
         else do 
          t <- takeMVar mv 
          killThread t                         
    tid <- forkIO $ httpServe hc 
    putMVar mv tid  
  killChan hc = do 
    let mv = httpchanThreadID hc 
    b <- isEmptyMVar mv 
    if b then return ()
         else do 
          t <- takeMVar mv 
          killThread t 
  toRequest hc = do 
    mip <- getMyIP' 
    return (toJSON (HttpReq mip (httpchanMyServingPort hc)))
  amend reqv hc = do 
    case (fromJSON reqv) :: Result HttpReq of
      Error er -> do 
        putStrLn $ "failed to read as request. Nothing to amend!!"
        -- return $ Left $ "No parse of Req: " ++ er 
        return hc 
      Success req -> do 
        putStrLn $ "successfully amended using: " ++ (show req)
        return $ HttpChannel (httpchanThreadID hc) (httpchanMyServingPort hc)  (Just (httpPort req)) (Just (httpIP req)) (httpchanMaybeConnection hc) (mvarMess hc)
  fromRequest reqv hc = do
     case (fromJSON reqv) :: Result HttpReq of
      Error er -> return $ Left $ "No parse of Req: " ++ er 
      Success req -> do 
        putStrLn $ "fromRequest: " ++ (show req)
        eitherPort <- findOpenPort defaultport maxport
        case eitherPort of 
          Left err -> do 
            let str = "Error finding open port in 'fromRequest': " ++ err 
            putStrLn str
            return $ Left str 
          Right p -> do 
            mv  <- newEmptyMVar
            mv2 <- newEmptyMVar 
            return $ Right $ HttpChannel mv {-(httpchanThreadID hc)-} p {- (httpchanMyServingPort hc)-}  (Just (httpPort req)) (Just (httpIP req)) (httpchanMaybeConnection hc) mv2 {-(mvarMess hc)-}
    --TODO should find open port and set it here!!!
    
       
   
  defaultChan = do 
--   ipp <- getMyIP'
   mv1 <- newEmptyMVar
   mv2 <- newEmptyMVar 
   return $ HttpChannel mv1 (fromIntegral defaultport) (Just (fromIntegral defaultport)) Nothing Nothing mv2
data HttpReq = HttpReq {
                httpIP :: HttpClient.Hostname,
                httpPort :: HttpClient.Port
                } deriving (Show, Eq)
instance ToJSON HttpReq where
 toJSON x = object 
  [ "httpIP" .= (httpIP x)
  , "httpPort" .= (httpPort x)
  ]

instance FromJSON HttpReq where
  parseJSON (Object o) = HttpReq <$> o .: "httpIP"
                                 <*> o .: "httpPort"


dumdumHttpIO = do 
 mv1 <- newEmptyMVar
 mv2 <- newEmptyMVar
 return $ HttpChannel mv1 0 Nothing Nothing Nothing mv2 

test :: IO ()
test = do 
  putStrLn "begginning test"
  lilNeg <- defaultChan :: IO HttpChannel
  comneg <- mkChannel $ lilNeg 
  dumChan <- defaultChan :: IO HttpChannel
  emptyChan <- mkChannel' dumChan
  declareCommunication (comneg,[emptyChan]) (\x -> putStrLn "succ")

  return ()

--findOpenPort :: HttpClient.Port -> HttpClient.Port -> IO (Either String HttpClient.Port)
findOpenPort x max = do 
  putStrLn $ "Doing find Open port!!"
  if x > max then return $ Left "Max point num reached"
   else do 
    meither <- timeout 100 $ tryIOError $ do  scotty x  (do Web.Scotty.get "/" $ Web.Scotty.text "hello there!\n")
    case meither of 
      Nothing -> return $ Right (fromIntegral x) 
      Just _  -> findOpenPort (x + 1) max 