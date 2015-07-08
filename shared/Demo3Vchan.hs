{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, RecordWildCards  #-}

module Demo3Vchan where

import TPM



import qualified Data.ByteString as B (ByteString)
import Codec.Crypto.RSA hiding (sign, verify)
import System.Random
import Crypto.Cipher.AES

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import qualified Data.Aeson as DA (Value(..), encode, decode, eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import Control.Applicative ( (<$>), (<*>), pure )
import qualified Data.HashMap.Strict as HM (member, lookup)
import Data.Maybe
import qualified Data.ByteString.Char8 as Char8

import Demo3SharedNOVCHAN
import VChanUtil
import Data.Binary
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict)


import qualified Control.Monad.Trans.State as T
import Control.Monad.Trans

data AttState = AttState {checks::[Bool],
meaChan :: LibXenVChan,
appChan :: LibXenVChan,
priChan :: LibXenVChan, 
stops :: Bool}


type Att = T.StateT AttState IO
runAtt = T.runStateT

getAt :: Int -> Att Bool
getAt ind = do
	st <- T.get
	return $ last $ take ind (checks st)
c1 :: Att Bool
c1 = getAt 1
c2 :: Att Bool
c2 = getAt 2
c3 :: Att Bool
c3 = getAt 3
c4 :: Att Bool
c4 = getAt 4
c5 :: Att Bool
c5 = getAt 5
c6 :: Att Bool
c6 = getAt 6
c7 :: Att Bool
c7 = getAt 7

enterP :: String -> Att ()
enterP s = do
  st <- T.get
  let stopsBool = stops st
  case stopsBool of 
    True -> liftIO $ putStrLn ("\nPress enter to " ++ s) >> getChar >> return ()
    False -> return ()
                


getMeaChan :: Att LibXenVChan
getMeaChan = do
	st <- T.get
	return $ meaChan st
getAppChan :: Att LibXenVChan
getAppChan = do
	st <- T.get
	return $ appChan st
getPriChan :: Att LibXenVChan
getPriChan = do
	st <- T.get
	return $ priChan st


			   --return chan

{-
vChanSend :: (ToJSON a) => Int -> a -> IO LibXenVChan
vChanSend id ob = do
			chan <- client_init id
			sendShared' chan ob
			return chan

vChanSend' :: (ToJSON a) => LibXenVChan -> a -> IO ()
vChanSend' chan obj = do
			   logger <- createLogger
			   sendChunkedMessageByteString logger chan (toStrict (jsonEncode obj))
			   return ()
			   --return chan

vChanReceive :: (FromJSON a) => LibXenVChan -> IO (Either String a)
vChanReceive chan = do
			ctrlWait chan
			logger <- createLogger
			bytes <- readChunkedMessageByteString logger chan
			let shared =  jsonEitherDecode (fromStrict bytes) :: Either String Shared
			return shared
-}




sendM :: (Binary a, Show a) => String -> LibXenVChan ->  a -> IO ()
sendM descrip chan m = do
  putStrLn $ descrip ++ "Sending: " ++ show m ++ "\n"
  send chan $ m
  return () 


sendR :: (Binary a, Show a) => PlatformID -> String -> a -> IO LibXenVChan
sendR dest descrip req = do
    chan <- client_init dest
    putStrLn $ descrip ++ "Sending: " ++ show req ++ "\n"
    send chan $ req
    return chan

receiveM :: (Binary a, Show a) => String -> LibXenVChan -> IO a
receiveM descrip chan = do
  ctrlWait chan
  res <- receive chan
  putStrLn $ descrip ++ "Received: " ++ show res ++ "\n"
  return res


process :: (Binary a, Show a, Binary b, Show b) => (LibXenVChan -> IO a) 
                -> (LibXenVChan -> b -> IO ()) -> (a -> IO b) -> PlatformID -> IO ()
process recA sendB mk pId = do
  --ctrlWait chan
  chan <- server_init pId
  process' recA sendB mk chan
  close chan
  return ()

process' :: (Binary a, Show a, Binary b, Show b) => (LibXenVChan -> IO a) 
                -> (LibXenVChan -> b -> IO ()) -> (a -> IO b) -> LibXenVChan -> IO ()
process' recA sendB mk chan = do
  --ctrlWait chan
  --chan <- server_init pId
  req <- recA chan
  putStrLn "\n\nPROCESS: Request received\n\n"
  resp <- mk req
  putStrLn "\n\nPROCESS: Response constructed\n\n"
  sendB chan resp
  putStrLn "\n\nPROCESS: Response sent\n\n"
  --close chan
  return ()
