{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MeasurerComm
( getSocket
, measureSession
, debugSession
, getTest1cVarValue
, getTestBufferValues
, Measurement (..)
)
where

import qualified Control.Monad.Remote.JSON as Jsonrpc
import Control.Monad.Remote.JSON
import Control.Monad.Remote.JSON.Debug (traceSessionAPI)
import Control.Monad.Remote.JSON.Types (SessionAPI(..))

import Control.Monad (void, mzero)
import Control.Applicative
import Data.Aeson ((.:),(.:?),FromJSON(..),decodeStrict, encode, Value(..), fromJSON, Result(..))
import System.Environment
import System.Exit
import Control.Concurrent (threadDelay)

import Data.ByteString.Lazy (toStrict)
import Data.ByteString (useAsCString, packCString)

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import Network.Socket
import qualified Network.Socket.ByteString as NBS

import Network.Info
import Data.List



data Measurement = Measurement { next:: Maybe (Measurement),
                                res::Text.Text,
                                resultType::Text.Text}
    deriving Show

instance FromJSON Measurement where
    parseJSON (Object v) = Measurement <$>
                             v .:? "next" <*>
                             v .: "data"<*>
                             v .: "type"
    parseJSON _          = mzero


topMeasurement :: Measurement -> Text.Text
topMeasurement m = head $ measurementToList m

measurementToList :: Measurement -> [Text.Text]
measurementToList m = case next m of
                         Nothing -> [res m]
                         Just m2 -> (res m): (measurementToList m2)

debugSession :: Socket -> Session
debugSession sock = session $ traceSessionAPI "MeasureComm" $ remoteSend sock

measureSession :: Socket -> Session
measureSession sock = session $ id $ remoteSend sock

remoteSend :: Socket -> SessionAPI a -> IO a
remoteSend sock (Sync v) = do NBS.sendAll sock $ toStrict $ encode v
                              msg' <- NBS.recv sock 4096
                              msg <- useAsCString msg' packCString
                              return $ fromMaybe Null (decodeStrict msg ::Maybe Value)

remoteSend sock (Async v) = do NBS.sendAll sock $ toStrict $ encode v
                               void $ NBS.recv sock 4096



getSocket :: HostName -> ServiceName -> IO Socket
getSocket host port = do
                         addrinfo <- getAddrInfo Nothing (Just host ) (Just port)
                         let serveraddr = head addrinfo

                         sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                         setSocketOption sock KeepAlive 1

                         connect sock (addrAddress serveraddr)

                         return sock

mySession :: Socket -> Session
mySession sock = session $ traceSessionAPI "Justin Test" $ remoteSend sock

   where
          remoteSend :: Socket -> SessionAPI a -> IO a
          remoteSend sock (Sync v) = do NBS.sendAll sock $ toStrict $ encode v
                                        msg' <- NBS.recv sock 4096
                                        msg <- useAsCString msg' packCString
                                        return $ fromMaybe Null (decodeStrict msg ::Maybe Value)

          remoteSend sock (Async v) = do NBS.sendAll sock $ toStrict $ encode v
                                         void $ NBS.recv sock 4096

getPort :: IO String
getPort = do
  stringArgs <- getArgs
  return $ stringArgs !! 3

getPid :: IO String
getPid = do
  stringArgs <- getArgs
  return $ stringArgs !! 4

getTest1cVarValue :: IO (Int, Socket)
getTest1cVarValue = do
  host <- getMyIPString
  port <- getPort
  pid <- getPid
  (m, sock) <- getMeasurement1 host port pid
  let text = topMeasurement m
      s = Text.unpack text
      i = read s
  return (i, sock)

getTestBufferValues :: IO (String, Int)
getTestBufferValues = do
  host <- getMyIPString
  port <- getPort
  pid <- getPid
  (password, session) <- getMeasurement2 host port pid
  let pText = topMeasurement password
      pString = Text.unpack pText
      sText = topMeasurement session
      sString = Text.unpack sText
      sInt = read sString
  putStrLn $"END OF getTestBufferValues!!!\n"  ++ "Decoded evidence:  \n" ++ "pString:  " ++ pString ++ "\n\nsInt: " ++ (show sInt) ++ "\n\n"
  return (pString, sInt)

getMeasurement1 :: String -> String -> String -> IO (Measurement, Socket)
getMeasurement1 host port pidString = do

          sock <- getSocket host {-"10.100.0.249"-} port

          a <- Jsonrpc.send (mySession sock) $ do
                       notification "eval" (List [String (Text.pack ("(set_target "++ pidString ++")"))])
                       method "eval" (List [String (Text.pack "(hook (reach \"test1.c\" 12 0) '(store 1 (measure (var \"c\"))))")])
          print a
          threadDelay 6000000
          t<- Jsonrpc.send (mySession sock) $ do
                       b <- method "eval" (List [String "(load 1)"])
                       --notification "eval" (List [String "(quit)"])
                       return b
          --close sock
          case fromJSON t of
             Success (m :: Measurement) -> return (m, sock)
             Error s ->  error s

getMeasurement2 :: String -> String -> String -> IO (Measurement,Measurement)
getMeasurement2 host port pidString = do

          sock <- getSocket host {-"10.100.0.249"-} port

          a <- Jsonrpc.send (mySession sock) $ do
                       notification "eval" (List [String (Text.pack ("(set_target "++ pidString ++")"))])
                       method "eval" (List [String (Text.pack "(hook (reach \"buffer_overflow2.c\" 37 0) '(store 1 (measure (var \"password\"))))")])
          print a
          --threadDelay 2000000

          b <- Jsonrpc.send (mySession sock) $ do
                       method "eval" (List [String (Text.pack "(hook (reach \"buffer_overflow2.c\" 38 0) '(store 2 (measure (var \"session\"))))")])
          print b
          putStrLn $ "\n\nMEASUREMENTS HOOKED!!!!!!!!!!!!!!!!!!!!!!\n"
          threadDelay 8000000

          t<- Jsonrpc.send (mySession sock) $ do
                       b <- method "eval" (List [String "(load 1)"])
                       --notification "eval" (List [String "(quit)"])
                       return b

          q<- Jsonrpc.send (mySession sock) $ do
                       b <- method "eval" (List [String "(load 2)"])
                       notification "eval" (List [String "(quit)"])
                       return b
          close sock
          case fromJSON t of
             Success (m1 :: Measurement) ->
               case fromJSON q of
                 Success (m2 :: Measurement) -> do
                   putStrLn $ "ATTESTER MEASUREMENTS:\n\n\n" ++ (show m1) ++ "\n\n" ++ (show m2) ++ "\n\n\n"
                   return (m1, m2)
                 Error s ->  error s
             Error s ->  error s



getMyIP :: IO IPv4
getMyIP = do
  ips <- getNetworkInterfaces
  let ip = Data.List.map ipv4 $ Data.List.filter (\x-> ((name x) /= "lo") && (show (ipv4 x)) /= "0.0.0.0" ) ips
  putStrLn $ "GETTING IP INFO: " ++ (show ip)
  return (Data.List.head ip)

getMyIPString = do
  pv4 <- getMyIP
  return (show pv4)
