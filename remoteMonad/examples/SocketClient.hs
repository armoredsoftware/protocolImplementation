{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}


import qualified Control.Monad.Remote.JSON as Jsonrpc
import Control.Monad.Remote.JSON
import Control.Monad.Remote.JSON.Debug (traceSessionAPI)
import Control.Monad.Remote.JSON.Types (SessionAPI(..), Args(..))
import Control.Applicative

import Control.Monad (void, mzero)
import Control.Concurrent (threadDelay)


import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (useAsCString, packCString)
import Data.Aeson

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import Network.Socket
import qualified Network.Socket.ByteString as NBS

import System.Environment
import System.Exit

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



getSocket :: HostName -> ServiceName -> IO Socket
getSocket host port = do
                         addrinfo <- getAddrInfo Nothing (Just host ) (Just port)
                         let serveraddr = head addrinfo

                         sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                         setSocketOption sock KeepAlive 1

                         connect sock (addrAddress serveraddr)

                         return sock


main:: IO()
main = do
          args<-getArgs
          if (length args /= 1) then
             do putStrLn "Error: Enter port number as argument"
                exitFailure
          else
             putStr ""
          let port = head args

          sock <- getSocket "10.100.0.249" port

          putStrLn "Enter PID number"
          pid      <- readLn :: IO Int
          a <- Jsonrpc.send (mySession sock) $ do
                       notification "eval" (List [String (Text.pack ("(set_target "++(show pid)++")"))])
                       method "eval" (List [String (Text.pack "(hook (reach \"test1.c\" 12 0) '(store 1 (measure (var \"c\"))))")])
          print a
          threadDelay 6000000
          t<- Jsonrpc.send (mySession sock) $ do
                       b <- method "eval" (List [String "(load 1)"])
                       notification "eval" (List [String "(quit)"])
                       return b

          case fromJSON t of
             Success (a :: Measurement) -> print a
             Error s ->  print s


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
