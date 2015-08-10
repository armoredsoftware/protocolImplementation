{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module MeasurerComm 
( getSocket
, measureSession
, debugSession
, Measurement (..)
)
where

import qualified Control.Monad.Remote.JSON as Jsonrpc
import Control.Monad.Remote.JSON
import Control.Monad.Remote.JSON.Debug (traceSessionAPI)
import Control.Monad.Remote.JSON.Types (SessionAPI(..))

import Control.Monad (void)
import Data.Aeson (decodeStrict, encode, Value(..))

import Data.ByteString.Lazy (toStrict)
import Data.ByteString (useAsCString, packCString)

import Data.Maybe (fromMaybe)

import Network.Socket
import qualified Network.Socket.ByteString as NBS

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

