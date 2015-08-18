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
import CommTools (whoAmI)
attest = do
            clearLogf
	    putStrLn "Appraise be to Attester"
	    let knownguys = [att,pCA]
	    let emptyvars = []
	    emptychans <- newTMVarIO []
            
            s <- fancyGetSocket
            t <- newMVar (AttState s)
            me <- whoAmI Attester 
	    let s0 = ArmoredState emptyvars me knownguys [] Nothing t emptychans
	    forkIO ( do
	    	runStateT negotiator s0
	    	return ()
	    	)
           -- runStateT negotiator s0
	    --threadDelay 6000000
	    --runExecute' myProto s0
	    newChannelTrigger emptychans []


newChannelTrigger :: TMVar [ChannelEntry] -> [ChannelEntry] -> IO a
newChannelTrigger chanETMVar handled = do 
  chanELS <- atomically $ takeTMVar chanETMVar
  let unhandled = chanELS \\ handled
  putStrLn $ (show unhandled)
  theadIDs <- sequence $ map (\chanE -> do 
        forkIO (
          do 
            putStrLn "Appraise be to Attester. Poppin' a thread."
	    let knownguys = [att,pCA]
	    let emptyvars = []	   
            let requester = channelEntity (channelEntryChannel chanE)
            t <- newMVar (AttState requester) 
	    
            let me = att
            atomically $ tryPutTMVar chanETMVar chanELS
	    let s0 = ArmoredState emptyvars me knownguys privacyPol (Just (channelEntryChannel chanE)) t chanETMVar  
            runExecute' myProto s0
            return ())) unhandled 
  let handled' = unhandled ++ handled
  atomically $ tryPutTMVar chanETMVar chanELS
  yield
  threadDelay 1000000 -- 1 second delay
  newChannelTrigger chanETMVar handled'
  
myProto =    CreateChannel (AChannel "chan") Requester
	   $ Receive (Var "request") (AChannel "chan")
	   $ ComputeCounterOffer (Var "counterOffer") (Var "request")	      
	   $ Send (Var "counterOffer") (AChannel "chan")
           $ Receive (Var "theirFinalChoice") (AChannel "chan")
           $ CheckFinalChoice (Var "finalAgreement") (Var "theirFinalChoice")
           $ Send (Var "finalAgreement") (AChannel "chan")
           $ HandleFinalChoice (Var "result") (Var "finalAgreement") 
	   $ Result (Var "result")
	     

-- <<<<<<< HEAD
--privacyPol = [Reveal [(ProtocolItem, [IntProperty 1])]  FREE 
  --           ]
-- =======
{-privacyPol = [Reveal [(ProtocolItem, [IntProperty 1])] condOS
                   , Reveal [(ProtocolItem, [IntProperty 2])] FREE
                                                                ] --condOS --FREE  -}
-- >>>>>>> 0652a7f8fdc5f8afb2b9f1f0c6df78abb8dd481b

privacyPol = [Reveal [(ProtocolItem, [IntProperty 1])] FREE
                                                                ] --condOS
condFree = FREE
condOS = Equals OS Name (ValString "McAffee")
--	      -}

data T1 = T1 T2 deriving (Show)

data T2 = T2 T1 deriving (Show)

f :: T1
f = T1 (T2 f)
