module Main where --Main

import CAProtoMain (caEntity_CA)
import ProtoMonad
import ArmoredTypes
--import ProtoTypesA
import ProtoActions
import VChanUtil
import TPMUtil
import Keys
import CommTools(getAttesterDomId)

import Prelude
import Data.ByteString.Lazy hiding (putStrLn, head, tail, map)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent

{-caCommInit :: Channel -> IO ProtoEnv
caCommInit attChan = do
  let myInfo = EntityInfo "CA" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
  myPri <- getCAPrivateKey
  (attPub, _) <- generateArmoredKeyPair -- Not used
  let pubs = M.fromList [(1,attPub)]
  return $ ProtoEnv 0 myPri ents pubs 0 0 0 1 -}

caProcess :: {-ProtoEnv -> -} LibXenVChan -> IO ()
caProcess {-env-} attChan = do
 -- yield
  {-eitherResult <- runProto (caEntity_CA attChan) env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right _ -> putStrLn $ "Completed successfully" -- ++ (show resp)
   -}
  caEntity_CA attChan
  putStrLn "Completed successfully"
  close attChan

main :: IO ()
main = do
  putStrLn "Main of entity CA"
  attDomId <- getAttesterDomId
  attChan <- server_init attDomId {-3-}
  --env <- caCommInit attChan
  caProcess {-env-} attChan

  forever$ do
    main
  return ()
