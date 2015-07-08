module Main where --Main

import CAProtoMain (caEntity_CA)
import ProtoMonad
import ProtoTypesA
import ProtoActions
import VChanUtil
import TPMUtil
import Keys
--import ProtoTypes(Channel)

import Prelude
import Data.ByteString.Lazy hiding (putStrLn, head, tail, map)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent

{-caCommInit :: Channel -> Int -> IO ProtoEnv
caCommInit attChan pId = do
 -- attChan <- server_init domid
  let myInfo = EntityInfo "CA" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
      myPri = snd $ generateAKeyPair
      attPub = getBPubKey
      pubs = M.fromList [(1,attPub)]


  return $ ProtoEnv 0 myPri ents pubs 0 0 0 pId -}



caCommInit :: LibXenVChan -> IO ProtoEnv
caCommInit attChan = do
  --attChan <- server_init domid
  let myInfo = EntityInfo "CA" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
  (_,myPri) <- generateAKeyPair
  attPub <- getBPubKey
  let pubs = M.fromList [(1,attPub)]


  return $ ProtoEnv 0 myPri ents pubs 0 0 0 1

  --return ()

caProcess :: ProtoEnv -> LibXenVChan -> IO ()
caProcess env attChan = do
 -- yield
  --attChan <- liftIO $ server_init chanId
  eitherResult <- runProto (caEntity_CA attChan) env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right _ -> putStrLn $ "Completed successfully" -- ++ (show resp)
  close attChan
--main = attCommInit [1,2]
main :: IO ()
main = do
  putStrLn "Main of entity CA"
  attChan <- server_init 3
  env <- caCommInit attChan -- [appId, caId]   --TODO: Need Channel form Paul
  --TODO:  choose protocol based on protoId

  caProcess env attChan
  {-let vChannels :: [Int]
      vChannels = [3, 5] -}
 -- mapM (forkIO . forever . (caProcess env)) (tail vChannels)
  forever$ do
    main --caProcess env 3
    --caProcess env 5
  --caProcess env (head vChannels)

  {-eitherResult <- runProto (caEntity_CA attChan) env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right _ -> putStrLn $ "Completed successfully" -- ++ (show resp)
    --TODO:  Output to Justin's log file here -}

  {-let as = [ANonce empty, ANonce empty, ACipherText empty]
      asCipher = genEncrypt (fst generateAKeyPair) as
      as' = genDecrypt (snd generateAKeyPair) asCipher
  putStrLn $ show $ as' -}
  --main
  return ()


{-camain :: Channel -> Int -> IO ()
camain chan pId = do
  putStrLn "Main of entity CA"
  env <- caCommInit undefined undefined -- [appId, caId]   --TODO: Need Channel form Paul
  --TODO:  choose protocol based on protoId
  eitherResult <- runProto caEntity_CA env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right _ -> putStrLn $ "Completed successfully" -- ++ (show resp)
    --TODO:  Output to Justin's log file here

  {-let as = [ANonce empty, ANonce empty, ACipherText empty]
      asCipher = genEncrypt (fst generateAKeyPair) as
      as' = genDecrypt (snd generateAKeyPair) asCipher
  putStrLn $ show $ as' -}
  return ()  -}
