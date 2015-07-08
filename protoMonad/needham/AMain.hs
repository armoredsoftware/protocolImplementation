module Main where

import ProtoMain (nsEntityA)
import ProtoMonad
--import ProtoTypesA
import ProtoActions
import VChanUtil

import Prelude 
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random

acommInit :: Int -> IO ProtoEnv
acommInit targetDomId = do
  chan <- client_init targetDomId
  let bInfo = EntityInfo "B" 22 chan
      aInfo = EntityInfo "A" 11 chan
      mList = [(0, aInfo), (1, bInfo)]
      ents = M.fromList mList
      myPri = snd $ generateAKeyPair
      bPub = getBPubKey
      pubs = M.fromList [(1,bPub)]
  
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0


main :: IO ()
main = do 
  putStrLn "Main of entity A"
  env <- acommInit 3
  eitherResult <- runProto nsEntityA env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right nonce -> putStrLn $ "Nonce received: " ++ (show nonce)
  
  
  {-let as = [ANonce empty, ANonce empty, ACipherText empty]
      asCipher = genEncrypt (fst generateAKeyPair) as
      as' = genDecrypt (snd generateAKeyPair) asCipher
  putStrLn $ show $ as' -}
  return ()
  
generateAKeyPair :: (Codec.Crypto.RSA.PublicKey, Codec.Crypto.RSA.PrivateKey)
generateAKeyPair = let 
  gen = mkStdGen 3 -- used 11 for B
  (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)
                                              
getBPubKey :: Codec.Crypto.RSA.PublicKey
getBPubKey =  let 
  gen = mkStdGen 11 
  (pub, _, _) = generateKeyPair gen 2048 in pub