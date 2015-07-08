module Main where
import Provisioning

import Codec.Crypto.RSA
import System.Random

generateNsKeyPair :: (PublicKey, PrivateKey)
generateNsKeyPair = let gen = mkStdGen 11 -- used 3 for A
                        (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)
main :: IO ()
main = do 
  putStrLn "START of provisioning main"
  --compGolden <- getCurrentComp
  --let fn = "bPubPri.txt"
     -- keyPair = generateNsKeyPair
  --genDoExport fn keyPair
  comp <- getCurrentComp
  doExport goldenFileName comp
  return ()
          
          
