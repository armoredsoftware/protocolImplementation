module Main where
import Provisioning

import Codec.Crypto.RSA
import System.Random

main :: IO ()
main = do
  putStrLn "START of provisioning main"

  ekProvision
  --exportCurrentComp
  --exportCAKeys

  return ()
