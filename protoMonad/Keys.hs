{-# LANGUAGE ScopedTypeVariables #-}
module Keys where

import Codec.Crypto.RSA
import Crypto.Random
import System.Random
import System.IO
import Data.Binary

import TPM

caPublicKeyFile :: FilePath
caPublicKeyFile = "caPublicKey.txt"

caPrivateKeyFile :: FilePath
caPrivateKeyFile = "caPrivateKey.txt"

getCAPrivateKey :: IO PrivateKey
getCAPrivateKey = do
  readPrivateKey caPrivateKeyFile

getCAPublicKey :: IO PublicKey
getCAPublicKey = do
  readPublicKey caPublicKeyFile

generateArmoredKeyPair :: IO (Codec.Crypto.RSA.PublicKey,
                              Codec.Crypto.RSA.PrivateKey)
generateArmoredKeyPair = do
  gen::SystemRandom <- newGenIO
  let (pub, pri, _) = generateKeyPair gen 2048
  return (pub, pri)

exportPublicKey :: FilePath -> PublicKey -> IO ()
exportPublicKey fileName pubKey = do
  encodeFile fileName pubKey

exportPrivateKey :: FilePath -> PrivateKey -> IO ()
exportPrivateKey fileName priKey = do
  encodeFile fileName priKey

readPublicKey :: FilePath -> IO PublicKey
readPublicKey fileName = do
  either <- decodeFileOrFail fileName
  case either of
    Left (_, s) -> do putStrLn $ "Error reading/decoding from: " ++  fileName ++ "\n" ++ s
                      error "error reading/decoding file"
    Right a -> return a

readPrivateKey :: FilePath -> IO PrivateKey
readPrivateKey fileName = do
  either <- decodeFileOrFail fileName
  case either of
    Left (_, s) -> do putStrLn $ "Error reading/decoding from: " ++  fileName ++ "\n" ++ s
                      error "error reading/decoding file"
    Right a -> return a
