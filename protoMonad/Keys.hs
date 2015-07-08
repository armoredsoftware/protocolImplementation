{-# LANGUAGE ScopedTypeVariables #-}
module Keys where

import Codec.Crypto.RSA
import Crypto.Random
import System.Random
import System.IO

import TPM



generateAKeyPair :: IO (Codec.Crypto.RSA.PublicKey, Codec.Crypto.RSA.PrivateKey)
generateAKeyPair = do {- let gen = mkStdGen 3 -- used 11 for B -}
  gen::SystemRandom <- newGenIO
  let (pub, pri, _) = generateKeyPair gen 2048
  return (pub, pri)

getBPubKey :: IO Codec.Crypto.RSA.PublicKey
getBPubKey =  do {-let gen = mkStdGen 11 -}
  gen::SystemRandom <- newGenIO
  let (pub, _, _) = generateKeyPair gen 2048
  return pub


generateCAKeyPair :: IO (PublicKey, PrivateKey)
generateCAKeyPair = do {-let gen = mkStdGen 3 -}
  gen::SystemRandom <- newGenIO
  let (pub, pri, _) = generateKeyPair gen 2048
  return (pub, pri)


exportEKFileName = "ekpub.txt"

readPubEK :: IO TPM_PUBKEY
readPubEK = do
  handle <- openFile exportEKFileName ReadMode
  pubKeyString <- hGetLine handle
  let pubKey :: TPM_PUBKEY
      pubKey = read pubKeyString
  hClose handle
  return pubKey

--"One-time use" export function
exportCAPub :: String -> PublicKey -> IO ()
exportCAPub fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle

--One-time use func to export pubEK
exportEK :: String -> TPM_PUBKEY -> IO ()
exportEK fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle
