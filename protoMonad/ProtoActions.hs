{-# LANGUAGE ScopedTypeVariables #-}
module ProtoActions where

import ArmoredTypes
import ProtoMonad
import VChanUtil
import TPM.Types

import Data.ByteString.Lazy hiding (pack, map, putStrLn)
import Data.Monoid
import Data.Binary
import qualified Codec.Crypto.RSA as C
import Crypto.Cipher.AES
import Crypto.Random
import System.Random
import Control.Monad.IO.Class
import Control.Monad.Error

generateNonce :: Proto Nonce
generateNonce = do
  return 56 --faked

checkNonce :: Nonce -> Nonce -> Proto ()
checkNonce expected actual = do
  case (expected == actual) of
    True -> return ()
    False -> throwError "Nonce check failed"

--Symmetric Key decryption
realDecrypt :: (Binary a) => SymmKey -> CipherText -> a
realDecrypt sessKey blob = let
  keyBytes = tpmSymmetricData sessKey
  strictKey = toStrict keyBytes
  aes = initAES strictKey
  ctr = strictKey
  decryptedBytes = decryptCTR aes ctr (toStrict blob)
  lazy = fromStrict decryptedBytes in
  (decode lazy)

send :: EntityId -> Message -> Proto ()
send toId ds = do
  chan <- getEntityChannel toId
  liftIO $ send' chan ds
  --liftIO $ putStrLn $ "Sent message! " ++ (show ds)
  return ()

send' :: LibXenVChan -> Message -> IO ()
send' chan ds = do
  logger <- createLogger
  sendChunkedMessageByteString logger chan (toStrict $ encode ds)
  putStrLn $ "Sending: " ++ (show ds)
  putStrLn $ "Sent message! " ++ (show ds)
  return ()

receive :: EntityId -> Proto Message
receive fromId = do
 -- liftIO $ putStrLn $ "In receive"
  chan <- getEntityChannel fromId
  result <- liftIO $ receive' chan
  return $ result

receive' :: LibXenVChan -> IO Message
receive' chan = do
  putStrLn $ "In receive"
  ctrlWait chan
  logger <- createLogger
  bytes <- readChunkedMessageByteString logger chan
 -- liftIO $ putStrLn $ "Got bytes"
  let result = decode $ fromStrict bytes
  putStrLn $ "Received message!"   -- ++ (show result)
  return $ result

realSign :: PrivateKey -> ByteString -> Signature --paramaterize over hash?
realSign priKey bytes = C.rsassa_pkcs1_v1_5_sign C.hashSHA1 priKey bytes --Concrete implementation plugs in here

realVerify :: PublicKey -> ByteString -> Signature -> Bool
realVerify pubKey m s = C.rsassa_pkcs1_v1_5_verify C.hashSHA1 pubKey m s

--Concrete packing(well-defined strategy for combining elements in preparation for encryption/signing) implementation
packImpl :: (Binary a) => [a] -> ByteString
packImpl as = encode as --mconcat bslist
 --where bslist = map tobs as

--Concrete unpacking implementation
unpackImpl :: Binary a => ByteString -> [a]
unpackImpl bs = decode bs
