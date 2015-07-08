{-# LANGUAGE ScopedTypeVariables #-}
module ProtoActions where

import ProtoTypesA
import ProtoMonad
import VChanUtil
--import CommTools (sendG', receiveG')
--import ProtoTypes(Channel)

import Data.ByteString.Lazy hiding (pack, map, putStrLn)
import qualified Control.Monad.Trans.Reader as T
import Data.Monoid
import Data.Binary
import qualified Codec.Crypto.RSA as C
import Crypto.Cipher.AES
import Crypto.Random
import System.Random
import Control.Monad.IO.Class
import Control.Monad.Error

import TPM --TODO:  Find a way to eliminate this import(abstract it away)

generateNonce :: Proto Nonce
generateNonce = do
  return 56

checkNonce :: Nonce -> Nonce -> Proto ()
checkNonce expected actual = do
  case (expected == actual) of
    True -> return ()
    False -> throwError "Nonce check failed"

--Encrypt with the PublicKey associated with targetId
--TODO:  Is there only 1 public key associated with each target(maybe abstractly?)
encrypt :: EntityId -> [ArmoredData]-> Proto CipherText
encrypt targetId inData = do
  pubKey <- getEntityPubKey targetId
  return $ genEncrypt pubKey inData

--Decrypt using MY PrivateKey
decrypt :: CipherText -> Proto [ArmoredData]
decrypt cipherText = do
  priKey <- T.asks (myPriKey)
  return $ genDecrypt priKey cipherText

--Symmetric Key decryption
decrypt' :: (Binary a) => SymmKey -> CipherText -> a
decrypt' sessKey blob = let
  keyBytes = tpmSymmetricData sessKey
  strictKey = toStrict keyBytes
  aes = initAES strictKey
  ctr = strictKey
  decryptedBytes = decryptCTR aes ctr (toStrict blob)
  lazy = fromStrict decryptedBytes in
  (decode lazy)


--Sign with MY PrivateKey
sign :: [ArmoredData] -> Proto (SignedData [ArmoredData])
sign inData = do
  priKey <- T.asks (myPriKey)
  return $ genSign priKey inData

--sendG' :: Channel -> [ArmoredData] -> IO ()
send :: EntityId -> Message -> Proto ()
send toId ds = do
  chan <- getEntityChannel toId
 -- logger <- liftIO createLogger
--  liftIO $ sendChunkedMessageByteString logger chan (toStrict $ encode ds)
 -- liftIO $ putStrLn $ "Sending: " ++ (show ds)
  --liftIO $ sendG' chan ds
  liftIO $ send' chan ds
  --liftIO $ putStrLn $ "Sent message! " ++ (show ds)
  return ()

send' :: LibXenVChan -> Message -> IO ()
send' chan ds = do
  --chan <- getEntityChannel toId
  logger <- createLogger
  sendChunkedMessageByteString logger chan (toStrict $ encode ds)
  putStrLn $ "Sending: " ++ (show ds)
 -- liftIO $ sendG' chan ds
  putStrLn $ "Sent message! " ++ (show ds)
  return ()

receive :: EntityId -> Proto Message
receive fromId = do
 -- liftIO $ putStrLn $ "In receive"
  chan <- getEntityChannel fromId
 -- liftIO $ putStrLn $ "Got Chan"
  --logger <- liftIO createLogger
  --bytes <- liftIO $ readChunkedMessageByteString logger chan
 -- liftIO $ putStrLn $ "Got bytes"
 -- let result = decode $ fromStrict bytes
  result <- liftIO $ receive' chan
--  result <- liftIO $ receiveG' chan
--  liftIO $ putStrLn $ "Received message!"   -- ++ (show result)
 -- liftIO $ putStrLn $ "Received: " ++ (show result)
  return $ result

receive' :: LibXenVChan -> IO Message
receive' chan = do
  putStrLn $ "In receive"
  --chan <- getEntityChannel fromId
 -- liftIO $ putStrLn $ "Got Chan"
  ctrlWait chan
  logger <- createLogger
  bytes <- readChunkedMessageByteString logger chan
 -- liftIO $ putStrLn $ "Got bytes"
  let result = decode $ fromStrict bytes
  --result <- liftIO $ receiveG' chan
  putStrLn $ "Received message!"   -- ++ (show result)
 -- liftIO $ putStrLn $ "Received: " ++ (show result)
  return $ result

--TODO:  Should this be in the Proto monad?(i.e. to choose packImpl).
genEncrypt :: Binary a => PublicKey -> [a] -> IO CipherText
genEncrypt pubKey inData = realEncrypt pubKey clearText
 where
   clearText = packImpl inData --extract packImpl from ProtoEnv here(via Proto monad)?

--TODO:  Should this be in the Proto monad?(i.e. to choose unpackImpl).
genDecrypt :: PrivateKey -> CipherText -> [ArmoredData]
genDecrypt priKey blob =
  let decrypted = realDecrypt priKey blob in
  unpackImpl decrypted

--TODO:  In Proto monad?
genSign :: Binary a => PrivateKey -> [a] -> (SignedData [a])
genSign priKey inData =
  let bytes = packImpl inData
      signature = realSign priKey bytes in
  SignedData inData signature


--Concrete implementations-------------------------------------------------
realEncrypt :: PublicKey -> ByteString -> IO CipherText
realEncrypt pubKey clearText = do --Concrete implementation plugs in here
  {-let gen = mkStdGen 3 -}
  gen::SystemRandom <- newGenIO
  let (cipher, _) = C.encrypt gen pubKey clearText
  return cipher

realDecrypt :: PrivateKey -> CipherText -> ByteString
realDecrypt priKey cipherText = --Concrete implementation here
  C.decrypt priKey cipherText

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
