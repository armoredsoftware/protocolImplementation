{-# LANGUAGE ScopedTypeVariables #-}

module CAProtoMain where

import ArmoredTypes
import ProtoMonad
import ProtoActions
import Keys
import Provisioning
import TPM
import TPMUtil
import VChanUtil hiding (send, receive)
import CommTools(killChannel, getCaDomId)
import MeasurerComm --(getTest1cVarValue, getTestBufferValues)

import System.IO
import System.Random
import Control.Monad.IO.Class
import Data.Binary
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Crypto.Cipher.AES
import Codec.Crypto.RSA hiding (sign, verify, PublicKey, PrivateKey, encrypt, decrypt)

import Control.Applicative hiding (empty)
import Data.ByteString.Lazy hiding (replicate, putStrLn)
import Control.Concurrent (threadDelay)
import Data.Text as Text hiding (replicate)
import Data.Aeson ((.:),(.:?),FromJSON(..),decodeStrict, encode, Value(..), fromJSON, Result(..))
import qualified Control.Monad.Remote.JSON as Jsonrpc
import Control.Monad.Remote.JSON hiding (send)
import Control.Monad.Remote.JSON.Debug (traceSessionAPI)
import Control.Monad.Remote.JSON.Types (SessionAPI(..))
import Network.Socket hiding (send)

iPass = tpm_digest_pass aikPass
oPass = tpm_digest_pass ownerPass

caEntity_Att :: Proto ()
caEntity_Att = do

  debugPrintP "\nBEGINNING OF ENTITY_ATT!!!!!!!!!!!!!!!!!!!\n"
  let appraiserEntityId :: EntityId
      appraiserEntityId = 1
  pId <- protoIs
  liftIO $ pcrReset
  liftIO $ pcrModify "a"

  case pId of
    _ -> do

      req@ [AAEvidenceDescriptor dList,
            reqNonce@(ANonce nApp),
            ATPM_PCR_SELECTION pcrSelect] <- receive appraiserEntityId

      (iKeyHandle, aikContents) <- tpmMk_Id
      (ekEncBlob, kEncBlob) <- caAtt_CA aikContents

      sessKey <- tpmAct_Id iKeyHandle ekEncBlob

      let caCert :: (SignedData TPM_PUBKEY)
          caCert = realDecrypt sessKey kEncBlob
      liftIO $ sequence $ [logf, putStrLn] <*> (pure ( "Sending Request to Measurer"))
      evidence <- caAtt_Mea dList

      liftIO $ sequence $ [logf, putStrLn] <*> (pure ( "Received response from Measurer: " ++ (show evidence)))

      let quoteExData =
            [AEvidence evidence,
             ANonce nApp,
             ASignedData $ SignedData (ATPM_PUBKEY (dat caCert)) (sig caCert)]
      (pcrComp, qSig) <- tpmQuote iKeyHandle pcrSelect quoteExData

      let response =
            [(quoteExData !! 0),
             reqNonce,
             ATPM_PCR_COMPOSITE pcrComp,
             (quoteExData !! 2),
             ASignature qSig]
      liftIO $ putStrLn $ "Sending response to Appraiser: \n\n" ++ (show response) ++ "\n\n"
      send appraiserEntityId response
      return ()

   {- 2 -> do
      [reqNonce@(ANonce nApp),
       ATPM_PCR_SELECTION pcrSelect] <- receive appraiserEntityId

      (iKeyHandle, aikContents) <- tpmMk_Id
      (ekEncBlob, kEncBlob) <- caAtt_CA aikContents

      sessKey <- tpmAct_Id iKeyHandle ekEncBlob

      let caCert :: (SignedData TPM_PUBKEY)
          caCert = realDecrypt sessKey kEncBlob

      --evidence <- caAtt_Mea dList

      evidence <- return []
      let quoteExData =
            [AEvidence evidence, ANonce nApp,
             ASignedData $ SignedData (ATPM_PUBKEY (dat caCert)) (sig caCert)]
      (pcrComp, qSig) <- tpmQuote iKeyHandle pcrSelect quoteExData

      let response =
            [reqNonce,
             ATPM_PCR_COMPOSITE pcrComp,
             (quoteExData !! 2),
             ASignature qSig]
      send appraiserEntityId response
      return ()
-}

    _ -> error "Protocol id not yet implemented!"


caAtt_CA :: AikContents -> Proto (CipherText, CipherText)
caAtt_CA signedContents = do
  caDomId <- liftIO $ getCaDomId
  attChan <- liftIO $ client_init caDomId {-4-}
  let caEntityId :: EntityId
      caEntityId = 2
  myInfo <- getEntityInfo 0
  let val = SignedData
            (ATPM_IDENTITY_CONTENTS  (dat signedContents))
            (sig signedContents)
  liftIO $ send' attChan [AEntityInfo myInfo, ASignedData val]
  [ACipherText ekEncBlob, ACipherText kEncBlob] <- liftIO $ receive' attChan

  --attChan <- getEntityChannel caEntityId
  liftIO $ VChanUtil.close attChan
  --liftIO $ killChannel attChan
  return (ekEncBlob, kEncBlob)



{-(s, i) <- getTestBufferValues
      return [M0 i, M1 s] -}
caAtt_Mea :: EvidenceDescriptor -> Proto Evidence
caAtt_Mea ed = do
  pId <- protoIs
  case pId of
    1 -> do
      cVarValue <- getTest1cVarValue
      return $ [M0 cVarValue]
    2 -> do
      (s, i) <- getTestBufferValues
      return [M0 i, M1 s]
      --x -> error $ "Evidence Descriptor" ++ (show x) ++ "not supported yet"

caEntity_App :: EvidenceDescriptor -> Nonce -> TPM_PCR_SELECTION ->
                Proto (Evidence, Nonce, TPM_PCR_COMPOSITE,
                       (SignedData TPM_PUBKEY), Signature)
caEntity_App d nonceA pcrSelect = do
 -- let nonceA = 34
  pId <- protoIs
  liftIO $ sequence $ [logf, putStrLn] <*> (pure ( "Got here......."))
  let request = case pId of
        _ -> [AAEvidenceDescriptor d, ANonce nonceA, ATPM_PCR_SELECTION pcrSelect]
        {-2 -> [ANonce nonceA, ATPM_PCR_SELECTION pcrSelect]

        _ -> error "Protocol id not yet implemented!"
             --return []-}

  send 1 request
  --liftIO $ logf "Sent Request \n"
  --liftIO $ threadDelay 5000000
  --liftIO $ logf "Appraiser receiving \n"
  case pId of
        _ -> do
          response@[AEvidence e, ANonce nA, ATPM_PCR_COMPOSITE pComp,
           ASignedData (SignedData (ATPM_PUBKEY aikPub) aikSig),
           ASignature sig] <- receive 1
          --liftIO $ logf $ "Appraiser received: \n" ++ (show response) ++ "\n\n"
          return (e, nA, pComp, SignedData aikPub aikSig, sig)

        {-2 -> do
          [ANonce nA, ATPM_PCR_COMPOSITE pComp,
           ASignedData (SignedData (ATPM_PUBKEY aikPub) aikSig),
           ASignature sig] <- receive 1
          return ([], nA, pComp, SignedData aikPub aikSig, sig) -}

        _ -> error "Protocol id not yet implemented!"

caEntity_CA :: LibXenVChan -> IO ()
caEntity_CA attChan = do

  [AEntityInfo eInfo,
   ASignedData (SignedData
                (ATPM_IDENTITY_CONTENTS pubKey)
                 sig)]  <- receive' attChan

  ekPubKey <- readEK

  let iPubKey = identityPubKey pubKey
      iDigest = tpm_digest $ Data.Binary.encode iPubKey
      asymContents = contents iDigest
      blob = Data.Binary.encode asymContents
  encBlob <- tpm_rsa_pubencrypt ekPubKey blob

  caPriKey <- getCAPrivateKey
  let caCert = realSign caPriKey (Data.Binary.encode iPubKey)
      certBytes = Data.Binary.encode (SignedData iPubKey caCert)

      strictCert = toStrict certBytes
      encryptedCert = encryptCTR aes ctr strictCert
      enc = fromStrict encryptedCert

  send' attChan [ACipherText encBlob, ACipherText enc]
 where
   symKey =
     TPM_SYMMETRIC_KEY
     (tpm_alg_aes128)
     (tpm_es_sym_ctr)
     key

   v:: Word8
   v = 1
   key = ({-B.-}Data.ByteString.Lazy.pack $ replicate 16 v)
   --strictKey = toStrict key
   aes = initAES $ toStrict key
   ctr = toStrict key
   contents dig = TPM_ASYM_CA_CONTENTS symKey dig

tpmMk_Id :: Proto (TPM_KEY_HANDLE, AikContents)
tpmMk_Id = liftIO $ do
  (aikHandle, iSig) <- makeAndLoadAIK
  aikPub <- attGetPubKey aikHandle iPass
  let aikContents = TPM_IDENTITY_CONTENTS iPass aikPub
  return (aikHandle, SignedData aikContents iSig)

tpmAct_Id :: TPM_KEY_HANDLE -> CipherText -> Proto SymmKey
tpmAct_Id iKeyHandle actInput = liftIO $ do
  iShn <- tpm_session_oiap tpm
  oShn <- tpm_session_oiap tpm
  sessionKey <- tpm_activateidentity tpm iShn oShn
                iKeyHandle iPass oPass actInput
  return sessionKey

tpmQuote :: TPM_KEY_HANDLE -> TPM_PCR_SELECTION -> [ArmoredData] -> Proto (TPM_PCR_COMPOSITE, Signature)
tpmQuote qKeyHandle pcrSelect exDataList = liftIO $ do
  let evBlob = packImpl exDataList
      evBlobSha1 = bytestringDigest $ sha1 evBlob
  (comp, sig) <- mkQuote qKeyHandle iPass pcrSelect evBlobSha1
  return (comp, sig)

getMeasurement1 :: String -> String -> String -> Proto Measurement
getMeasurement1 host port pidString = do

  --sock <- getSocket host {-"10.100.0.249"-} port
  sock <- getMeaSocket
  liftIO $ do
  a <- Jsonrpc.send (mySession sock) $ do
                       set_target_app pidString
                       hook_app_variable "test1.c" 12 False 1 "c"

  print a
  threadDelay 6000000
  t<- Jsonrpc.send (mySession sock) $ do
    m <- load_store 1
    return m
      --b <- method "eval" (List [String "(load 1)"])
      --notification "eval" (List [String "(quit)"])
      --return b
      --close sock
      --case fromJSON t of
      --   Success (m :: Measurement) -> return (m, sock)
      --   Error s ->  error s
  return t

getMeasurement2 :: String -> String -> String -> Proto (Measurement,Measurement)
getMeasurement2 host port pidString = do

  sock <- getMeaSocket
  liftIO $ do

  Jsonrpc.send (mySession sock) $ do
       set_target_app pidString

  t<- Jsonrpc.send (mySession sock) $ do
    b <- measure_variable "password"
    --notification "eval" (List [String "(quit)"])
    return b

  q<- Jsonrpc.send (mySession sock) $ do
    b <- measure_variable "session"
    return b

  putStrLn $ "ATTESTER MEASUREMENTS:\n\n\n" ++ (show t) ++ "\n\n" ++ (show q) ++ "\n\n\n"
  return (t, q)


getTest1cVarValue :: Proto Int
getTest1cVarValue = do
  host <- liftIO $ getMyIPString
  port <- liftIO $ getPort
  pid <- liftIO $ getPid
  m <- getMeasurement1 host port pid
  let text = topMeasurement m
      s = Text.unpack text
      i = read s
  return i

getTestBufferValues :: Proto (String, Int)
getTestBufferValues = do
  host <- liftIO $ getMyIPString
  port <- liftIO $ getPort
  pid <- liftIO $ getPid
  (password, session) <- getMeasurement2 host port pid
  let pText = topMeasurement password
      pString = {-read-} $ Text.unpack pText
      sText = topMeasurement session
      sString = Text.unpack sText
      sInt = read sString

  liftIO $ putStrLn $"END OF getTestBufferValues!!!\n"  ++ "Decoded evidence:  \n" ++ "pString:  " ++ pString ++ "\n\nsInt: " ++ (show sInt) ++ "\n\n"
  return (pString, sInt)





{-getMeasurement2 :: String -> String -> String -> Proto (Measurement,Measurement)
getMeasurement2 host port pidString = do

  --sock <- getSocket host {-"10.100.0.249"-} port
  sock <- getMeaSocket
  liftIO $ do

  a <- Jsonrpc.send (mySession sock) $ do
       set_target_app pidString
       hook_app_variable "buffer_overflow2.c" 37 False 1 "password"

  --print a
  --threadDelay 2000000

  b <- Jsonrpc.send (mySession sock) $ do
    hook_app_variable "buffer_overflow2.c" 38 False 2 "session"

  --threadDelay 8000000

  t<- Jsonrpc.send (mySession sock) $ do
    b <- load_store 1
    --notification "eval" (List [String "(quit)"])
    return b

  q<- Jsonrpc.send (mySession sock) $ do
    b <- load_store 2
         --notification "eval" (List [String "(quit)"])
    return b
  --close sock
  {-case fromJSON t of
    Success (m1 :: Measurement) ->
      case fromJSON q of
        Success (m2 :: Measurement) -> do
          putStrLn $ "ATTESTER MEASUREMENTS:\n\n\n" ++ (show m1) ++ "\n\n" ++ (show m2) ++ "\n\n\n"
          return (m1, m2)
        Error s ->  error s
    Error s ->  error s -}
  putStrLn $ "ATTESTER MEASUREMENTS:\n\n\n" ++ (show t) ++ "\n\n" ++ (show q) ++ "\n\n\n"
  return (t, q)
-}
