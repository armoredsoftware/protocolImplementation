module Main where --AppMain where

import CAProtoMain (caEntity_App)
import ProtoMonad
import ProtoTypesA
import ProtoActions
import VChanUtil
import TPM
import TPMUtil
import Keys
--import TPM.Types
import Provisioning(readComp)
--import ProtoTypes(Channel)

import Prelude
import Data.ByteString.Lazy hiding (putStrLn, map)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.Binary
import Control.Applicative hiding (empty)

appCommInit :: Int -> IO ProtoEnv
appCommInit domid = do
  attChan <- client_init domid
  let myInfo = EntityInfo "Appraiser" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
  (_,myPri) <- generateAKeyPair
  attPub <-getBPubKey
  let pubs = M.fromList [(1,attPub)]


  return $ ProtoEnv 0 myPri ents pubs 0 0 0 1


main = appmain' 1

appmain' :: Int -> IO ()
appmain' pId = do
  putStrLn "Main of entity Appraiser"
  env <- appCommInit 3 -- [appId, caId] --TODO: Need Channel form Paul
  let pcrSelect = mkTPMRequest [0..23]
      nonce = 34
  eitherResult <- runProto (caEntity_App [0,1,2] nonce pcrSelect) env
  str <- case eitherResult of
              Left s -> return $ "Error occured: " ++ s
              Right  resp@(ev, n, comp, cert@(SignedData aikPub aikSig), qSig) ->
                evaluate pId ([0,1,2], nonce, pcrSelect) (ev, n, comp, cert, qSig) -- "Response received:\n" ++ (show resp)
  putStrLn str
  return ()


evaluate :: Int -> (EvidenceDescriptor, Nonce, TPM_PCR_SELECTION) ->
            (Evidence, Nonce, TPM_PCR_COMPOSITE,
             (SignedData TPM_PUBKEY), Signature) -> IO String
evaluate pId (d, nonceReq, pcrSelect)
  (ev, nonceResp, pcrComp, cert@(SignedData aikPub aikSig), qSig) = do
  (caPublicKey, _) <- generateCAKeyPair
  let blobEvidence :: ByteString
      blobEvidence = packImpl [AEvidence ev, ANonce nonceResp,
                               ASignedData $ SignedData ( ATPM_PUBKEY (dat cert)) (sig cert)] --pubKey
      evBlobSha1 =  bytestringDigest $ sha1 blobEvidence

      quoteInfo :: TPM_QUOTE_INFO
      quoteInfo = TPM_QUOTE_INFO (tpm_pcr_composite_hash $ pcrComp)                                                        (TPM_NONCE evBlobSha1)

      aikPublicKey = tpm_get_rsa_PublicKey aikPub

      r1 = realVerify caPublicKey (encode aikPub) aikSig
      r2 = realVerify aikPublicKey (encode quoteInfo) qSig
      r3 = nonceReq == nonceResp
  goldenPcrComposite <- readComp
  --putStrLn $ "\n \n COMP Compare: \n"
  --putStrLn $ "\n measured comp: \n" ++ show pcrComp
  --putStrLn $ "\n measured comp: \n" ++ show goldenPcrComposite
  let r4 = pcrComp == goldenPcrComposite
      r5 = case pId of 1 -> ev == [0,1,2]
                       2 -> ev == []
  putStrLn $ show ev
  sequence $ [logf, putStrLn] <*> (pure ("CACert Signature: " ++ (show r1)))
  sequence $ [logf, putStrLn] <*> (pure ( "Quote Package Signature: " ++ (show r2)  ))
  sequence $ [logf, putStrLn] <*> (pure ( "Nonce: " ++ (show r3)))
  sequence $ [logf, putStrLn] <*> (pure ( "PCR Values: " ++ (show r4)))
  if (or[pId == 1, pId == 2] ) then sequence ([logf, putStrLn] <*> (pure ("Evidence: " ++ (show r5)))) else return [()]
  return $ case (and [r1, r2, r3, r4, r5]) of
    True -> "All checks succeeded"
    False -> "At least one check failed"


logf ::String -> IO ()
logf m = do
  h <- openFile "log.1" AppendMode
  hPutStrLn h (m ++ "\n")
  hClose h
