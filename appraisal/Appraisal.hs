module Appraisal where --AppMain where

import CAProtoMain (caEntity_App)
import ProtoMonad
import ArmoredTypes
import ProtoActions
import VChanUtil
import TPM
import TPMUtil
import Keys
import Provisioning(readGoldenComp)


import Prelude
import Data.ByteString.Lazy hiding (putStrLn, map)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.Binary
import Control.Applicative hiding (empty)

import AbstractedCommunication

appCommInit :: Channel -> Int -> IO ProtoEnv
appCommInit attChan protoId {-domid-} = do
  --attChan <- client_init domid
  let myInfo = EntityInfo "Appraiser" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
  (attPub,myPri) <- generateArmoredKeyPair --Currently not used
  --attPub <-getBPubKey
  let pubs = M.fromList [(1,attPub)]


  return $ ProtoEnv 0 myPri ents pubs 0 0 0 protoId Nothing


--main = appmain' 1

appmain' :: Int -> Channel -> IO String
appmain' protoId chan = do
  putStrLn "Main of entity Appraiser"
  env <- appCommInit chan protoId {-3-}
  let pcrSelect = mkTPMRequest [0..23]
      nonce = 34
  eitherResult <- runProto (caEntity_App D0 nonce pcrSelect) env
  str <- case eitherResult of
              Left s -> return $ "Error occured: " ++ s
              Right  resp@(ev, n, comp, cert@(SignedData aikPub aikSig), qSig) ->
                evaluate protoId ([D0, D1, D2], nonce, pcrSelect) (ev, n, comp, cert, qSig) -- "Response received:\n" ++ (show resp)
              Right x@_ -> do
                let strr =  "ERROR: resp from runproto did not match expected.\n\n\n"
                return strr
  putStrLn str
  return str


evaluate :: Int -> ([EvidenceDescriptor], Nonce, TPM_PCR_SELECTION) ->
            (Evidence, Nonce, TPM_PCR_COMPOSITE,
             (SignedData TPM_PUBKEY), Signature) -> IO String
evaluate pId (d, nonceReq, pcrSelect)
  (ev, nonceResp, pcrComp, cert@(SignedData aikPub aikSig), qSig) = do
  debugPrint "Inside Evaluate" --sequence $ [logf, putStrLn] <*> (pure ( "Inside Evaluate..."))
  caPublicKey <- getCAPublicKey
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
  goldenPcrComposite <- readGoldenComp

  let r4 = pcrComp == goldenPcrComposite
      intVal = let m0 = Prelude.head ev in
                 case m0 of
                   M0 i -> i
                   _ -> error "Measurement descriptor not implemented!!"
      passString = case pId of
        1 -> ""
        2 -> let m1 = (ev !! 1) in
                case m1 of
                  M1 s -> s
                  _ -> error "Measurement descriptor not implemented!!"

      r5 = case pId of
        1 -> (intVal >= (20 :: Int))


        2 -> let goldenPassword = "\"12345\\000\\000\\000\\260\\005\"" in
          or [intVal == 0,  and [intVal == 1, passString == goldenPassword]]


  putStrLn $ show ev
  sequence $ [logf, putStrLn] <*> (pure ("CACert Signature: " ++ (show r1)))
  sequence $ [logf, putStrLn] <*> (pure ( "Quote Package Signature: " ++ (show r2)  ))
  sequence $ [logf, putStrLn] <*> (pure ( "Nonce: " ++ (show r3)))
  sequence $ [logf, putStrLn] <*> (pure ( "PCR Values: " ++ (show r4)))
  let guardString = case pId of
        1 -> "(Value >= 20?)"
        2 -> "(session == 0 OR session == 1 AND password == \"12345\":  False implies a buffer overflow)"
  if (or[pId == 1, pId == 2] )
    then sequence ([logf, putStrLn] <*> (pure ("Evidence" ++ guardString ++ ": " ++ (show r5))))
    else return [()]

  case pId of
    1 -> sequence ([logf, putStrLn] <*> (pure ("(Evidence Value: " ++ (show intVal) ++ ")")))
    2 -> sequence ([logf, putStrLn] <*> (pure ("(Session: " ++ (show intVal) ++ ", \n Password: " ++ passString ++ ")")))


    _ -> return [()]

  --if (or[pId == 1, pId == 2] ) then sequence ([logf, putStrLn] <*> (pure ("Evidence: " ++ (show r6) ++ ", Password Value: " ++ (show passString) ++ ", session Int Value: " ++ (show evVal)))) else return [()]
  return $ case (and [r1, r2, r3, r4, r5 {-,r6-}]) of
    True -> "All checks succeeded"
    False -> "At least one check failed"
