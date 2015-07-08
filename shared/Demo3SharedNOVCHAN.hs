{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, RecordWildCards  #-}

module Demo3SharedNOVCHAN where

import TPM

import Data.Binary
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict)
import qualified Data.ByteString as B (ByteString)
import Codec.Crypto.RSA hiding (sign, verify)
import System.Random
import Crypto.Cipher.AES
import qualified Control.Monad.Trans.State as T hiding (pack)
import Control.Monad.Trans
import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import qualified Data.Aeson as DA (Value(..), encode, decode, eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import Control.Applicative ( (<$>), (<*>), pure )
import qualified Data.HashMap.Strict as HM (member, lookup)
import Data.Maybe
import qualified Data.ByteString.Char8 as Char8

import qualified Network.Http.Client as HttpClient
--import Prelude ( ($!) )
import Data.List (isInfixOf, head) --for parsing the id file.
import Text.Read (readMaybe) --for parsin'
import System.IO (IOMode( ReadMode ), openFile,hGetContents)
import System.IO.Unsafe (unsafePerformIO) --LOL

tpm :: TPMSocket
tpm = tpm_socket "/var/run/tpm/tpmd_socket:0"



{-
q :: Att ()
q = do b <- done
liftIO $ case b of
True -> putStrLn "done"
False -> putStrLn "not done"
{-before <- cc
liftIO $ putStrLn $ show before
stepFalse 1
after <- cc
liftIO $ putStrLn $ show after
-}
mapM stepFalse [0..6]
st <- T.get
liftIO $ putStrLn $ show $ checks st
-}
setAt :: Bool -> Int -> [Bool] -> [Bool]
setAt b ind bs = (xs ++ (b:ys'))
 where (xs, ys) = splitAt (ind-1) bs
       ys' = drop 1 ys
setTrueAt = setAt True
setFalseAt = setAt False
updateFalse :: Int -> [Bool] -> [Bool]
updateFalse ind bs
	| (ind == 0) = bs
	| otherwise = setFalseAt ind bs
updateTrue :: Int -> [Bool] -> [Bool]
updateTrue ind bs
	| (ind == 0) = bs
	| otherwise = setTrueAt ind bs


appName :: String
appName = "Appraiser"
appId :: Int
appId = ($!)(\x -> x) (getID appName 1) --68 --20    --		

attName :: String
attName = "Attester"
attId :: Int
attId = getID attName 3 --69 --19

meaName :: String
meaName = "Measurer"
meaId :: Int
meaId = getID meaName 2 --61 --7

caName :: String
caName = "CA"
caId :: Int
caId = getID meaName 21 --21



getID :: String -> Int -> Int
getID str defaultInt = let eitherWords = unsafePerformIO $ getWords "domains.txt" in
		         case eitherWords of
		         	(Left err) -> defaultInt
		         	(Right words) -> let eitherId = searchFor str words in
        					  case eitherId of
        	   				   (Left err) -> defaultInt
			    	        	   (Right r)  -> r
		    
getWords :: String -> IO (Either String [String])
getWords file =     do
        	handle <-  openFile file ReadMode
        	contents <-  hGetContents handle
        	let singlewords = (words contents)
        	return (Right (singlewords))
        	
        	{-
        	
        	   		    -}
searchFor :: String -> [String] -> Either String Int
searchFor _ [] = (Left "Not found!!")
searchFor _ [x] = (Left "Not found!!!")
searchFor str (x:xs) = if isInfixOf str x then
			let maybeParsedInt = readMaybe (head xs)in
				case maybeParsedInt of
					(Just i) -> Right i
					(Nothing) -> Left "Hey that wasn't a number following the idName I was looking for!"
		       else searchFor str xs


ownerPass :: String
ownerPass = "adam"

srkPass :: String
srkPass = ""

exportEKFileName = "attEKPubKey.txt"

exportCAPubFileName = "appCAPublicKey.txt"


type PubKey = Codec.Crypto.RSA.PublicKey
type PriKey = Codec.Crypto.RSA.PrivateKey

type SymKey = B.ByteString
generateBadQuotePriKey :: PriKey
generateBadQuotePriKey = let gen = mkStdGen 5
			     (_, pri, _) = generateKeyPair gen 2048 in pri
generateCAKeyPair :: (PubKey, PriKey)
generateCAKeyPair = let gen = mkStdGen 3
                        (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)
                                                                    
generateBadCAKeyPair :: (PubKey, PriKey)
generateBadCAKeyPair = let gen = mkStdGen 11
                           (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)

encrypt :: Binary a => SymKey -> a -> ByteString
encrypt key m = (fromStrict encryptedStrictM)

 where encodedM = encode m
       strictM = toStrict encodedM
       --strictKey = toStrict lazyKey
       aes = initAES key
       ctr = key
       encryptedStrictM = encryptCTR aes ctr strictM
       --lazyEncryptedStrictM = fromStrict encryptedStrictM

decrypt :: SymKey -> ByteString -> ByteString
decrypt key encryptedM = (fromStrict strictDecryptedM)

 where strictEncryptedM = toStrict encryptedM
       aes = initAES key
       ctr = key
       strictDecryptedM = decryptCTR aes ctr strictEncryptedM

sign :: (Binary a{-, Signable a-}) => PriKey -> a -> ByteString
sign priKey a = rsassa_pkcs1_v1_5_sign ha_SHA1 priKey ({-toBits-} encode a)

verify :: (Binary a{-, Signable a-})=> PubKey -> Signed a -> Bool
verify pubKey signed = rsassa_pkcs1_v1_5_verify ha_SHA1 
                                                                          pubKey 
                                                                          ({-toBits-}encode $ dat signed) 
                                                                          (sig signed)
  
                      
signPack :: (Binary a{-, Signable a-}) => PriKey -> a -> Signed a
signPack priKey x = Signed x sig
  where sig = sign priKey x

--instance Signable TPM_QUOTE_INFO


-- Primitive types
type Signature = ByteString

--Abstract datatype for signed payloads
data Signed a = Signed {
  dat :: a, 
  sig :: Signature
  } deriving (Eq, Show, Read)

--Request
data Request = Request {
  desiredE :: DesiredEvidence,
  pcrSelect :: TPM_PCR_SELECTION,
  nonce :: TPM_NONCE
  } deriving (Eq{-Show-})
             
instance Show Request where
  show (Request e c q) = "Request {\n\ndesiredE = " ++ (show e) ++ ",\n\npcrSelect = " ++ (show c) ++ ",\n\nnonce = " ++ (show q) ++ "\n}"
             
type DesiredEvidence = [EvidenceDescriptor]
data EvidenceDescriptor = D0 | D1 | D2 
                        | DONE deriving(Eq, Ord, Show) --for now

instance Binary EvidenceDescriptor where
  put D0 = put (0::Word8)
  put D1 = put (1::Word8)
  put D2 = put (2::Word8)
  put DONE = put (3::Word8)
           
  get = do t<- get :: Get Word8
           case t of
               0 -> return D0
               1 -> return D1
               2 -> return D2
               3 -> return DONE
 {-                   
instance Show EvidenceDescriptor where
  show D0 = "Desired: Measurement #0"
  show D1 = "Desired: Measurement #1"
  show D2 = "Desired: Measurement #2"
  show DONE = "DONE DESCRIPTOR" -}


--type Quote = (TPM_PCR_COMPOSITE, Signature)
--type Quote = Signed TPM_PCR_COMPOSITE

data Quote = Quote {
  pcrComposite :: TPM_PCR_COMPOSITE,
  qSig :: Signature
  } deriving (Eq{-Show-})
             
instance Show Quote where
  show (Quote p s) = "Quote {\npcrComposite = " ++ (take 200 (show p)) ++ "\"...} ,\n\n" ++ "qSig = " ++ (take 20 (show s)) ++ "\"..."

--instance Signable TPM_PUBKEY

--Response
data Response = Response {
  evPack :: EvidencePackage, 
  caCert :: CACertificate,
  quote :: Quote
  } deriving (Eq {-Show-})

instance Show Response where
  show (Response e c q) = "Response {\n\nevPack = " ++ (show e) ++ ",\n\ncaCert = " ++ (show c) ++ ",\n\nquote = " ++ (show q) ++ "\n}"
             
data EvidencePackage = EvidencePackage {
  evList :: Evidence, 
  eNonce :: TPM_NONCE,
  eSig :: Signature
  } deriving (Eq, Show)
             

    
type Evidence = [EvidencePiece]

data EvidencePiece = M0 M0Rep 
                   | M1 M1Rep
                   | M2 M2Rep 
                   | OK deriving (Eq, Ord, Show)

type M0Rep = ByteString
type M1Rep = ByteString
type M2Rep = ByteString

instance Binary EvidencePiece where
         put (M0 req) = do put (0::Word8);
                             put req;
         put(M1 quote) =  do put (1::Word8);
                               put quote;
         put(M2 res)= do put(2::Word8);
                           put res;
                                            
         get = do t<- get :: Get Word8
                  case t of
                    0 -> do req <- get
                            return (M0 req)
                    1 -> do quote <- get
                            return (M1 quote)
                    2 -> do res <- get
                            return (M2 res)
                         
       
ePack :: Evidence -> TPM_NONCE -> CACertificate -> ByteString
ePack e (TPM_NONCE n) cert = ePack' e `append` n `append` (encode cert) 

ePackSilly :: Evidence -> TPM_NONCE -> ByteString
ePackSilly e (TPM_NONCE n) = n `append` ePack' e   

--This is where we will need to convert measurement type to ByteString
-- if it is something else.  see comment below
ePack' :: Evidence -> ByteString
ePack'  = foldr f empty 
  where f (M0 x) y = x `append` y -- (i.e. (toByteString x) `append` y )
        f (M1 x) y = x `append` y
        f (M2 x) y = x `append` y          
        f (OK) y = y
         
         
      
type MakeIdResult = Signed TPM_IDENTITY_CONTENTS

type PlatformID = Int  
type SessionKey = ByteString --Is this helpful?
type Encrypted = ByteString --Is this helpful?

data CARequest = CARequest {
  pId :: PlatformID, 
  mkIdResult :: MakeIdResult
  } deriving ({-Show, -}Read)

instance Eq CARequest where 
 c1 == c2 = (pId c1) == (pId c2)
--TODO THIS IS WRONG
             
instance Show CARequest where
  show (CARequest p (Signed (TPM_IDENTITY_CONTENTS lab (TPM_PUBKEY parms dat)) _ )) = "CARequest {\n\npid = " ++ (show p) ++ ",\n\n" ++ "mkIdResult = " {-Signed-} ++ "TPM_IDENTITY_CONTENTS\n}" {-{dat = TPM_IDENTITY_CONTENTS {" ++ (show lab) ++ "\n identityPubKey = TPM_PUBKEY {" ++ (show parms) ++ (take 10 (show dat)) ++ "..." -}
             
data CAResponse = CAResponse {
  encCACert :: Encrypted, 
  encActIdInput :: Encrypted
  } deriving (Eq{-Show-})
             
instance Show CAResponse where
  show (CAResponse a b) = 
    let aShort = take 20 (show a) ++ "\"..."
        bShort = take 20 (show b) ++ "\"..." in
    "CAResponse {\n\n" ++ "CACert(encrypted with K)= " ++ aShort ++ ",\n\nActivateIdInput(encrypted with EK)= " ++ bShort ++ "\n}"

type CACertificate = Signed TPM_PUBKEY   

instance Show CACertificate where
  show _ = "Signed TPM_PUBKEY"{-(Signed  (TPM_PUBKEY parms dat) _ ) = "CACertificate {\n" ++ "TPM_PUBKEY {" ++ (show parms) ++ (take 10 (show dat)) ++ "..." -}

data ActivateIdRequest = ActivateIdRequest {
  sessKey :: SessionKey, 
  aikDigest :: TPM_DIGEST
  } deriving (Show)
                     

{-                  
--type Hash = ByteString
--type QuotePackage = (Quote, Hash, Signature)

doHash :: ByteString -> ByteString
doHash = hash

-}


--Boilerplate Binary instances(remove if there is an easier way to generate)--

instance Binary Quote where
  put(Quote a b) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return (Quote a b)

    
instance Binary MakeIdResult where
  put(Signed a b) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return (Signed a b)
    
instance Binary CACertificate where
  put(Signed a b) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return (Signed a b)
    
    
instance Binary Request where
  put(Request a b c) = do
    put a
    put b
    put c
  get = do
    a <- get
    b <- get
    c <- get
    return $ Request a b c
    
instance Binary Response where
  put(Response a b c) = do
    put a
    put b
    put c
  get = do
    a <- get
    b <- get
    c <- get
    return $ Response a b c
    
instance Binary EvidencePackage where
  put(EvidencePackage a b c) = do
    put a
    put b
    put c
  get = do
    a <- get
    b <- get
    c <- get
    return $ EvidencePackage a b c
    
             
instance Binary  CARequest where
  put(CARequest a b ) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return $ CARequest a b 
             
instance Binary  CAResponse where
  put(CAResponse a b ) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return $ CAResponse a b 
    
instance Binary ActivateIdRequest where
  put(ActivateIdRequest a b ) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return $ ActivateIdRequest a b 
    
    
    
    
-- JSON stuff!

--Request Things first
   --toJSON
jsonEncode :: (ToJSON a) => a -> ByteString
jsonEncode = DA.encode

jsonEitherDecode :: (FromJSON a) => ByteString -> Either String a
jsonEitherDecode = DA.eitherDecode

jsonDecode :: (FromJSON a) => ByteString -> Maybe a
jsonDecode= DA.decode
instance ToJSON Request where
  toJSON (Request {..}) = object [ "DesiredEvidence"    .= toJSON desiredE
						      , "TPM_PCR_SELECTION" .= toJSON pcrSelect
			  		
	      , "TPM_NONCE" .= toJSON nonce
						      ]
instance FromJSON Request where
  parseJSON (DA.Object o) = Request <$> o .: "DesiredEvidence"
  				 <*> o .: "TPM_PCR_SELECTION"
  				 <*> o .: "TPM_NONCE"
instance ToJSON EvidenceDescriptor where
	toJSON D0 = DA.String "D0"
	toJSON D1 = DA.String "D1"
	toJSON D2 = DA.String "D2"
	toJSON DONE = DA.String "DONE"
instance FromJSON EvidenceDescriptor where
	parseJSON (DA.String "D0")   = pure D0
	parseJSON (DA.String "D1")   = pure D1
	parseJSON (DA.String "D2")   = pure D2
	parseJSON (DA.String "DONE") = pure DONE

instance ToJSON TPM_PCR_SELECTION where
	toJSON (TPM_PCR_SELECTION bs) = object [ "TPM_PCR_SELECTION" .= encodeToText (toStrict bs) ]
instance FromJSON TPM_PCR_SELECTION where
	parseJSON (DA.Object o) = TPM_PCR_SELECTION <$> ((o .: "TPM_PCR_SELECTION") >>= decodeFromTextL) 
		
instance ToJSON TPM_NONCE where
  toJSON (TPM_NONCE n) = object ["TPM_NONCE" .= encodeToText (toStrict n)]
instance FromJSON TPM_NONCE where
	parseJSON (DA.Object o) = TPM_NONCE <$> ((o .: "TPM_NONCE") >>= decodeFromTextL) 
 
--Reponse stuff
instance ToJSON Response where
	toJSON (Response {..} ) = object [ "EvidencePackage"    .= toJSON evPack
						      , "CACertificate" .= toJSON caCert
			  			      , "Quote" .= toJSON quote
						      ]  
instance FromJSON Response where
	parseJSON (DA.Object o) = Response <$> o .: "EvidencePackage"
				           <*> o .: "CACertificate"
					   <*> o .: "Quote"
instance ToJSON Quote where
	toJSON (Quote {..}) = object [ "TPM_PCR_COMPOSITE" .= toJSON pcrComposite
				     , "Signature" .= encodeToText (toStrict qSig)
				     ]	
instance FromJSON Quote where 
	parseJSON (DA.Object o) = Quote <$> o .: "TPM_PCR_COMPOSITE"
					<*> ((o .: "Signature") >>= decodeFromTextL)	
instance ToJSON TPM_PCR_COMPOSITE where
	toJSON (TPM_PCR_COMPOSITE {..}) = object [ "TPM_PCR_SELECTION" .= toJSON tpmPcrCompositeSelection
						 , "TPM_PCRVALUEs" .= toJSON tpmPcrCompositePcrs
						 ]
instance FromJSON TPM_PCR_COMPOSITE where
	parseJSON (DA.Object o) = TPM_PCR_COMPOSITE <$> o .: "TPM_PCR_SELECTION"
						    <*> o .: "TPM_PCRVALUEs"						 
						 
--instance ToJSON TPM_PCRVALUE where TPM_PCRVALUE is a synonym for TPM_DIGEST
instance ToJSON TPM_DIGEST where
	toJSON (TPM_DIGEST bs) = object [ "TPM_DIGEST" .= encodeToText (toStrict bs) ]
instance FromJSON TPM_DIGEST where
	parseJSON (DA.Object o) = TPM_DIGEST <$> ((o .: "TPM_DIGEST") >>= decodeFromTextL)	
{-
data TPM_PCR_COMPOSITE = TPM_PCR_COMPOSITE {
      tpmPcrCompositeSelection :: TPM_PCR_SELECTION
    , tpmPcrCompositePcrs      :: [TPM_PCRVALUE]
    } deriving (Show,Eq, Read)-}					     			      
instance ToJSON EvidencePackage where
	toJSON (EvidencePackage {..}) = object [ "Evidence"    .= toJSON evList
					       , "TPM_NONCE" .= toJSON  eNonce
			  		       , "Signature" .= encodeToText  (toStrict eSig)
					       ]
instance FromJSON EvidencePackage where
	parseJSON (DA.Object o) = EvidencePackage <$> o .: "Evidence"
						  <*> o .: "TPM_NONCE"
						  <*> ((o .: "Signature") >>= decodeFromTextL)					       
instance ToJSON EvidencePiece where
	toJSON (M0 rep0) = object [ "M0" .= encodeToText (toStrict rep0) ]
	toJSON (M1 rep1) = object [ "M1" .= encodeToText (toStrict rep1) ]
	toJSON (M2 rep2) = object [ "M2" .= encodeToText (toStrict rep2) ]
	toJSON OK = DA.String "OK"
instance FromJSON EvidencePiece where
	parseJSON (DA.Object o) | HM.member "M0" o = M0 <$> ((o .: "M0") >>= decodeFromTextL)
				| HM.member "M1" o = M1 <$> ((o .: "M1") >>= decodeFromTextL)
				| HM.member "M2" o = M2 <$> ((o .: "M2") >>= decodeFromTextL)
	parseJSON (DA.String "OK") = pure OK
	
--instance ToJSON CACertificate where just a type synonym. of Signed TPM_PUB_KEY
instance (ToJSON a)=> ToJSON (Signed a) where
	toJSON (Signed {..}) = object [ "Signed" .= toJSON dat
				      , "Signature" .= encodeToText (toStrict sig)
				      ]
instance (FromJSON a) => FromJSON (Signed a) where
	parseJSON (DA.Object o) = Signed <$> o .: "Signed"
					 <*> ((o .: "Signature") >>= decodeFromTextL)				      
instance ToJSON TPM_PUBKEY where
	toJSON (TPM_PUBKEY {..}) = object [ "TPM_KEY_PARMS" .= toJSON tpmPubKeyParams
					  , "TPM_STORE_PUBKEY" .= toJSON tpmPubKeyData
					  ]
instance FromJSON TPM_PUBKEY where
	parseJSON (DA.Object o) = TPM_PUBKEY <$> o .: "TPM_KEY_PARMS"
					     <*> o .: "TPM_STORE_PUBKEY" 					  
instance ToJSON TPM_KEY_PARMS where
	toJSON TPM_KEY_PARMS {..} = object [ "TPM_ALGORITHM_ID" .= toJSON tpmKeyParamAlg --word32
					   , "TPM_ENC_SCHEME" .= toJSON tpmKeyParamEnc --word16
					   , "TPM_SIG_SCHEME" .= toJSON tpmKeyParamSig  --word16
					   , "TPM_KEY_PARMS_DATA" .= toJSON tpmKeyParamData
					   ]
instance FromJSON TPM_KEY_PARMS where
	parseJSON (DA.Object o) = TPM_KEY_PARMS <$> o .: "TPM_ALGORITHM_ID"
						<*> o .: "TPM_ENC_SCHEME"
						<*> o .: "TPM_SIG_SCHEME"
						<*> o .: "TPM_KEY_PARMS_DATA"			   
{-
data TPM_KEY_PARMS_DATA = RSA_DATA TPM_RSA_KEY_PARMS
                        | AES_DATA TPM_SYMMETRIC_KEY_PARMS
                        | NO_DATA 
                        deriving (Eq, Read, Show)					    -}
						                          
instance ToJSON TPM_KEY_PARMS_DATA where
	toJSON (RSA_DATA tpm_RSA_KEY_PARMS) = object [ "RSA_DATA" .= toJSON tpm_RSA_KEY_PARMS ]						     
	toJSON (AES_DATA tpm_SYMMETRIC_KEY_PARMS) = object [ "AES_DATA" .= toJSON tpm_SYMMETRIC_KEY_PARMS ] 
	toJSON (NO_DATA) = DA.String "NO_DATA"
instance FromJSON TPM_KEY_PARMS_DATA where
	parseJSON (DA.Object o)	| HM.member "RSA_DATA" o = RSA_DATA  <$> o .: "RSA_DATA"
				| HM.member "AES_DATA" o = AES_DATA <$> o .: "AES_DATA"
				| HM.member "NO_DATA" o = pure NO_DATA
	
--DA.Object = HaskMap Text Value
	
	
	
instance ToJSON TPM_RSA_KEY_PARMS where
	toJSON (TPM_RSA_KEY_PARMS {..}) = object [ "tpmRsaKeyLength" .= toJSON tpmRsaKeyLength
						 , "tpmRsaKeyPrimes" .= toJSON tpmRsaKeyPrimes
						 , "tpmRsaKeyExp" .= encodeToText (toStrict tpmRsaKeyExp)
						 ]
instance FromJSON TPM_RSA_KEY_PARMS where
	parseJSON (DA.Object o) = TPM_RSA_KEY_PARMS <$> o .: "tpmRsaKeyLength"
						    <*> o .: "tpmRsaKeyPrimes"
						    <*> ((o .: "tpmRsaKeyExp") >>= decodeFromTextL)			 
instance ToJSON TPM_SYMMETRIC_KEY_PARMS where
	toJSON (TPM_SYMMETRIC_KEY_PARMS {..}) = object [ "tpmSymKeyLength" .= toJSON tpmSymKeyLength
					 	       , "tpmSymKeyBlockSize" .= toJSON tpmSymKeyBlockSize
					 	       , "tpmSymKeyIV" .= encodeToText (toStrict tpmSymKeyIV)
					 	       ]
instance FromJSON TPM_SYMMETRIC_KEY_PARMS where
	parseJSON (DA.Object o) = TPM_SYMMETRIC_KEY_PARMS <$>  o .: "tpmSymKeyLength"
							  <*> o .:  "tpmSymKeyBlockSize"
							  <*> ((o .: "tpmSymKeyIV") >>= decodeFromTextL)
	
	{-<$> o .: "DesiredEvidence"
  				 <*> o .: "TPM_PCR_SELECTION"
  				 <*> o .: "TPM_NONCE" -}				 	       
instance ToJSON TPM_STORE_PUBKEY where
	toJSON (TPM_STORE_PUBKEY bs) = object [ "TPM_STORE_PUBKEY" .= encodeToText (toStrict bs) ]
testpubkey = TPM_STORE_PUBKEY $ fromStrict $ Char8.pack "3434"
instance FromJSON TPM_STORE_PUBKEY where
	parseJSON (DA.Object o) = TPM_STORE_PUBKEY <$> ((o .: "TPM_STORE_PUBKEY") >>= decodeFromTextL) 
--34 instances to this point
{-
type PlatformID = Int  
type SessionKey = ByteString --Is this helpful?
type Encrypted = ByteString --Is this helpful?

data CARequest = CARequest {
  pId :: PlatformID, 
  mkIdResult :: MakeIdResult
  } deriving (Show)
data CARequest = CARequest {
  pId :: PlatformID, 
  mkIdResult :: MakeIdResult
  } deriving (Show)
             
data CAResponse = CAResponse {
  encCACert :: Encrypted, 
  encActIdInput :: Encrypted
  } deriving (Show)

type CACertificate = Signed TPM_PUBKEY 

-}
--CA Data types
instance ToJSON CARequest where
	toJSON (CARequest{..}) = object [ "PlatformID" .= toJSON pId
					, "MakeIdResult" .= toJSON mkIdResult
				        ]
instance FromJSON CARequest where
	parseJSON (DA.Object o) = CARequest <$> o .: "PlatformID"
					    <*> o .: "MakeIdResult"

instance ToJSON TPM_IDENTITY_CONTENTS where
	toJSON (TPM_IDENTITY_CONTENTS {..}) = object [ "labelPrivCADigest" .= toJSON labelPrivCADigest --this is just TPM_Digest again
						     , "identityPubKey" .= toJSON identityPubKey   --did this one too.
						     ]
instance FromJSON TPM_IDENTITY_CONTENTS where
	parseJSON (DA.Object o) = TPM_IDENTITY_CONTENTS <$> o .: "labelPrivCADigest"
							<*> o .: "identityPubKey"
--instance ToJSON 
	
--type MakeIdResult = Signed TPM_IDENTITY_CONTENTS
{-data TPM_IDENTITY_CONTENTS = TPM_IDENTITY_CONTENTS {
  labelPrivCADigest :: TPM_CHOSENID_HASH,
  identityPubKey :: TPM_PUBKEY
  }  deriving (Show)
 -}

instance ToJSON CAResponse where
	toJSON (CAResponse {..}) = object [ "encCACert" .= encodeToText (toStrict encCACert)
					 , "encActIdInput" .= encodeToText (toStrict encActIdInput)
 		 	       		 ]
instance FromJSON CAResponse where
	parseJSON (DA.Object o) = CAResponse <$> ((o .: "encCACert") >>= decodeFromTextL) 
					     <*> ((o .: "encActIdInput") >>= decodeFromTextL) 
	{-
ata TPM_SYMMETRIC_KEY_PARMS = TPM_SYMMETRIC_KEY_PARMS {
      tpmSymKeyLength    :: UINT32
    , tpmSymKeyBlockSize :: UINT32
    , tpmSymKeyIV        :: ByteString
    } deriving (Show,Eq, Read) -}						   
{-data TPM_RSA_KEY_PARMS = TPM_RSA_KEY_PARMS {
      tpmRsaKeyLength  :: UINT32
    , tpmRsaKeyPrimes  :: UINT32
    , tpmRsaKeyExp     :: ByteString
    } deriving (Show,Eq, Read)-}
    
    	{-data Shared = Appraisal Request
              | Attestation Response
              | Result Bool-}

--PortRequest Entity  HttpClient.Port
--		 | VChanRequest Entity Int

{- instance ToJSON PortRequest where
	toJSON (PortRequest port entity) = object [ "port" .= toJSON port
						  , "entity" .= toJSON entity
						  ]-}
instance ToJSON B.ByteString where
	toJSON = DA.String . encodeToText
instance FromJSON B.ByteString where
	parseJSON (DA.String str) = pure $ decodeFromText str	
				 		         
encodeToText :: B.ByteString -> T.Text
encodeToText = TE.decodeUtf8 . Base64.encode

decodeFromText :: T.Text -> B.ByteString
decodeFromText = {-either fail return .-} Base64.decodeLenient . TE.encodeUtf8

decodeFromTextL :: (Monad m) => T.Text -> m ByteString
decodeFromTextL x = let bs = decodeFromText x in
		       return (fromStrict bs)  

decodeFromTextLStayStrict :: (Monad m) => T.Text -> m B.ByteString
decodeFromTextLStayStrict x = let bs = decodeFromText x in
		       return (bs)  


decodeFromTextL' :: T.Text -> ByteString
decodeFromTextL' x = let bs = decodeFromText x in
		       fromStrict bs  
-- {-




