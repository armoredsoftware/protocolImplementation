{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}

module ProtoTypesA where



import Data.ByteString.Lazy
import Data.Binary
import VChanUtil
import Codec.Crypto.RSA hiding (sign, verify)
import TPM.Types
import qualified TPM.Types as TPM


{-import ProtoTypes(Channel)

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import Demo3Shared hiding (dat,Signature, EvidenceDescriptor,Evidence,sig)
import qualified Data.Aeson as DA (Value(..), encode, decode, eitherDecode) --for JSON stuff
import Control.Applicative ( (<$>), (<*>), pure )                           --for JSON stuff
import qualified Data.HashMap.Strict as HM (member, lookup)                 --for JSON stuff
import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )--for JSON stuff -}


import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict) --for JSON stuff

--Abstract entity identifier.  The id assignments are LOCAL to the particular protocol being represented.
type EntityId = Int

type Nonce = Int

data TPM_DATA =
  TdTPM_IDENTITY_CONTENTS  TPM_IDENTITY_CONTENTS
  | TdTPM_KEY_HANDLE TPM_KEY_HANDLE


              deriving (Show)




{-instance ToJSON ArmoredData where
  toJSON (ANonce n) = object
    ["ANonce" .= n]
  toJSON(ACipherText ct) = object
    ["ACipherText" .= encodeToText (toStrict ct)]
  toJSON (ATPM_PCR_SELECTION pcrSel) = object
    ["ATPM_PCR_SELECTION" .= toJSON pcrSel]
  toJSON (ATPM_PCR_COMPOSITE pcrC) = object
    ["ATPM_PCR_COMPOSITE" .= toJSON pcrC]
  toJSON (ATPM_IDENTITY_CONTENTS c) = object
    ["ATPM_IDENTITY_CONTENTS" .= toJSON c]
  toJSON (ATPM_PUBKEY k) = object
    ["ATPM_PUBKEY" .= toJSON k]
  toJSON (ASignedData sd) = object
    ["ASignedData" .= toJSON sd]
  toJSON (ASignature s) = object
    ["ASignature" .= encodeToText (toStrict s)]
  toJSON (AEvidenceDescriptor ed) = object
    ["AEvidenceDescriptor" .= toJSON ed]
  toJSON (AEvidence e) = object
    ["AEvidence" .= toJSON e]
  toJSON (AFailure s) = object
    ["AFailure" .= s]

instance FromJSON ArmoredData where
  parseJSON (DA.Object o)  | HM.member "ANonce" o = ANonce <$> o .: "ANonce"
                          | HM.member "ACipherText" o = ACipherText <$> ((o .: "ACipherText") >>= decodeFromTextL)
                          | HM.member "ATPM_PCR_SELECTION"o  = ATPM_PCR_SELECTION <$> o .: "ATPM_PCR_SELECTION"
                          | HM.member "ATPM_PCR_COMPOSITE" o = ATPM_PCR_COMPOSITE <$> o .: "ATPM_PCR_COMPOSITE"
                          | HM.member "ATPM_IDENTITY_CONTENTS" o = ATPM_IDENTITY_CONTENTS <$> o .: "ATPM_IDENTITY_CONTENTS"
                          | HM.member "ATPM_PUBKEY" o = ATPM_PUBKEY <$> o .: "ATPM_PUBKEY"
                          | HM.member "ASignedData" o = ASignedData <$> o .: "ASignedData"
                          | HM.member "ASignature" o = ASignature <$> ((o .: "ASignature" ) >>= decodeFromTextL)
                          | HM.member "AEvidenceDescriptor" o = AEvidenceDescriptor <$> o .: "AEvidenceDescriptor"
                          | HM.member "AEvidence" o = AEvidence <$> o .: "AEvidence"
                          | HM.member "AFailure" o = AFailure <$> o .: "AFailure"
-}



--TODO:  Should the following "Command" items be message items(ArmoredData) that must be evaluated in the monad prior to sending?  For now, they are implemented as seperate explicit monadic function calls.
--Common data that is sent or received by an armored entity.
data ArmoredData =
  ANonce Nonce
  | AEntityInfo EntityInfo
  | ACipherText CipherText
 -- | ATPM_DATA TPM_DATA
  | ATPM_PCR_SELECTION TPM_PCR_SELECTION
  | ATPM_PCR_COMPOSITE TPM_PCR_COMPOSITE
  | ATPM_IDENTITY_CONTENTS TPM_IDENTITY_CONTENTS
  | ATPM_PUBKEY TPM_PUBKEY
  | ASignedData (SignedData ArmoredData)
  | ASignature Signature
  | AEvidenceDescriptor EvidenceDescriptor
  | AEvidence Evidence
  | AFailure String deriving (Eq,Show)
{-| GenNonce
  | Encrypt [ArmoredData]
  | Decrypt CipherText -}

--Binary instance necessary for communication, encryption, and decryption(or a combination of these).
instance Binary ArmoredData where
  put (ANonce n) = do put (0::Word8)
                      put n
  --put (AEntityId id) = do put (1::Word8)
                      --    put id
  put (ACipherText ct) = do put(2::Word8)
                            put ct
  put (AEntityInfo einfo) =
    do
      put(3::Word8)
      put einfo
  put (AEvidenceDescriptor e) =
    do
      put (4::Word8)
      put e
  put(ATPM_PCR_SELECTION s) =
    do
      put(5::Word8)
      put s
  put(ATPM_PUBKEY p) =
    do
      put(6::Word8)
      put p
  put(ASignedData s) =
    do
      put(7::Word8)
      put s
  put(ASignature a) =
    do
      put(8::Word8)
      put a
  put(ATPM_PCR_COMPOSITE p) =
    do
      put(9::Word8)
      put p
  put(ATPM_IDENTITY_CONTENTS i) =
    do
      put(10::Word8)
      put i
  put(AEvidence e) =
    do
      put(11::Word8)
      put e

  get = do t <- get :: Get Word8
           case t of
             0 -> do n <- get
                     return $ ANonce n
            -- 1 -> do id <- get
                 --    return $ AEntityId id
             2 -> do ct <- get
                     return $ ACipherText ct
             3 -> do einfo <- get
                     return $ AEntityInfo einfo

             4 -> do e <- get
                     return $ AEvidenceDescriptor e
             5 -> do s <- get
                     return $ ATPM_PCR_SELECTION s
             6 -> do s <- get
                     return $ ATPM_PUBKEY s
             7 -> do s <- get
                     return $ ASignedData s
             8 -> do s <- get
                     return $ ASignature s
             9 -> do s <- get
                     return $ ATPM_PCR_COMPOSITE s
             10 -> do s <- get
                      return $ ATPM_IDENTITY_CONTENTS s
             11 -> do s <- get
                      return $ AEvidence s


type Message = [ArmoredData]


--This shoud contain the concrete info that is necessary to communicate with an entity
data EntityInfo = EntityInfo {
  entityName :: String,
  entityIp :: Int,
  chan :: LibXenVChan
} deriving (Eq, Show)

instance Binary EntityInfo where
  put (EntityInfo name ip vchan) =
    do
      put name
      put ip

  get = do name <- get
           ip <- get
           let vchan = undefined --vchan <- get  --IS THIS OK??
           return $ EntityInfo name ip vchan


type PrivateKey = Codec.Crypto.RSA.PrivateKey --ByteString;
type PublicKey = Codec.Crypto.RSA.PublicKey --ByteString;

type SymmKey = TPM_SYMMETRIC_KEY
--Encrypted text
type CipherText = ByteString;

type EvidenceDescriptor = [Int]
type Evidence = [Int]

type Signature = ByteString;
data SignedData a = SignedData {
  dat :: a,
  sig :: Signature
} deriving (Eq, Show)

{-instance (ToJSON a) => ToJSON (SignedData a) where
  toJSON (s) = object
    ["dat" .= toJSON (dat s)
    , "sig" .= encodeToText (toStrict (sig s))
    ]
instance (FromJSON a) => FromJSON (SignedData a) where
  parseJSON (DA.Object o) = SignedData <$> o .: "dat"
                                       <*> ((o .: "sig") >>= decodeFromTextL)
-}

instance (Binary a) => Binary (SignedData a) where
  put (SignedData a b) =
    do
      put a
      put b

  get = do a <- get
           b <- get
           return $ SignedData a b

type AikContents = SignedData TPM_IDENTITY_CONTENTS
