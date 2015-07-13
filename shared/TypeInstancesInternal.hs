{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module TypeInstancesInternal where

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
--import Demo3Shared hiding (Result, sig, dat, Signature, Evidence, EvidenceDescriptor)
import Control.Monad
import Control.Monad.State.Strict hiding (get, put)
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
import TPM
import TPM.Types
import qualified Network.Http.Client as HttpClient
--import qualified Demo3Shared as Demo3
import VChanUtil

import TypesInternal

{-
import qualified Data.ByteString as B (ByteString)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
-}
import ByteStringJSON





import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import Web.Scotty hiding (get, put)
import qualified Data.Aeson as DA (Value(..), encode, decode, eitherDecode) --for JSON stuff
import Control.Applicative ( (<$>), (<*>), pure )                           --for JSON stuff
import qualified Data.HashMap.Strict as HM (member, lookup)                 --for JSON stuff
import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )--for JSON stuff
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict) --for JSON stuff
import Control.Concurrent (ThreadId)
import Data.Binary
import Codec.Crypto.RSA hiding (sign, verify)

instance ToJSON FormalRequest where
       toJSON (FormalRequest ent nreq) = object
                   [ "Entity" .= toJSON ent
                   , "NRequest" .= toJSON nreq
                   ]
instance FromJSON FormalRequest where
       parseJSON (DA.Object o) = FormalRequest <$> o .: "Entity"
                                               <*> o .: "NRequest"
instance ToJSON NRequest where
       toJSON (ProtoNum i) = object
                   [ "ProtoNum" .= DA.String "NRequest"
                   , "Num"      .= i
                   ]
       toJSON (Process proc)  = object
                   [ "Process" .= DA.String "NRequest"
                   , "Proc"    .= DA.String "TODO"
                   ]
       toJSON (RequestItem item property) = object
                   [ "RequestItem" .= DA.String "NRequest"
                   , "Item"        .= toJSON item
                   , "Property"    .= toJSON property
                   ]
       toJSON (ReqLS ls) = object
                   [ "ReqLS" .= DA.String "NRequest"
                   , "Requests" .= toJSON ls
                   ]
       toJSON (TierRequest ls) = object
                   [ "TierRequest" .= DA.String "NRequest"
                   , "Requests"    .= toJSON ls
                   ]
instance FromJSON NRequest where
         parseJSON (DA.Object o) | HM.member "ProtoNum" o    = ProtoNum    <$> o .: "Num"
                                 -- | HM.member "Process" o     = Process     <$> o .: "Proc" --TODO implement to/fro JSON for Process... :D
                                 | HM.member "RequestItem" o = RequestItem <$> o .: "Item"
                                                                           <*> o .: "Property"
                                 | HM.member "ReqLS" o       = ReqLS       <$> o .: "Requests"
                                 | HM.member "TierRequest" o = TierRequest <$> o .: "Requests"

instance ToJSON NResponse where
         toJSON No = object
                ["No" .= DA.String "NResponse"]
         toJSON (Measurement item prop val) = object
                ["Measurement" .= DA.String "NResponse"
                , "Item"       .= toJSON item
                , "Property"   .= toJSON prop
                , "Value"      .= toJSON val
                ]
         toJSON (CounterOffer ls) = object
                ["CounterOffer"     .= DA.String "NResonse"
                , "CounterOfferLS" .=  toJSON ls
                ] {-
                , "Request"          .= toJSON req
                ]-}
         toJSON (RespLS responselS) = object
                ["RespLS"         .= DA.String "NResponse"
                , "Responses"     .= toJSON responselS
                ]
instance FromJSON NResponse where
         parseJSON (DA.Object o) | HM.member "No" o = pure No
                                 | HM.member "Measurement" o = Measurement <$> o .: "Item"
                                                                           <*> o .: "Property"
                                                                           <*> o .: "Value"
                                 | HM.member "CounterOffer" o = CounterOffer <$> o .: "CounterOfferLS"
                                                                   -- <*> o .: "Request"
                                 | HM.member "RespLS" o = RespLS   <$> o .: "Responses"

instance ToJSON Item where
         toJSON OS = object
                ["OS" .= DA.String "Item"]
         toJSON VC = object
                ["VC" .= DA.String "Item"]
         toJSON (PCR intLS) = object
                [ "PCR" .= DA.String "Item"
                , "Ints" .= toJSON intLS
                ]
         toJSON EntityProp = object
                ["EntityProp" .= DA.String "Item"]
         toJSON ProtocolItem = object
                ["ProtocolItem" .= DA.String "Item"]

instance FromJSON Item where
         parseJSON (DA.Object o) | HM.member "OS" o = pure OS
                                 | HM.member "VC" o = pure VC
                                 | HM.member "PCR" o = PCR <$> o .: "Ints"
                                 | HM.member "ID" o = pure ID
                                 | HM.member "EntityProp" o = pure EntityProp
                                 | HM.member "ProtocolItem" o = pure ProtocolItem

instance ToJSON Property where
         toJSON Name = object
                [ "Name" .= DA.String "Property"]
         toJSON Version = object
                [ "Version" .= DA.String "Property"]
         toJSON (IntProperty i) = object
                [ "IntProperty" .= i ]
instance FromJSON Property where
         parseJSON (DA.Object o) | HM.member "Name" o = pure Name
                                 | HM.member "Version" o = pure Version
                                 | HM.member "IntProperty" o = IntProperty <$> o .: "IntProperty"
instance ToJSON PrivacyRule where
         toJSON (Reveal itemProplsLS condition) = object
                ["Reveal" .= DA.String "PrivacyRule"
                , "ItemPropertylsLS" .= toJSON itemProplsLS
                , "Condition"        .= toJSON condition
                ]
instance FromJSON PrivacyRule where
         parseJSON (DA.Object o) | HM.member "Reveal" o = Reveal <$> o .: "ItemPropertylsLS"
                                                                 <*> o .: "Condition"

instance ToJSON Condition where
         toJSON (Equals item prop val) = object
                [ "Equals" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ]
         toJSON (OneOf item prop valLS) = object
                [ "OneOf" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Values"    .= toJSON valLS
                ]
         toJSON (NoneOf item prop valLS) = object
                [ "NoneOf" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Values"    .= toJSON valLS
                ]
         toJSON (GTV item prop val) = object
                [ "GTV" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ]
         toJSON (LTV item prop val) = object
                [ "LTV" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ]
         toJSON (GTETV item prop val) = object
                [ "GTETV" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ]
         toJSON (LTETV item prop val) = object
                [ "LTETV" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ]
         toJSON (Or c1 c2) = object
                [ "Or" .= DA.String "Condition"
                , "Condition1" .= toJSON c1
                , "Condition2" .= toJSON c2
                ]
         toJSON (And c1 c2) = object
                [ "And" .= DA.String "Condition"
                , "Condition1" .= toJSON c1
                , "Condition2" .= toJSON c2
                ]
instance FromJSON Condition where
         parseJSON (DA.Object o) | HM.member "Equals" o = Equals <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "OneOf" o = OneOf   <$> o .: "Item"
                                                                 <*> o .:  "Property"
                                                                 <*> o .:  "Values"
                                 | HM.member "NoneOf" o = NoneOf <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Values"
                                 | HM.member "GTV" o    = GTV    <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "LTV" o    = LTV    <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "GTETV" o    = GTETV <$> o  .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "LTETV" o    = LTETV <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "Or" o = Or <$> o .: "Condition1"
                                                         <*> o .: "Condition2"
                                 | HM.member "And" o = And <$> o .: "Condition1"
                                                           <*> o .: "Condition2"

instance ToJSON Value where
         toJSON (ValString str) = object
                ["ValString" .= DA.String "Value"
                , "String"   .= toJSON str
                ]
         toJSON (ValInt int) = object
                ["ValInt" .= DA.String "Value"
                , "Int"   .= toJSON int
                ]
         toJSON (ValDouble doub) = object
                ["ValDouble" .= DA.String "Value"
                , "Double"   .= toJSON doub
                ]
         toJSON (ValBool bool) = object
                ["ValBool" .= DA.String "Value"
                , "Bool"   .= toJSON bool
                ]
         toJSON (ValByteString bs) = object
                ["ValByteString" .= DA.String "Value"
                , "ByteString"   .= encodeToText (toStrict bs)
                ]

instance FromJSON Value where
         parseJSON (DA.Object o) | HM.member "ValString" o = ValString <$> o .: "String"
                                 | HM.member "ValInt" o = ValInt <$> o .: "Int"
                                 | HM.member "ValDouble" o = ValDouble <$> o .: "Double"
                                 | HM.member "ValBool" o = ValBool <$> o .: "Bool"
                                 | HM.member "ValByteString" o = ValByteString <$> ((o .: "ByteString") >>= decodeFromTextL)

instance ToJSON ArmoredConfig where
         toJSON (ArmoredConfig req condition pripo) = object
                        [ "ArmoredConfig" .= DA.String "ArmoredConfig"
                        , "Request"       .= toJSON req
                        , "Condition"     .= toJSON condition
                        , "PrivacyPolicy" .= toJSON pripo
                        ]
instance FromJSON ArmoredConfig where
         parseJSON (DA.Object o) | HM.member "ArmoredConfig" o = ArmoredConfig <$> o .: "Request"
                                                                               <*> o .: "Condition"
                                                                               <*> o .: "PrivacyPolicy"

instance ToJSON CommRequest where
	toJSON (PortRequest entity port nonce) =  object
		   ["PortRequest" .= DA.String "CommRequest"
		   , "Entity" .= toJSON entity
		   , "Port"   .= port
		   , "Nonce"  .= nonce
		   ]
	toJSON (VChanRequest entity nonce) = object [ "VChanRequest" .= DA.String "CommRequest"
					      , "Entity"       .= toJSON entity
					      , "Nonce"	       .= nonce
					      ]
instance FromJSON CommRequest where
	parseJSON (DA.Object o) | HM.member "PortRequest" o = PortRequest <$> o .: "Entity"
									  <*> o .: "Port"
									  <*> o .: "Nonce"
				| HM.member "VChanRequest" o = VChanRequest <$> o .: "Entity"
									    <*> o .: "Nonce"
instance ToJSON Entity where
	toJSON (Entity name mip mid role mnote) = object [ "EntityName" .= name
						         , "EntityIp"   .= toJSON mip
						         , "EntityId"   .= mid
						         , "EntityRole" .= toJSON role
						         , "EntityNote" .= mnote
						         ]
instance FromJSON Entity where
	parseJSON (DA.Object o) = Entity <$> o .: "EntityName"
					 <*> o .: "EntityIp"
					 <*> o .: "EntityId"
					 <*> o .: "EntityRole"
					 <*> o .: "EntityNote"

instance ToJSON Role where
	toJSON Appraiser = DA.String "Appraiser"
	toJSON Attester  = DA.String "Attester"
	toJSON Measurer  = DA.String "Measurer"
        toJSON PrivacyCA = DA.String "PrivacyCA"
        toJSON Undetermined = DA.String "Undetermined"

instance FromJSON Role where
	parseJSON (DA.String "Appraiser") = pure Appraiser
	parseJSON (DA.String "Attester")  = pure Attester
	parseJSON (DA.String "Measurer")  = pure Measurer
	parseJSON (DA.String "PrivacyCA") = pure PrivacyCA
        parseJSON (DA.String "Undetermined") = pure Undetermined

instance ToJSON ArmoredData where
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
  toJSON (AAEvidenceDescriptor ed) = object
    ["AAEvidenceDescriptor" .= toJSON ed]
  toJSON (AEvidence e) = object
    ["AEvidence" .= toJSON e]
  toJSON (AAFailure s) = object
    ["AAFailure" .= s]

instance FromJSON ArmoredData where
  parseJSON (DA.Object o)  | HM.member "ANonce" o = ANonce <$> o .: "ANonce"
                          | HM.member "ACipherText" o = ACipherText <$> ((o .: "ACipherText") >>= decodeFromTextL)
                          | HM.member "ATPM_PCR_SELECTION"o  = ATPM_PCR_SELECTION <$> o .: "ATPM_PCR_SELECTION"
                          | HM.member "ATPM_PCR_COMPOSITE" o = ATPM_PCR_COMPOSITE <$> o .: "ATPM_PCR_COMPOSITE"
                          | HM.member "ATPM_IDENTITY_CONTENTS" o = ATPM_IDENTITY_CONTENTS <$> o .: "ATPM_IDENTITY_CONTENTS"
                          | HM.member "ATPM_PUBKEY" o = ATPM_PUBKEY <$> o .: "ATPM_PUBKEY"
                          | HM.member "ASignedData" o = ASignedData <$> o .: "ASignedData"
                          | HM.member "ASignature" o = ASignature <$> ((o .: "ASignature" ) >>= decodeFromTextL)
                          | HM.member "AAEvidenceDescriptor" o = AAEvidenceDescriptor <$> o .: "AAEvidenceDescriptor"
                          | HM.member "AEvidence" o = AEvidence <$> o .: "AEvidence"
                          | HM.member "AAFailure" o = AAFailure <$> o .: "AAFailure"

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
  put (AAEvidenceDescriptor e) =
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
                     return $ AAEvidenceDescriptor e
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

instance Binary EntityInfo where
  put (EntityInfo name ip vchan) =
    do
      put name
      put ip

  get = do name <- get
           ip <- get
           let vchan = undefined --vchan <- get  --IS THIS OK??
           return $ EntityInfo name ip vchan

instance (ToJSON a) => ToJSON (SignedData a) where
  toJSON (s) = object
    ["dat" .= toJSON (dat s)
    , "sig" .= encodeToText (toStrict (sig s))
    ]
instance (FromJSON a) => FromJSON (SignedData a) where
  parseJSON (DA.Object o) = SignedData <$> o .: "dat"
                                       <*> ((o .: "sig") >>= decodeFromTextL)


instance (Binary a) => Binary (SignedData a) where
  put (SignedData a b) =
    do
      put a
      put b

  get = do a <- get
           b <- get
           return $ SignedData a b


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
