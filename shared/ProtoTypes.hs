{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module ProtoTypes where

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import Demo3Shared hiding (Result, sig, dat, Signature, Evidence, EvidenceDescriptor)
import Control.Monad
import Control.Monad.State.Strict hiding (get, put)
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
import TPM
import qualified Network.Http.Client as HttpClient
import qualified Demo3Shared as Demo3
import VChanUtil
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


data FormalRequest = FormalRequest Entity NRequest deriving (Show)

data NRequest = ProtoNum Int
              | Process Process
              | RequestItem Item Property
              | ReqLS [NRequest]
              | TierRequest [NRequest] deriving (Eq, Show)
data NResponse = No
               | Measurement Item Property Value
               | CounterOffer [((Item,Property), Maybe NRequest)]
               --Counter [(Item,[Property])] NRequest
               | RespLS [NResponse] deriving (Show, Eq)
--Privacy Policy related types -----------------------------------------------------------------------
data Item = OS
          | VC
          | PCR [Int]
          | ID 
          | EntityProp 
          | ProtocolItem deriving (Eq, Show)

data Property = Name
              | Version 
              | IPAddr 
              | IntProperty Int deriving (Eq, Show)
 
type PrivacyPolicy = [PrivacyRule]

type ProtocolId = Int 
data PrivacyRule = Reveal [(Item,[Property])] Condition 
                {- | WillExecute ProtocolId Condition-} deriving (Eq, Show)
data Condition = Equals Item Property Value
               | OneOf Item Property  [Value]
               | NoneOf Item Property [Value]
               | GTV Item Property Value
               | LTV Item Property Value
               | GTETV Item Property Value
               | LTETV Item Property Value
               | Or Condition Condition
               | And Condition Condition
               | FREE deriving (Eq, Show)


data Value = ValString String
           | ValInt Int
           | ValDouble Double
           | ValBool Bool
           | ValByteString ByteString  deriving (Eq, Show)

data ArmoredConfig = ArmoredConfig NRequest Condition PrivacyPolicy deriving ( Show)
--end Privacy Policy related types -------------------------------------------------------------------
-- these are the 'verbs'
data Process = Send Armored Armored Process
	         --  variable mess, channel 
	     | Receive Armored Armored Process  --first is variable
	     	 -- variable mess, variable new chan 
	     | ReceiveAny Armored Armored Process-- the last armored is a variable name for the new channel
	                 --chanName, entity, entity, followingProc
	     | CreateChannel Armored Armored Process
	         --var,    val,    next proc
	     | Let Armored Armored Process
	            --   _  with  _  store var  nextProc  
	     | Encrypt Armored Armored Armored Process
	                   -- _ with  _  store _, success,  failure
	   --  | CaseDecrypt Armored Armored Armored Process Process
	           --var     val1   proc1,  val2    otherwiseproc2
	     | Case Armored [Armored] Process  Process
             | CaseCompare Armored Ordering Armored Process Process
                 -- these  onONEsucceed  onfail
             | Try [Process]
             | Measure Armored Item Property Process
	                --sig    key   succeed  fail
	     | CheckSig Armored Armored Process Process
	              --array  do      finally
	     | ForEach Armored Process Process
	                 --array   val     finally
	     | AppendArray Armored Armored Process--first armored is array, second is val to append
             | ComputeCounterOffer Armored Armored Process
             | CalculateFinalRequest Armored Armored Armored Process
             | HandleFinalChoice Armored Armored Process
             | CheckFinalChoice Armored Armored Process
	     | Result Armored
	     | Stuck String
	     | Stop 
             | StopM String
             | PLACEHOLDER deriving (Show, Eq) 

--these are the 'nouns'	   
--putOnArmor :: a -> Armored
--putOnArmor ..  
data Armored = Var String
	     | ARequest Request
 	     | AResponse Response
             | AEvidenceDescriptor Demo3.EvidenceDescriptor
	     | AEvidencePiece EvidencePiece
	     | ACARequest CARequest
	     | ACAResponse CAResponse 
	     | APair Armored Armored 
--	     | AKey Key  --because show complains commented out.
	     | AEncrypted ByteString
	     | AEntity Entity 
	     | AChannel String
	    -- | ATPM_PCR_SELECTION TPM_PCR_SELECTION
	   --  | ATPM_NONCE TPM_NONCE 
	     | ArmoredPCRSel [Int]
	     | ArmoredCreateNonce
	     | ArmoredCreateDesiredEvidence [Int]
	     | ArmoredRequesetForAttest Armored Armored Armored
	     | ArmoredCreateChannelWith Armored
	     | AMyself --entity stored in state that is the executor
	     | ArmoredEvaluate Armored Armored
	     | ArmoredCreateCACertReq Armored
	     | ArmoredCAChannel
	     | ArmoredEmptyArray
	     | Array String
	     | ArmoredExtractDesiredEvidence
	     | ArmoredArrayItem 
	     | ArmoredMeasurerChan
	     | ArmoredCreateQuote
	     | ArmoredCreateAppResponse
	     | AAttester  --constants for state
             | Target
             | ANRequest
             | ANRequestV NRequest
             | ANResponse NResponse
             | Requester
             | AVal Value
	     | AAppraiser
	     | AMeasurer
	     | APrivacyCA
             | ArmoredAdamList [ArmoredData]
	     | AFailure String 
             | AString String deriving (Show, Eq)
data ChannelEntry = ChannelEntry {
		channelEntryName    :: String,
		channelEntryChannel :: Channel		
		} deriving (Show)

instance Eq ChannelEntry where
  (ChannelEntry name1 chan1) == (ChannelEntry name2 chan2) = chan1 == chan2
data Channel = Channel {
	channelEntity      :: Entity,
	channelInfo :: ChannelInfo
	} deriving (Show)


--di this because the deriving Eq ends thinks when I change the name, it's a new channel, not so!
instance Eq Channel where
  (Channel ent1 info1) == (Channel ent2 info2) = ent1 == ent2 && info1 == info2
                           -- server chan         client channel
data ChannelInfo = VChanInfo {
		    vChanInfoMaybeChan    :: (Maybe LibXenVChan)
		 	}
		 | HttpInfo{
                    httpInfoThreadID         :: Maybe ThreadId,
		    httpInfoMyServingPort    :: HttpClient.Port,
		    httpInfoTheirServingPort :: Maybe HttpClient.Port,
		    httpInfoTheirIp 	     :: HttpClient.Hostname,
		    httpInfoMaybeConnection  :: (Maybe HttpClient.Connection),
		    httpInfoTMVarMsgList     :: TMVar [Armored],
		    httpInfoTMVarUnit        :: TMVar ()
		   }
	   
instance Show ChannelInfo where
  show (VChanInfo vchan) = ("VChanInfo " ++ (show vchan))
  show (HttpInfo mt mp tp tip mconn tmls tmunit) = ("HttpInfo " ++ (show mp) ++ " " ++ (show tp) ++ " " ++ (show tip))
  
data CommRequest = PortRequest {
		   portRequestEntity  :: Entity,		 
		   portRequestPort    :: HttpClient.Port,
		   portRequestNonce   :: Integer
		   }
		 | VChanRequest {
		     vChanRequestEntity :: Entity,
		     vChanReqquestNonce :: Integer		     
		   }


instance Eq ChannelInfo where
 (VChanInfo _) == (HttpInfo _ _ _ _ _ _ _ ) = False
 (VChanInfo m1) == (VChanInfo m1' ) = m1 == m1'
 (HttpInfo mt mp tp tip mconn tmls tmunit) == (HttpInfo mt' mp' tp' tip' mconn' tmls' tmunit') = and [mt == mt', mp == mp', tp == tp', tip == tip']
data Role = Appraiser
    	  | Attester
	  | Measurer
          | PrivacyCA 
          | Undetermined deriving ( Eq, Show)


            
data Entity = Entity {
	        entityName    :: String,
	        entityIp   :: Maybe HttpClient.Hostname,	   
	        entityId   :: Maybe Int,
	        entityRole :: Role,
	        entityNote :: Maybe String
	      }
	     deriving (Eq,  Show)	    
	     
data Key = Rsa ByteString
	 | Tpm ByteString
	 -- | etc	     		


type ArmoredStateTMonad a = StateT ArmoredState IO a 
type VariableBindings = [(Armored,Armored)]
data ArmoredState = ArmoredState {
                            getVars :: VariableBindings,
                            getExecutor :: Entity,
                            getKnownentities  :: [Entity],
                            getPrivacyPolicy :: PrivacyPolicy,
                            getmStateChannel :: Maybe Channel,
                            getInternalStateMVar       :: MVar InternalState,
                            getChannelEntriesTMVar :: TMVar [ChannelEntry]                         
                          }                                                     

data InternalState = AppState {
                       appStateTarget :: Entity,
                       appStateNRequest :: NRequest
                     }
                   | AttState {
                       attStateRequester :: Entity
                     }                       
app = Entity {
	        entityName = "Appraiser",
	        entityIp   = (Just "10.100.0.214"),
	        entityId   = Just 4,
	        entityRole = Appraiser,
	        entityNote = (Just "Just a lonely Appraiser")
	      }
	      
att = Entity {
	        entityName = "Attester",
	        entityIp   = (Just "10.100.0.229"),
	        entityId   = Just 8,
	        entityRole = Attester,
	        entityNote = (Just "Just an attestered here to do your bidding")
	      }

pCA = Entity {
	        entityName = "PrivacyCA",
	        entityIp   = (Just "10.100.0.6"),
	        entityId   = Nothing,
	        entityRole = PrivacyCA,
	        entityNote = (Just "Just a lonely Privacy CA out here in the deep web")
	      }	
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
					                 



--Abstract entity identifier.  The id assignments are LOCAL to the particular protocol being represented.
type EntityId = Int

type Nonce = Int

data TPM_DATA = 
  TdTPM_IDENTITY_CONTENTS  TPM_IDENTITY_CONTENTS 
  | TdTPM_KEY_HANDLE TPM_KEY_HANDLE 
              
              
              deriving (Show)


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
  | AAEvidenceDescriptor EvidenceDescriptor 
  | AEvidence Evidence
  | AAFailure String deriving (Eq,Show)
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
             

type Message = [ArmoredData]
  

--This shoud contain the concrete info that is necessary to communicate with an entity
data EntityInfo = EntityInfo {
  eName :: String,
  eIp :: Int,
  chan :: Channel
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

type AikContents = SignedData TPM_IDENTITY_CONTENTS