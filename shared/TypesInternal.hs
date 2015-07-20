{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module TypesInternal where

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
--import Demo3Shared hiding (Result, sig, dat, Signature, Evidence, EvidenceDescriptor)
import Control.Monad
import Control.Monad.State.Strict hiding (get, put)
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
import TPM
import qualified Network.Http.Client as HttpClient
--import qualified Demo3Shared as Demo3
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
data EvidenceDescriptor = D0 | D1 | D2
                        | DONE deriving(Eq, Ord, Show)

type Evidence = [EvidencePiece]

data EvidencePiece = M0 M0Rep 
                   | M1 M1Rep
                   | M2 M2Rep 
                   | OK deriving (Eq, Ord, Show)

data Armored = Var String
	  --   | ARequest Request
 	  --   | AResponse Response
             | AEvidenceDescriptor EvidenceDescriptor
	     | AEvidencePiece EvidencePiece
	    -- | ACARequest CARequest
	     -- | ACAResponse CAResponse
	     | APair Armored Armored
--	     | AKey Key  --because show complains commented out.
	     | AEncrypted ByteString
	     | AEntity Entity
	     | AChannel String
	     ---- | ATPM_PCR_SELECTION TPM_PCR_SELECTION
	     | ATPM_NONCE TPM_NONCE
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


type AikContents = SignedData TPM_IDENTITY_CONTENTS

type PrivateKey = Codec.Crypto.RSA.PrivateKey --ByteString;
type PublicKey = Codec.Crypto.RSA.PublicKey --ByteString;

type SymmKey = TPM_SYMMETRIC_KEY
--Encrypted text
type CipherText = ByteString;



type Signature = ByteString;
data SignedData a = SignedData {
  dat :: a,
  sig :: Signature
} deriving (Eq, Show)

type Message = [ArmoredData]

--This shoud contain the concrete info that is necessary to communicate with an entity
data EntityInfo = EntityInfo {
  eName :: String,
  eIp :: Int,
  chan :: Channel
} deriving (Eq, Show)

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

--Abstract entity identifier.  The id assignments are LOCAL to the particular protocol being represented.
type EntityId = Int

type Nonce = Int

data TPM_DATA =
  TdTPM_IDENTITY_CONTENTS  TPM_IDENTITY_CONTENTS
  | TdTPM_KEY_HANDLE TPM_KEY_HANDLE


              deriving (Show)



type M0Rep = ByteString
type M1Rep = ByteString
type M2Rep = ByteString
