{-# LANGUAGE RecordWildCards  #-}
module Protocol where

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import Demo3Shared hiding (Result, AttState)
import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict)
import TPM
import qualified Network.Http.Client as HttpClient
import qualified CommTools as CommTools
--import qualified Demo3Shared as Demo3
import VChanUtil
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import qualified Web.Scotty as Scotty
import ProtoTypes
import CommunicationNegotiator
import CommTools hiding (Result)
import Control.Concurrent
import Data.Tuple
import ExampleArmoredConfig
import Data.Maybe
import System.IO                 
import System.Timeout
import qualified AttesterMain as AttSubProto (attmain)
import qualified AppraiserMain as AppSubProto (appmain)  
	      
runExecute :: Process -> Entity ->IO (Process, ArmoredState)
runExecute proto executor = do
   let emptyvars = []
       privacyPolicy = []
       statechan = Nothing
       knownEs = []
   e <- newTMVarIO []
   t <- newEmptyMVar

   let s0 = ArmoredState emptyvars executor knownEs privacyPolicy statechan t e
   forkIO $ do 
   	     runStateT negotiator s0
   	     return ()
   runStateT (execute proto) s0

runExecute' :: Process -> ArmoredState ->IO (Process, ArmoredState)
runExecute' proto s0 = do
   runStateT (execute proto) s0

execute :: Process -> ArmoredStateTMonad Process
       --variable, entity, entity, commMethod, followingProc
--	     | CreateChannel Armored Armored Armored Armored Process
execute (Let var val proc) = do 
   --var' <- subIfVar var
   val' <- subIfVar val
   addVariable var val'
   let str = ("performing let: variable: " ++ (show var) ++ (" val: " ++ (show val')))
   liftIO $ putStrLn str 
   logf' str
   execute proc
   
execute (CreateChannel achan ent1 proc) = do
   let str = "EXECUTING CREATECHANNEL COMMAND\n" 
   liftIO $ putStrLn str
   logf' str
   achan' <- subIfVar achan
   ent1' <- subIfVar ent1
   case achan' of
     (AChannel chanName) -> do                                   
       case ent1' of
        (AEntity ent1'') -> do
          mVChannel <- maybeCreateVChannelWith ent1'' chanName
          case mVChannel of 
            Nothing -> do
              let str = "vchan establishment unsuccessful. Trying Http.."
              liftIO $ putStrLn $ str
              logf' str
              mHttpChannel <- tryCreateHttpChannel ent1'' chanName 
              case mHttpChannel of
                Nothing -> do
                  let  str2 = "super error! no channel created. vchan and http failed."                 
                  liftIO $ putStrLn str2
                  logf' str2
                  killChannels
                  return $ Stuck str2
                Just hChan -> do
                  let str3 = "successfully created httpChannel"
                  liftIO $ putStrLn $ str3
                  logf' str3
                  --http chan added to state in tryCreateHttpChannel                  
                  s <- get 
                  case getmStateChannel s of 
                    Just _ -> return () 
                    Nothing -> do 
                      ArmoredState {..} <- get 
                      put $ ArmoredState { getmStateChannel = Just hChan, ..}
                      
                  execute proc 
            Just vchan -> do --could be because channel existed, or because I just made it. 
              s <- get 
              case getmStateChannel s of 
                Just _ -> return () 
                Nothing -> do 
                  ArmoredState {..} <- get 
                  put $ ArmoredState { getmStateChannel = Just vchan, ..}
              execute proc  
        (_)              -> do
          let str = "not an entity in create channel!!! stopping now."
          liftIO $ putStrLn str
          logf' str
          killChannels
          return $ Stuck "didn't have an entity in the CreateChannel command. sorry, I gave up."
     a@_ -> do 
       let err = "unexpected type in first argument to CreateChannel: " ++ (show a) ++ " stuck!"
       liftIO $ putStrLn $ err
       logf' err
       killChannels
       return $ Stuck err
   
       
           --addChannel achan' (Channel entity1 entity2 (HttpInfo ip2 port2 Nothing )) -- conn))                        
execute (Send mess chan proc) = do
  let str = "sending on channel"
  liftIO $ putStrLn str 
  --logf' str 
  chan' <- subIfVar chan
  mess' <- subIfVar mess 
  case chan' of
   (AChannel str) -> do
     s <- get
     let chanEntryTMVar = getChannelEntriesTMVar s 
     chanEntryLS <- liftIO $ atomically $ takeTMVar chanEntryTMVar 
     case lookupViaName str chanEntryLS of
       Nothing        -> do 
         let str = "Error for now: Send without creating first. easily change to create channel call here."
         liftIO $ putStrLn str 
         logf' str
         liftIO $ atomically $ putTMVar chanEntryTMVar chanEntryLS
       Just chanEntry -> do 
         let chan = channelEntryChannel chanEntry 
         let str =  "sending:  " ++ (show mess') -- ++ " which converted to shared is: " ++ (show (armoredToShared mess'))
         liftIO $ putStrLn str
         logf' str
         liftIO $ atomically $ putTMVar chanEntryTMVar chanEntryLS
         liftIO $ sendG chan mess' 
     execute proc 
   _		      -> do
                          killChannels
                          return (Stuck "attempt to send on non-channel")
  
execute (Receive var chan proc) = do
  let str =  "receiving.."
  liftIO $ putStrLn str
  logf' str
  chan' <- subIfVar chan
  case chan' of
   (AChannel str) -> do
     s <- get
     let chanEntryTMVar = getChannelEntriesTMVar s 
     chanEntryLS <- liftIO $ atomically $ takeTMVar chanEntryTMVar 
     --liftIO $ putStrLn $ "looking for channel named: " ++ str ++ " in " ++ (show chanEntryLS)
     case lookupViaName str chanEntryLS of
       Nothing        -> do 
         let strer = "Error!! channel with name: " ++ str ++ " not found!!"
         liftIO $ putStrLn strer 
         logf' strer
         liftIO $ atomically $ putTMVar chanEntryTMVar chanEntryLS
         killChannels
         return (Stuck str)
       Just chanEntry -> do 
         let chan = channelEntryChannel chanEntry 
         armoredGift <- liftIO $ receiveG chan
         liftIO $ atomically $ putTMVar chanEntryTMVar chanEntryLS
         case armoredGift of
           AFailure str -> do 
             let str =  ("Crap. failed in receive: " ++ str )
             liftIO $ putStrLn str 
             logf' str 
             killChannels
             return $ Stuck str 
           x@_          -> do 
             addVariable var x 
             execute proc 
   _		      -> do
                           killChannels
                           return (Stuck "attempt to receive on non-channel")
execute (Case v1 ls procSucceed procFail) = do 
  v1' <- subIfVar v1
  ls' <- sequence $ map subIfVar ls 
  case v1' `elem` ls' of 
    True -> execute procSucceed
    False -> execute procFail
execute (Result res) = do
			res' <- subIfVar res
			let str = ("Result: " ++ (show res'))
                        liftIO $ putStrLn str 
                        logf' str
                        killChannels			
			return (Result res')
execute (Stop) = do
                   killChannels
                   return Stop
execute (Try ls) = do 
                    case ls of 
                      [] -> execute (Stuck "Nothing left in the Try!!")
                      (x:xs) -> do 
                        final <- execute x
                        case final of 
                          (Stuck str) -> do 
                            let str = "reached a stuck in the Try statement: " ++ str ++ "\n Trying next.."
                            liftIO $ putStrLn str 
                            logf' str
                            execute (Try xs)
execute (ComputeCounterOffer storeVar armReq proc) = do
  armReq' <- subIfVar armReq
  case armReq' of 
    (ANRequestV nreq) -> do 
      ArmoredState {..} <- get 
      let counteroffer = createCounterOffer nreq getPrivacyPolicy
      let str = "computed counter offer: " ++ (show counteroffer)
      liftIO $ putStrLn str 
      logf' str 
      addVariable storeVar (ANResponse counteroffer)
      execute proc
    a@_               -> do 
      let str = "Error: tried to compute counter offer with: " ++ (show armReq') ++ "\n but expected an NRequest" 
      liftIO $ putStrLn str 
      logf' str
      execute $ Stuck str 
execute (CalculateFinalRequest storeVar myOriginalRequest counterOffer proc) = do 
--TODO measurement deadlock resolution here
--TODO make sure counterOffer is subset of offer
  counterOffer' <- subIfVar counterOffer
  myOriginalRequest' <- subIfVar myOriginalRequest
  case counterOffer' of
    (ANResponse (CounterOffer ls)) -> do 
      ArmoredState {..} <- get 
      let finalItemPropPairs = foldr (\((i,p),mNreq) acc -> case mNreq of 
                                       Nothing -> (i,p):acc -- then we get for free!!
                                       Just r  -> case r `liesin` getPrivacyPolicy of 
                                                   [] -> acc --my privacy policy says I won't give you that. 
                                                   a@_ -> (i,p):acc) [] ls
          finalReq = pairsToN1Req finalItemPropPairs
      let str = "calculated final Request: " ++ (show finalReq)
      liftIO $ putStrLn str 
      liftIO $ putStrLn $ "By the way, this is my privacy policy: " ++ (show getPrivacyPolicy)
      logf' str 
      addVariable storeVar (ANRequestV finalReq)
      execute proc 
    a@_               -> do 
      let str = "Error: tried to compute final request expected counteroffer but found: " ++ (show counterOffer') 
      liftIO $ putStrLn str 
      logf' str
      execute $ Stuck str 
execute (CheckFinalChoice storeVar anreq proc) = do 
  anreq' <- subIfVar anreq 
  case anreq' of 
    (ANRequestV nreq) -> do 
      ArmoredState {..} <- get 
      let val = (case nreq `completelyAbidesBy` getPrivacyPolicy of 
                      False -> (ANResponse No)
                      True  -> anreq')
      addVariable storeVar val
      let str = "checked final choice: " ++ (show val)
      liftIO $ putStrLn str 
      logf' str
      execute proc 
 
execute (HandleFinalChoice storeVar finalNReq  proc) = do 
  finalNReq' <- subIfVar finalNReq
  case finalNReq' of 
    (ANRequestV nreq) -> do 
       ArmoredState {..} <- get 
       case getmStateChannel of 
         Nothing -> execute $ Stuck $ "Error: NO channel in the state!! Did you call CreateChannel?"
         Just chan -> do 
           case nreq of 
             ReqLS [] -> do 
               let str = "Could not come to an agreement. No attestation to take place."
               liftIO $ putStrLn str 
               --logf' str 
               addVariable storeVar (AString str)
               execute proc 
             (RequestItem ProtocolItem (IntProperty i)) -> do
               let (who,f) = case (entityRole getExecutor) of 
                              Appraiser -> ("Appraiser",AppSubProto.appmain )
                              Attester  -> ("Attester",AttSubProto.attmain )
               let str = "Negotiation complete. About to perform " ++ who ++ " sub protocol for: " ++ (show nreq)
               liftIO $ putStrLn str 
               logf' str
               subResult <- liftIO $ f chan i
               addVariable storeVar (AString subResult)
               execute proc 
                        
    a@_                 -> do 
          let str = "Error: in HandleFinalChoice. Expected to be NResponse, but instead found: " ++ (show a)
          liftIO $ putStrLn str 
          logf' str 
          execute $ Stuck str                      
--execute (PrivacyPolicyInsertion proc) = do 
  --      ArmoredState {..} <- get 
execute (StopM str) = do 
  let str' = "StopM: " ++ str
  liftIO $ putStrLn str'
  logf' str' 
  killChannels
  return (StopM str)

execute (Stuck str) = do 
  let str' = "Stuck: " ++ str 
  liftIO $ putStrLn str' 
  logf' str'
  killChannels
  return (Stuck str)
              
addVariable :: Armored -> Armored -> ArmoredStateTMonad ()
addVariable var val = do
		    ArmoredState {..} <- get
		    let variables = getVars
		    let variables' = (var,val) : variables
                    put (ArmoredState {getVars=variables', ..} )		   
		    return ()
       --channel name

		    
typeCheckProcess :: Process -> Either String Bool
typeCheckProcess proc = Left "fail" --TODO      	      

lookupViaName :: String  -> [ChannelEntry] -> Maybe ChannelEntry
lookupViaName str [] = Nothing
lookupViaName str (x:xs) = case str == (channelEntryName x) of
                             True  -> Just x 
                             False -> lookupViaName str xs 

lookupViaChan :: Channel -> [ChannelEntry] -> Maybe ChannelEntry
lookupViaChan chan [] = Nothing
lookupViaChan chan (x:xs) = case chan == (channelEntryChannel x) of
                             True  -> Just x 
                             False -> lookupViaChan chan xs 

subIfVar ::  Armored -> ArmoredStateTMonad  Armored
subIfVar  armItem = do
   s <- get
   case armItem of 
     ANRequest -> do 
       s <- get 
       let mvarinternal = getInternalStateMVar s 
       maybeInternalState <- liftIO $ tryTakeMVar mvarinternal
       case maybeInternalState of 
        Nothing -> do 
          let str = "Error: Encountered ANRequest, but No internal State in MVar!!"
          liftIO $ putStrLn str 
          logf' str
          return $ AFailure str 
        Just is -> do 
          liftIO $ putMVar mvarinternal is 
          return (ANRequestV (appStateNRequest is))
     Target -> do 
                       s <- get
                       let internalMVar = (getInternalStateMVar s)
                       b <- liftIO $ isEmptyMVar internalMVar
                       if b
                         then do
                           let q = "Error: No Target Specified in state. exiting."
                           liftIO $ putStrLn q
                           logf' q
                           return $ AFailure q                         
                         else do 
                          iState <- liftIO $ readMVar internalMVar                       
                          return $ AEntity $ appStateTarget iState   
     Requester -> do 
       s <- get 
       let internalMVar = getInternalStateMVar s
       b <- liftIO $ isEmptyMVar internalMVar
       if b
        then do
         let q = "Error: No Requester Specified in state. exiting."
         liftIO $ putStrLn q
         logf' q
         return $ AFailure q                         
           else do 
            iState <- liftIO $ readMVar internalMVar     
            case iState of 
             (AppState {..}) -> do 
               let q = "Error: Wrong state! Appraiser state found, but code called for 'Requester' which is only available in attester state"
               liftIO $ putStrLn q 
               logf' q
               return $ AFailure q
             (AttState x) -> do 
               return $ AEntity x
     _ -> return $ subIfVar'  armItem (getVars s) 

subIfVar' ::  Armored -> VariableBindings ->  Armored 
subIfVar'  armItem gamma = case myLookup  armItem gamma of
   Nothing ->  armItem
   Just val -> val
                    
myLookup ::  Armored -> VariableBindings -> Maybe  Armored
myLookup  armItem []     = Nothing
myLookup  arm@(Var str) (((Var str2),val):xs) = if str == str2 
				    then Just val
				    else myLookup arm xs
myLookup any@(_) (x:xs)  = myLookup any xs        				                                                   

pairsToN1Req :: [(Item,Property)] -> NRequest
pairsToN1Req ls = case length ls of 
                   1 -> let (i,p) = head ls in RequestItem i p 
                   _ -> ReqLS (map (\(i,p) -> RequestItem i p) ls)
	      	     
createCounterOffer :: NRequest -> PrivacyPolicy -> NResponse
createCounterOffer theirReq privy = let ls = theirReq `liesin` privy in
  CounterOffer (map (\(p,rule) -> (p, privacyRuleToNResquest rule)) ls)  

privacyRuleToNResquest :: PrivacyRule -> Maybe NRequest
privacyRuleToNResquest (Reveal ls cond) = condToNRequest cond 

condToNRequest :: Condition -> Maybe NRequest
condToNRequest (Equals i p v) = Just $ RequestItem i p
condToNRequest (OneOf i p ls) = Just $ RequestItem i p 
condToNRequest (NoneOf i p ls) = Just $ RequestItem i p
condToNRequest (GTV i p v)     = Just $ RequestItem i p
condToNRequest (LTV i p v)     = Just $ RequestItem i p
condToNRequest (GTETV i p v)   = Just $ RequestItem i p
condToNRequest (LTETV i p v)   = Just $ RequestItem i p
condToNRequest (Or c1 c2)      = case (condToNRequest c1,condToNRequest c2) of 
                                         (Nothing, Nothing) -> Nothing 
                                         (Just x, Nothing)  -> Just x 
                                         (Nothing, Just x)  -> Just x 
                                         (Just x, Just y)   -> Just (ReqLS [x,y])
                                        
condToNRequest (And c1 c2)      = case (condToNRequest c1,condToNRequest c2) of 
                                         (Nothing, Nothing) -> Nothing 
                                         (Just x, Nothing)  -> Just x 
                                         (Nothing, Just x)  -> Just x 
                                         (Just x, Just y)   -> Just (ReqLS [x,y])
condToNRequest FREE             = Nothing 
 
reifyPolicy :: NRequest -> PrivacyPolicy -> Process
reifyPolicy req policy = case req `liesin` policy of 
                           []        -> (Send (ANResponse No) Requester 
                                        (Stuck "Privacy Policy did not have reference to requested items."))
                           --the following needs severe optimization, that or the state needs to keep track of what
                           --still needs to be known so that repeat requests are avoided.
                           (relevantPol@_) ->  reifyPolicy' relevantPol

-- regard note about optimizing. 
reifyPolicy' :: [((Item,Property),PrivacyRule)] -> Process
reifyPolicy' [] = Stop
reifyPolicy' (((i,p),(Reveal things cond)):xs) = let varname =("Measurementof:" ++ (show i) ++ (show p)) in 
                                               (reifyCondition cond) `procAppend` (Measure (Var varname) i p 
                                                                                  (Send (Var varname) Requester 
                                                                                  Stop))

reifyCondition :: Condition -> Process
reifyCondition (Equals item prop value) = let req = ANRequestV $ RequestItem item prop 
                                              varname = (show item) ++ (show prop) in 
                                           (Send req Requester 
                                           (Receive (Var varname) Requester 
                                           (Case (Var varname) [(AVal value)] PLACEHOLDER (Stuck ("Values did not match. Expected: " ++ (show item) ++ ", " ++ (show prop) ++ " to be: " ++ (show value))))))
reifyCondition (OneOf item prop valls) = let ls' = map AVal valls 
                                             req = ANRequestV $ RequestItem item prop 
                                             varname = (show item) ++ (show prop) in 
                                          (Send req Requester 
                                          (Receive (Var varname) Requester 
                                          (Case (Var varname) ls' PLACEHOLDER (Stuck ("Value did not match. Expected: " ++ (show item) ++ ", " ++ (show prop) ++ " to be one of: " ++ (show ls'))))))
reifyCondition (NoneOf item prop valls) = let ls' = map AVal valls 
                                              req = ANRequestV $ RequestItem item prop 
                                              varname = (show item) ++ (show prop) in 
                                          (Send req Requester 
                                          (Receive (Var varname) Requester 
                                          (Case (Var varname) ls' (Stuck ("Value found to be forbidden set: " ++ (show item) ++ ", " ++ (show prop) ++ " was found in: " ++ (show ls'))) PLACEHOLDER)))
reifyCondition (GTV item prop val) = let req = ANRequestV $ RequestItem item prop 
                                         varname = (show item) ++ (show prop) in 
                                          (Send req Requester 
                                          (Receive (Var varname) Requester 
                                          (CaseCompare (Var varname) GT (AVal val) PLACEHOLDER (Stuck ("Value in CaseCompare did not satisfy condition: " ++ (show item) ++ ", " ++ (show prop) ++ " did not satisfy GREATER THAN: " ++ (show val))))))
reifyCondition (GTV item prop val) = let req = ANRequestV $ RequestItem item prop 
                                         varname = (show item) ++ (show prop) in 
                                          (Send req Requester 
                                          (Receive (Var varname) Requester 
                                          (CaseCompare (Var varname) LT (AVal val) PLACEHOLDER (Stuck ("Value in CaseCompare did not satisfy condition: " ++ (show item) ++ ", " ++ (show prop) ++ " did not satisfy GREATER THAN: " ++ (show val))))))
reifyCondition (GTETV item prop val) = let req = ANRequestV $ RequestItem item prop
                                           varname = (show item) ++ (show prop) in 
                                          (Send req Requester 
                                          (Receive (Var varname) Requester 
                                          (CaseCompare (Var varname) GT (AVal val) PLACEHOLDER 
                                                       (Case (Var varname) [(AVal val)] PLACEHOLDER 
                                                             (Stuck ("Value in CaseCompare did not satisfy condition: " ++ (show item) ++ ", " ++ (show prop) ++ " did not satisfy GREATER THAN: " ++ (show val)))))))
reifyCondition (LTETV item prop val) = let req = ANRequestV $ RequestItem item prop
                                           varname = (show item) ++ (show prop) in 
                                          (Send req Requester 
                                          (Receive (Var varname) Requester 
                                          (CaseCompare (Var varname) LT (AVal val) PLACEHOLDER 
                                                       (Case (Var varname) [(AVal val)] PLACEHOLDER 
                                                             (Stuck ("Value in CaseCompare did not satisfy condition: " ++ (show item) ++ ", " ++ (show prop) ++ " did not satisfy GREATER THAN: " ++ (show val)))))))
reifyCondition (Or c1 c2) = let proc1 = reifyCondition c1
                                proc2 = reifyCondition c2 in 
                              Try [proc1,proc2]
reifyCondition (And c1 c2) = let proc1 = reifyCondition c1
                                 proc2 = reifyCondition c2 in 
                               proc1 `procAppend` proc2
reifyCondition FREE = PLACEHOLDER

procAppend :: Process -> Process -> Process 
procAppend PLACEHOLDER p2 = p2
procAppend (Send a1 a2 p) p2 = Send a1 a2 (procAppend p p2)
procAppend (Receive a1 a2 p) p2 = Receive a1 a2 (procAppend p p2)
procAppend (CreateChannel a1 a2 p) p2 = CreateChannel a1 a2 (procAppend p p2)
procAppend (Let a1 a2 p) p2 = Let a1 a2 (procAppend p p2)
procAppend (Case a1 a2 p1 p2) p = Case a1 a2 (procAppend p1 p) (procAppend p2 p)
procAppend (CaseCompare a1 o a2 p1 p2) p = CaseCompare a1 o a2 (procAppend p1 p) (procAppend p2 p)
procAppend (Try ls) p2 = Try $ map ((flip procAppend) p2) ls
procAppend (Measure a i prop p) p2 = Measure a i prop (procAppend p p2)  
procAppend a@_ p2 = a

--reifyCondition (Or c1 c2) = 
{- Note that calling liesin does 2 things. 
   1. removes irrelevent rules
   2. If there is no rule for the requested item, you get [].
      Therefore, the proposed protocol will not include anything about it.
-}
liesin :: NRequest -> PrivacyPolicy ->  [((Item,Property), PrivacyRule)] --PrivacyPolicy 
liesin (ReqLS []) _ = []
liesin (ReqLS ls) pol =  join $ map ((flip liesin) pol) ls
liesin (RequestItem item prop) pol = case liesinHelper item prop pol of 
                                      Nothing -> []
                                      Just rule -> [rule] 
      
liesinHelper :: Item -> Property -> PrivacyPolicy -> Maybe ((Item, Property),PrivacyRule)
liesinHelper _ _ [] = Nothing 
liesinHelper item prop (r@(Reveal ls _):xs) = case lookup item ls of
                                                Nothing -> liesinHelper item prop xs
                                                Just props -> if prop `elem` props 
                                                               then Just ((item,prop),r)
                                                               else liesinHelper item prop xs
completelyAbidesBy :: NRequest -> PrivacyPolicy -> Bool 
completelyAbidesBy (ReqLS []) pol = True
completelyAbidesBy (ReqLS ls) pol = and $ map ( (flip completelyAbidesBy) pol) ls 
completelyAbidesBy (RequestItem item prop) pol = case liesinHelper item prop pol of 
                                                   Nothing -> False
                                                   Just _  -> True 
convertNReq :: NRequest -> NRequest
convertNReq (ProtoNum i) = RequestItem ProtocolItem (IntProperty i)
convertNReq (ReqLS ls)   = ReqLS (map convertNReq ls)
convertNReq (TierRequest ls) = TierRequest (map convertNReq ls)
convertNReq x@_              = x 

clearLogf :: IO ()
clearLogf = do 
         h <- openFile "log.1" WriteMode
         hPutStr h ""
         hClose h

