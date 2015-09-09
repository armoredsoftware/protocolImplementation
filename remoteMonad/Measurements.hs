{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Measurements
( topMeasurement
, measurementToList
, set_target_app
, measure_variable
, hook_app_variable
, load_store
, Measurement (..)
) 
where

import qualified Control.Monad.Remote.JSON as Jsonrpc
import Control.Monad.Remote.JSON
import Control.Monad.Remote.JSON.Debug (traceSessionAPI)
import Control.Monad.Remote.JSON.Types (SessionAPI(..))

import Control.Applicative
import Control.Monad (mzero)
import Data.List
import Data.Aeson (Result(..),Value(..),FromJSON, fromJSON, parseJSON, (.:),(.:?))
import qualified Data.Text as Text


data Measurement = Measurement { next:: Maybe (Measurement),
                                res::Text.Text,
                                resultType::Text.Text}
    deriving Show

instance FromJSON Measurement where
    parseJSON (Object v) = Measurement <$>
                             v .:? "next" <*>
                             v .: "data"<*>
                             v .: "type"
    parseJSON _          = mzero


topMeasurement :: Measurement -> Text.Text
topMeasurement m = head $ measurementToList m

measurementToList :: Measurement -> [Text.Text]
measurementToList m = case next m of
                         Nothing -> [res m]
                         Just m2 -> (res m): (measurementToList m2)

set_target_app :: String -> RPC ()
set_target_app pidString = notification "eval" (List [String (Text.pack ("(set_target "++ pidString ++")"))])

measure_variable :: String -> RPC Measurement
measure_variable varString = do
  v <- method "eval" (List [String $ Text.pack $ "(measure (var \"" ++ varString ++ "\"))"])
  case fromJSON v of
             Success (m :: Measurement) -> return m
             Error s ->  error s


hook_app_variable :: String -> Int -> Bool -> Int -> String -> RPC ()
hook_app_variable appName sourceLine repeat storeNum varName =
  let repeatStr = case repeat of
                      True -> "1"
                      False -> "0" in --return ()
  do
  method "eval" (List [String (Text.pack $ "(hook (reach \"" ++ appName ++ "\" " ++ (show sourceLine) ++ " " ++ repeatStr ++ ") '(store " ++ (show storeNum) ++ " (measure (var \"" ++ varName ++ "\"))))")])
  return () -- TODO: Should we return result of hook here eventually??

load_store :: Int -> RPC Measurement
load_store storeNum = do
  v <- method "eval" (List [String $ Text.pack $ "(load " ++ (show storeNum) ++ ")"])
  case fromJSON v of
             Success (m :: Measurement) -> return m
             Error s ->  error s
