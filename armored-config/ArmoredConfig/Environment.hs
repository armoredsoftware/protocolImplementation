module ArmoredConfig.Environment 
(
getAppraiserDomId,
getAttesterDomId,
getCaDomId,
getPort,
getPid

) where

import System.Environment

getVmDomIds :: IO [Int]
getVmDomIds = do
  stringArgs <- getArgs
  return $ map read stringArgs

getAppraiserDomId :: IO Int
getAppraiserDomId = do
  ids <- getVmDomIds
  return $ ids !! 0

getAttesterDomId :: IO Int
getAttesterDomId = do
  ids <- getVmDomIds
  return $ ids !! 1

getCaDomId :: IO Int
getCaDomId = do
  ids <- getVmDomIds
  return $ ids !! 2

getPort :: IO String
getPort = do
  stringArgs <- getArgs
  return $ stringArgs !! 3

getPid :: IO String
getPid = do
  stringArgs <- getArgs
  return $ stringArgs !! 4


