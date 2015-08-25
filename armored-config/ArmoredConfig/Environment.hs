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
  if (length ids) < 1 then
    error "Invalid number of arguments"
  else
    return $ ids !! 0

getAttesterDomId :: IO Int
getAttesterDomId = do
  ids <- getVmDomIds
  if (length ids) < 2 then
    error "Invalid number of arguments"
  else
    return $ ids !! 1

getCaDomId :: IO Int
getCaDomId = do
  ids <- getVmDomIds
  if (length ids) < 3 then
    error "Invalid number of arguments"
  else
    return $ ids !! 2

getPort :: IO String
getPort = do
  stringArgs <- getArgs
  if (length stringArgs) < 4 then
    error "Invalid number of arguments"
  else
    return $ stringArgs !! 3

getPid :: IO String
getPid = do
  stringArgs <- getArgs
  if (length stringArgs) < 5 then
    error "Invalid number of arguments"
  else
    return $ stringArgs !! 4


