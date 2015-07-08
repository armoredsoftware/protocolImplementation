{-# LANGUAGE ScopedTypeVariables #-}

module ProtoMain where

import ProtoTypesA
import ProtoMonad
import ProtoActions

import Data.ByteString.Lazy


nsEntityA :: Proto Nonce
nsEntityA = do
  let nonceA = 34 --nonceA <- generateNonce
  myInfo <- getEntityInfo 0  --0 is MY id by default
  cipherX <- encrypt 1 [ANonce nonceA, AEntityInfo myInfo]
  send 1 [ACipherText cipherX]
  [ACipherText cipherText] <- receive 1
  [ANonce nA, ANonce nB, AEntityInfo eInfo] <- decrypt cipherText
  checkNonce nonceA nA --Throws error if not equal
  cipherOut <- encrypt 1 [ANonce nB]
  send 1 [ACipherText cipherOut]
  return nB

nsEntityB :: Proto Nonce
nsEntityB = do
  [ACipherText cipherMessage1] <- receive 1
  [ANonce nA, AEntityInfo eInfo] <- decrypt cipherMessage1
  --nonceB <- generateNonce
  let nonceB = 56
  myInfo <- getEntityInfo 0  --0 is MY id by default
  cipherOut <- encrypt 1 [ANonce nA, ANonce nonceB, AEntityInfo myInfo]
  send 1 [ACipherText cipherOut]
  [ACipherText cipherMessage2] <- receive 1
  [ANonce nB] <- decrypt cipherMessage2
  checkNonce nonceB nB --Throws error if not equal
  return nA
  
  
  





