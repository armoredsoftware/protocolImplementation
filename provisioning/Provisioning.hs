module Provisioning where

import TPM
import TPMUtil
--import Demo3Shared

import System.IO
import Data.Word
import Data.Binary

goldenFileName :: String
goldenFileName= "goldenPcrComposite.txt"


readComp :: IO TPM_PCR_COMPOSITE
readComp = do
  handle <- openFile goldenFileName ReadMode
  compString <- hGetLine handle
  let comp :: TPM_PCR_COMPOSITE
      comp = read compString
  putStrLn $ show comp
  hClose handle
  return comp


getCurrentComp :: IO TPM_PCR_COMPOSITE
getCurrentComp = do
  let list = [0..23] :: [Word8]
      pcrSelect = tpm_pcr_selection 24 list
  compGolden <- tpm_pcr_composite tpm pcrSelect
  return compGolden


--Helper for export
doExport :: String -> TPM_PCR_COMPOSITE ->  IO ()
doExport fileName comp =
                   do handle <- openFile fileName WriteMode
                      hPutStrLn handle $ show comp
                      hClose handle

--Helper for export
genDoExport :: (Binary a) => String -> a ->  IO ()
genDoExport fileName val =
                   do encodeFile fileName val
