{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, RecordWildCards  #-}

module Demo3Shared (module Demo3SharedNOVCHAN,
		    module Demo3Vchan) where

import Demo3SharedNOVCHAN
import Demo3Vchan


import qualified Control.Monad.Trans.State as T
import Control.Monad.Trans
