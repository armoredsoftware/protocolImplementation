#!/bin/bash

cabal sandbox delete
cabal sandbox init

cabal install ../../remote-json

cabal sandbox add-source ../shared/ ../shared/bytestringJSON/ ../../tpmEmulator/ ../../xenVchan/VChanUtil/ ../negotiationtools/ ../attestation/ ../appraisal/ ../protoMonad/ ../remoteMonad

cabal configure
cabal install #--dependencies-only

cabal build
