#!/bin/bash

cabal sandbox delete
cabal sandbox init


cabal sandbox add-source ../shared/ ../shared/bytestringJSON/ ../../tpmEmulator/ ../../xenVchan/VChanUtil/ ../negotiationtools/ ../attestation/ ../appraisal/ ../protoMonad/ ../remoteMonad ../../remote-json ../armored-config/

cabal configure
cabal install --dependencies-only

cabal build
