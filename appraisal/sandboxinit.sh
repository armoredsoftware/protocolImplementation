#!/bin/bash
cabal sandbox delete
cabal sandbox init
cabal sandbox add-source ../shared/bytestringJSON/
cabal sandbox add-source ../shared/
cabal sandbox add-source ../../tpmEmulator/
cabal sandbox add-source ../../xenVchan/VChanUtil/
cabal sandbox add-source ../negotiationtools/
cabal sandbox add-source ../protoMonad/


cabal configure
cabal build
cabal install
