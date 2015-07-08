#!/bin/bash
cabal sandbox init
cabal sandbox add-source ../../tpmEmulator/
cabal sandbox add-source ../../xenVchan/VChanUtil/

cabal configure
cabal build
cabal install
