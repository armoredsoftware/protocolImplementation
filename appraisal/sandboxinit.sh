#!/bin/bash

cabal sandbox delete
cabal sandbox init

vch=`locate xeVchan/VChanUtil -n1`
cabal install $vch

tpm=`locate tpmEmulator/tpm-emulator.cabal -n1`
cabal install $tpm

shar=`locate protocolImplementation/shared/sharedarmor.cabal | pwd`
cabal install shar 


cabal configure
cabal build
# cabal install
