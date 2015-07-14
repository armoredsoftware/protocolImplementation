#!/bin/bash

cabal sandbox delete
cabal sandbox init

pp=`locate bytestringJSON -n1`
cabal install $pp

vch=`locate xeVchan/VChanUtil -n1`
cabal install $vch

tpm=`locate tpmEmulator/tpm-emulator.cabal -n1`
cabal install $tpm



cabal configure
cabal build
# cabal install
