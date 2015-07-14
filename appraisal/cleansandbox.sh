#!/bin/bash

cabal sandbox delete
cabal sandbox init

pp=`locate bytestringJSON/bytestringJSON.cabal -n1 | pwd`
cd pp
./sandboxinit.sh
cd -

#not needed here, but other depends depends on it being rebuilt (sandboxinit)!
#cabal install $pp

# bytestring cleaning must be done FIRST 
tpm=`locate tpmEmulator/tpm-emulator.cabal -n1 | pwd`
cd $tpm
./sandboxinit.sh
cd -

cabal install $tpm


# vchan has no sandbox.
vch=`locate xeVchan/VChanUtil -n1`
cabal install $vch

cabal configure
cabal build
# cabal install
