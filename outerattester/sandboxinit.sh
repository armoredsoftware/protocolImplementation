#!/bin/bash

cabal sandbox delete
cabal sandbox init


cabal sandbox add-source ../shared/ ../shared/bytestringJSON/ ../../tpmEmulator/ ../../xenVchan/VChanUtil/ ../negotiationtools/ ../attestation/ ../appraisal/ ../protoMonad/ ../remoteMonad ../../remote-json/ ../armored-config/ ~/channelsRepo/channels/Communication/ChannelInstances/VChan/ ~/channelsRepo/channels/Communication/ChannelInstances/Http/ ~/channelsRepo/channels/Communication/ChannelInstances/HttpTuna/ ~/channelsRepo/channels/Communication/Monad/ ~/channelsRepo/channels/ArmoredCom/ ~/channelsRepo/channels/AbstractChannel/ 

cabal configure
cabal install --dependencies-only

cabal build
