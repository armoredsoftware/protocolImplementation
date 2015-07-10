#!/bin/bash
cabal sandbox init
cabal sandbox add-source ../negotiationtools

cabal configure
cabal build
cabal install
