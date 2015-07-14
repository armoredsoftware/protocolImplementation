#!/bin/bash

cabal sandbox delete
cabal sandbox init

cabal configure
cabal build
# cabal install
