#!/bin/bash

cabal build
scp dist/build/outerAppraiser/outerAppraiser root@10.100.0.248:
