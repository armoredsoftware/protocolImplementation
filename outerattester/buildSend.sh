#!/bin/bash

cabal build
scp dist/build/outerAttester/outerAttester root@10.100.0.244:
