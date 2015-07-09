#!/bin/bash

cabal build Attester

scp dist/build/Attester/Attester root@10.100.0.222: ; #Attester
