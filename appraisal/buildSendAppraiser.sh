#!/bin/bash

cabal build Appraiser

scp dist/build/Appraiser/Appraiser root@10.100.0.234: ;  #Appraiser
