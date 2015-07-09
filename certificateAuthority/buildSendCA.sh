#!/bin/bash

cabal build CA

scp dist/build/CA/CA root@10.100.0.221: ; #PrivacyCA
