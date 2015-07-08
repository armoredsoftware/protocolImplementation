#!/bin/bash

cd appraisal
cabal build Appraiser
cd ../attestation
cabal build Attester
cd ../certificateAuthority
cabal build CA
cd ..

scp appraisal/dist/build/Appraiser/Appraiser root@10.100.0.234: ;  #Appraiser
scp attestation/dist/build/Attester/Attester root@10.100.0.222: ; #Attester
scp certificateAuthority/dist/build/CA/CA root@10.100.0.221: ; #PrivacyCA
