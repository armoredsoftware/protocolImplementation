#!/bin/bash

#cd provisioning
#cabal build ProvisioningMain
#cd ../outerappraiser
cd outerappraiser
cabal build outerAppraiser
cd ../outerattester
cabal build outerAttester
cd ../certificateAuthority
cabal build CA
cd ..

#scp provisioning/dist/build/Provisioning/Provisioning root@10.100.0.221: ; #Provisioning
scp outerappraiser/dist/build/outerAppraiser/outerAppraiser root@10.100.0.234: ; # Appraiser
scp outerattester/dist/build/outerAttester/outerAttester root@10.100.0.222: ; # Appraiser
scp certificateAuthority/dist/build/CA/CA root@10.100.0.221: ; #
