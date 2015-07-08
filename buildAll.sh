#!/bin/bash

cd appraisal
cabal build Appraiser
cd ../attestation
cabal build Attester
cd ../certificateAuthority
cabal build CA
cd ..
#cabal build CAMain
#cabal build Measurer



scp appraisal/dist/build/Appraiser/Appraiser root@10.100.0.234: ;  #Appraiser
scp attestation/dist/build/Attester/Attester root@10.100.0.222: ; #Attester
scp certificateAuthority/dist/build/CA/CA root@10.100.0.221: ; #PrivacyCA
#scp dist/build/AttesterProtocol/AttesterProtocol root@10.100.0.203: ; #Attester
#scp Measurer/Measurer root@10.100.0.227: ; #Measurer
#scp dist/build/CAMain/CAMain root@10.100.0.221: ; #PrivacyCA

#scp tpmi/tpmi root@10.100.0.220: ; #Attester(tpmi)
#scp Provisioning/Provisioning root@10.100.0.220: ; #Attester(provision)
