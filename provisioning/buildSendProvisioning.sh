#!/bin/bash

cabal build Provisioning

#scp dist/build/Provisioning/Provisioning root@10.100.0.221: ; #CA Provisioning
scp dist/build/Provisioning/Provisioning root@10.100.0.222: ; #Att Provisioning
#scp dist/build/Provisioning/Provisioning root@10.100.0.234: ; #App Provisioning
