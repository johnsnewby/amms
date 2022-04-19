#!/bin/bash

CONFIG=settings.yaml

echo "contracts:" > $CONFIG
./make-settings.py tz1NbDzUQCcV2kp3wxdVHVSZEDeq2h97mweW >> $CONFIG #  plenty creator account
./make-settings.py  KT1FWHLMk5tHbwuSsp31S4Jum4dTVmkXpfJw >> $CONFIG #  quipu fa1.2 factory
./make-settings.py  KT1PvEyN1xCFCgorN92QCfYjw3axS6jawCiJ  >> $CONFIG #  quipu fa2 factory
./make-settings.py  KT1JW8AeCbvshGkyrsyu1cWa5Vt7GSpNKrUz >> $CONFIG # vortex FA2 factory
./make-settings.py  KT1UnRsTyHVGADQWDgvENL3e9i6RMnTVfmia >> $CONFIG # vortex FA1.2 factory


