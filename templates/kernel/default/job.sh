#!/bin/sh
#PJM --rsc-list "node=1"
#PJM --rsc-list "rscgrp=small"
#PJM --rsc-list "elapse=3600"
#PJM --stg-transfiles all
#PJM --stgin "kernel.out ./"
#PJM --stgin "initialize*.yml ./"
#PJM --stgin "var_dump_app*.bin ./"
#PJM --stgout "./* ./"
#PJM -s

# Environment setting
. /work/system/Env_base

#--- Program section: 1 --------------------------------------------------
#unset PARALLEL; unset OMP_NUM_THREADS
export PARALLEL=8

# running program
./kernel.out


