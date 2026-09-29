#!/bin/bash
# Kink with k FIXED and s ESTIMATED, plus a score row for the scale (Hans, 2026-09-29). Build KINK_S (12 rows:
# moment set A with eps*psi, tax row, share row [10], row [11] = eps * softsign(dh/dkappa)); binary grid_estimator_kinks.
# Grid: k in {0.3, 1} x kappa in {0.3, 0.4, 0.5, 0.6, 0.7} = 10 single-point fits; s starts at 0.2, bounds [0.02, 0.6].
# Seeds (delta, gamma1..11 + gamma12 = 0): k=0.3 from 1560 kappa=0.4 s=0.25 (k_hat 0.323, TS 582);
#                                           k=1   from 1560 kappa=0.4 s=0.2  (k_hat 1.016, TS 616).
# 4 at a time x 3 threads, maxtime=3600 s per pass. Step 0: build + smoke (k must stay fixed). caffeinate: bind at launch.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
F="-O3 -std=c++17 -Wall -Wno-deprecated-declarations -I/opt/homebrew/include"
L="-L/opt/homebrew/lib -lnlopt -framework Accelerate -lpthread"
clang++ $F -DKINK -DKINK_S grid_estimator.cpp -o grid_estimator_kinks $L
seed() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', c(unlist(r[1,c('delta0_hat','delta1_hat','delta2_hat')]), $2, 0.2, unlist(r[1,paste0('gamma',1:11)]), 0)), sep=',')" 2>/dev/null; }
X03=$(seed $P/1560-kink-k0.4-s0.25-k00.2.csv 0.3)
X10=$(seed $P/1560-kink-k0.4-s0.2-k00.9.csv 1)
echo "seed k=0.3: $X03"; echo "seed k=1: $X10"
# smoke: tiny chains, k must come back exactly at k_fixed
./grid_estimator_kinks mode=lambdagrid qform=power_kink k_fixed=0.3 kink_share=0.2 row6=eps_psi input_csv=$IN lambdas=0.5 x0=$X03 \
  algo=neldermead n_burn=20 n_keep=40 n_threads=4 maxtime=10 base_seed=20260829 output_csv=$P/1563-smoke.csv > $P/1563-smoke.Rout 2>&1
Rscript -e "r <- read.csv('$P/1563-smoke.csv'); cat('smoke: k_hat', r\$k_hat, 's_hat', r\$s_hat, 'Lhat', r\$Lhat, '\n'); stopifnot(abs(r\$k_hat - 0.3) < 1e-12)"
run() { local X=$X03; [ "$1" = "1" ] && X=$X10
  ./grid_estimator_kinks mode=lambdagrid qform=power_kink k_fixed=$1 kink_share=0.2 row6=eps_psi input_csv=$IN lambdas=$2 x0=$X \
    algo=neldermead n_burn=1000 n_keep=3000 n_threads=3 maxtime=3600 base_seed=20260829 \
    output_csv=$P/1563-kinks-k$1-kappa$2.csv > $P/1563-kinks-k$1-kappa$2.Rout 2>&1; echo "done k=$1 kappa=$2"; }
n=0
for K in 0.3 1; do for KA in 0.3 0.4 0.5 0.6 0.7; do
  run $K $KA & n=$((n+1)); if [ $((n % 4)) -eq 0 ]; then wait; fi
done; done
wait; echo "all done"
