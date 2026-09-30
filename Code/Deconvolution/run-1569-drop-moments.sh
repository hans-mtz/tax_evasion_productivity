#!/bin/bash
# Drop-moment factorial (Hans, 2026-09-29): all 15 non-empty subsets of the candidate rows {11 kappa-score, 6 eps*psi,
# 8 k-score, 2 psi*lnM}, at the best point (k=0.3, kappa=0.556, s=0.20 fixed), n_keep=3000. Every fit seeded from the
# no-drop fit (1568-chain-nkeep3000, TS 581.9); two NM passes; then adiag at base_seed 20260830/20260831 (parameters
# and gamma held) for the seed spread. Dropped rows are zeroed inside the moment function (drop_rows). 4 at a time x 3 threads.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
X0=$(Rscript -e "r <- read.csv('$P/1568-chain-nkeep3000.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))])), sep=',')" 2>/dev/null)
C="qform=power_kink k_fixed=0.3 s_fixed=0.2 row6=eps_psi input_csv=$IN n_burn=1000 n_keep=3000"
run() { local D=$1 TAG=${1//,/-}
  ./grid_estimator_kinks3 mode=lambdagrid $C drop_rows=$D lambdas=0.556 x0=$X0 algo=neldermead n_threads=3 maxtime=3600 base_seed=20260829 \
    output_csv=$P/1569-drop-$TAG.csv > $P/1569-drop-$TAG.Rout 2>&1
  PAR=$(Rscript -e "r <- read.csv('$P/1569-drop-$TAG.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))])), sep=',')" 2>/dev/null)
  for SEED in 20260830 20260831; do
    TS=$(./grid_estimator_kinks3 mode=adiag $C drop_rows=$D par=$PAR n_threads=3 base_seed=$SEED output_csv=/dev/null 2>&1 | grep "Lhat (recomputed)" | sed 's/.*TS = 2 n Lhat = \([0-9.]*\).*/\1/')
    echo "drop=$D seed=$SEED TS=$TS" >> $P/1569-drop-seed-spread.txt
  done; echo "done drop=$D"; }
n=0
for D in 11 6 8 2 11,6 11,8 11,2 6,8 6,2 8,2 11,6,8 11,6,2 11,8,2 6,8,2 11,6,8,2; do
  run $D & n=$((n+1)); if [ $((n % 4)) -eq 0 ]; then wait; fi
done
wait; echo "all done"
