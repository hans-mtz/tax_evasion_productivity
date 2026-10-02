#!/bin/bash
# eps*lnM by industry (Hans, 2026-09-30): IND5 build (grid_estimator_ind5), row 5 replaced by rows 13-21 = eps*lnM*1{j}
# for the 9 interior industries (313 321 322 324 331 342 351 352 369). Interior only, AK cut, rows 5, 6, 7, 12 dropped
# -> 18 rows (chi2_18 crit 28.9). Kinked power q, kappa and s estimated, n_keep=1000, n_burn=1000, three NM passes,
# shared start (delta from 1579 k=1, kappa 0.556, s 0.2, gamma 0). Seed 30 only (Hans), k in {0.5, 0.75, 1}. adiag at own seed.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1585-stage2-input-designA-interior-trim0.005.csv; B=./grid_estimator_ind5
D=$(Rscript -e "r <- read.csv('$P/1579-kgrid-kf-k1.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
Z22=$(printf ',0%.0s' $(seq 22))
run() { local K=$1 S=20260830 T=1588-ind5-k$1-s30
  local C="cut=ak qform=power_kink k_fixed=$K row6=eps_psi drop_rows=5,6,7,12 input_csv=$IN n_burn=1000"
  $B mode=lambdagrid $C n_keep=1000 lambdas=0.556 x0="$D,$K,0.2,0.556$Z22" algo=neldermead n_passes=3 \
    n_threads=4 maxtime=5400 base_seed=$S output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  $B mode=adiag $C n_keep=1000 par=$(getpar $P/$T.csv) n_threads=4 base_seed=$S output_csv=/dev/null > $P/$T-adiag.txt 2>&1
  echo "done k=$K"; }
for K in 0.5 0.75 1; do run $K & done
wait; echo "all done"
