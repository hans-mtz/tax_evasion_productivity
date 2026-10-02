#!/bin/bash
# kappa grid with k estimated (Hans, 2026-09-30): IND5 system (18 rows: rows 5, 6, 7, 12 dropped; eps*lnM by industry),
# interior only, AK cut, kinked power q; kappa FIXED in {0.4, 0.5, 0.6}; k ESTIMATED (k_free=1, bounds [0.05, 2.5],
# start 0.5); s, delta, gamma free; gamma on dropped rows pinned at 0 (new). Seed 30 only. n_keep=1000, n_burn=1000,
# three NM passes. Shared start: delta from the 1588 k=0.5 fit, k 0.5, s 0.2, gamma 0. adiag at own seed.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1585-stage2-input-designA-interior-trim0.005.csv; B=./grid_estimator_ind5k
D=$(Rscript -e "r <- read.csv('$P/1588-ind5-k0.5-s30.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
Z22=$(printf ',0%.0s' $(seq 22))
run() { local KA=$1 S=20260830 T=1589-kappa$1-kfree-s30
  local C="cut=ak qform=power_kink k_fixed=0.5 k_free=1 row6=eps_psi drop_rows=5,6,7,12 input_csv=$IN n_burn=1000"
  $B mode=lambdagrid $C n_keep=1000 lambdas=$KA x0="$D,0.5,0.2$Z22" algo=neldermead n_passes=3 \
    n_threads=4 maxtime=5400 base_seed=$S output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  $B mode=adiag $C n_keep=1000 par=$(getpar $P/$T.csv) n_threads=4 base_seed=$S output_csv=/dev/null > $P/$T-adiag.txt 2>&1
  echo "done kappa=$KA"; }
for KA in 0.4 0.5 0.6; do run $KA & done
wait; echo "all done"
