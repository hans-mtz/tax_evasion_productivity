#!/bin/bash
# Phase 0 (audit 2026-09-30): (1) adiag at the 1588 fits with the IND5 jidx fix (TS/row t must be unchanged; tilted
# summaries corrected); (2) tail diagnostic (finding 1) at seed-30 fits of 1585/1587/1588, n_keep 1000 (as fitted) and
# 10000 (same parameters): exposure, a_i > 0, max kept u, firms with u > 8. No refits.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1585-stage2-input-designA-interior-trim0.005.csv
p13() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
p22() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
one() { local BIN=$1 K=$2 PAR=$3 DR=$4 NK=$5 OUT=$6
  ./$BIN mode=adiag cut=ak qform=power_kink k_fixed=$K row6=eps_psi drop_rows=$DR input_csv=$IN n_burn=1000 n_keep=$NK \
    par=$PAR n_threads=4 base_seed=20260830 output_csv=/dev/null > $P/$OUT 2>&1; }
jobs_list=()
for K in 0.5 0.75 1; do
  PAR=$(p22 $P/1588-ind5-k$K-s30.csv)
  for NK in 1000 10000; do jobs_list+=("grid_estimator_ind5b $K $PAR 5,6,7,12 $NK 1590-tail-1588-k$K-nk$NK.txt"); done
done
for T in 1585-interior-k0.5 1585-interior-k0.75 1587-kfine-k0.4 1587-kfine-k1; do
  K=${T##*-k}; PAR=$(p13 $P/$T-s20260830.csv)
  for NK in 1000 10000; do jobs_list+=("grid_estimator_kf3 $K $PAR 6,7,12 $NK 1590-tail-$T-nk$NK.txt"); done
done
n=0
for j in "${jobs_list[@]}"; do one $j & n=$((n+1)); if [ $((n % 3)) -eq 0 ]; then wait; fi; done
wait; echo "all done"
