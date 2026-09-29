#!/bin/bash
# adiag at the best and worst kink fits of run 1560 (Hans, 2026-09-29): row stats, gamma, Omega sd/correlation,
# eigen-spectrum/truncation, tilted u/omega/ln B. Same rows as the fits (kink, share row, tax row, eps*psi).
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
for spec in "0.4 0.25 0.2 best_overall" "0.4 0.2 0.9 best_k1" "0.4 0.15 0.2 worst" "0.5 0.2 1.5 worst_k1"; do
  set -- $spec; K=$1; S=$2; K0=$3; LAB=$4
  F=$P/1560-kink-k$K-s$S-k0$K0.csv
  PAR=$(Rscript -e "r <- read.csv('$F'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat', paste0('gamma',1:11))])), sep=',')" 2>/dev/null)
  echo "===== $LAB: kappa=$K s=$S (start k $K0); fit file TS=$(Rscript -e "r <- read.csv('$F'); cat(round(2*r\$n*r\$Lhat,2), ' k_hat=', round(r\$k_hat,3))" 2>/dev/null)"
  ./grid_estimator_kink2 mode=adiag qform=power_kink kink_share=$S row6=eps_psi input_csv=$IN par=$PAR \
    n_burn=1000 n_keep=3000 n_threads=12 base_seed=20260829 output_csv=/dev/null 2>&1 | sed -n '/Lhat (recomputed)/,$p'
done
