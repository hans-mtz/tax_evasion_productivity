#!/bin/bash
# adiag at the best KINK_S fit for each fixed k (run 1563): k=0.3 kappa=0.5; k=1 kappa=0.4. Same rows as the fits.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
for spec in "0.3 0.5" "1 0.4"; do
  set -- $spec; K=$1; KA=$2; F=$P/1563-kinks-k$K-kappa$KA.csv
  PAR=$(Rscript -e "r <- read.csv('$F'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))])), sep=',')" 2>/dev/null)
  echo "===== k=$K kappa=$KA; fit file TS=$(Rscript -e "r <- read.csv('$F'); cat(round(2*r\$n*r\$Lhat,2), ' s_hat=', round(r\$s_hat,3))" 2>/dev/null)"
  ./grid_estimator_kinks mode=adiag qform=power_kink k_fixed=$K row6=eps_psi input_csv=$IN par=$PAR \
    n_burn=1000 n_keep=3000 n_threads=12 base_seed=20260829 output_csv=/dev/null 2>&1 | sed -n '/Lhat (recomputed)/,$p'
done
