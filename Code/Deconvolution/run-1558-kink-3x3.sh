#!/bin/bash
# Kinked power detection (Hans, 2026-09-28): q = (e/(kappa*Mbar))^k up to the FOC ceiling c_k = (1+k)^(-1/k), flat
# beyond (FOC does not rationalize those draws: eps rows only); share beyond the kink fixed at s (row [10]); k estimated.
# 3x3: kappa in {0.5, 1, 2} x s in {0.2, 0.3, 0.4}. Tax row + row6 = eps*psi, design A, no year intercepts.
# Nelder-Mead, seeded from the power-form screen fit (1557-power_scale-0.5; k start 0.5, gamma11 = 0).
# Single-point processes (global g_kpow), 3 threads each, 4 at a time. maxtime=3600 s per pass.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
X0=$(Rscript -e "r <- read.csv('$P/1557-power_scale-0.5.csv'); cat(sprintf('%.15g', c(unlist(r[1,c('delta0_hat','delta1_hat','delta2_hat')]), 0.5, unlist(r[1,paste0('gamma',1:10)]), 0)), sep=',')" 2>/dev/null)
echo "seed = $X0"
run() { ./grid_estimator_kink mode=lambdagrid qform=power_kink kink_share=$2 row6=eps_psi input_csv=$IN lambdas=$1 x0=$X0 \
          algo=neldermead n_burn=1000 n_keep=3000 n_threads=3 maxtime=3600 base_seed=20260829 \
          output_csv=$P/1558-kink-k$1-s$2.csv > $P/1558-kink-k$1-s$2.Rout 2>&1; echo "done kappa=$1 s=$2"; }
n=0
for K in 0.5 1 2; do for S in 0.2 0.3 0.4; do
  run $K $S & n=$((n+1)); if [ $((n % 4)) -eq 0 ]; then wait; fi
done; done
wait; echo "all done"
