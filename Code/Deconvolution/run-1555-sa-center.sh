#!/bin/bash
# Optimizer test (Nail Kashaev's suggestion, 2026-09-28): Nelder-Mead pass 1 -> simulated annealing (2 h) -> Nelder-Mead
# pass 2, at the S4 center (lambda1 = 0.5, year intercepts, tax row, row6 = eps*psi, design A), seeded from the NM-only
# fit 1554-yfe-l0.5 (TS = 1,822). Waits for the S4 grid (PID given as $1) to free the machine. 12 threads.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
[ -n "${1:-}" ] && while kill -0 "$1" 2>/dev/null; do sleep 30; done
P=../Products
X0=$(Rscript -e "r <- read.csv('$P/1554-yfe-l0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat', paste0('d0yr',82:91), paste0('gamma',1:20))])), sep=',')" 2>/dev/null)
./grid_estimator_yfe_sa mode=lambdagrid qform=exp_scale row6=eps_psi input_csv=$P/1546-stage2-input-designA-tau-trim0.005.csv \
  lambdas=0.5 x0=$X0 algo=neldermead n_burn=1000 n_keep=3000 n_threads=12 maxtime=3600 sa_time=7200 base_seed=20260829 \
  output_csv=$P/1555-sa-center-l0.5.csv > $P/1555-sa-center-l0.5.Rout 2>&1
echo "SA center done"
