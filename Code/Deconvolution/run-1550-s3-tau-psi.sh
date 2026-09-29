#!/bin/bash
# S3 + tax row + row [6] = eps*psi (replaces eps*e, whose peso scale collapsed the eigen-cut; log 2026-09-28).
# + row [9] psi*ln(tau_P) (firm's own rate; grid_estimator_tau, TAU_ROW build). Same sequence as run-1544 so the only
# change is row [6]: (1) center refit at lambda1 = 0.5, 12 threads, seeded from the 1547 center (same rows except [6]);
# (2) coarse lambda1 grid {0.1,0.3,0.7,0.9} seeded from (1), one process, 4 points x 3 threads.
# maxtime=1800 s per pass. Critical value chi2_{10,.95} = 18.31.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
IN=../Products/1546-stage2-input-designA-tau-trim0.005.csv
COMMON="mode=lambdagrid qform=exp_scale row6=eps_psi input_csv=$IN algo=neldermead n_burn=1000 n_keep=3000 n_threads=12 maxtime=1800 base_seed=20260829"
seed() { Rscript -e "r <- read.csv('$1'); g <- r[1, grep('^gamma', names(r))]; if (length(g) < 10) g <- c(unlist(g), 0);
  cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), unlist(g))), sep=',')" 2>/dev/null; }

X0=$(seed ../Products/1547-s3tau-center-designA.csv); echo "center seed = $X0"
./grid_estimator_tau $COMMON lambdas=0.5 x0=$X0 output_csv=../Products/1550-s3tau-psi-center-designA.csv > ../Products/1550-s3tau-psi-center-designA.Rout 2>&1
echo "center done"
X0=$(seed ../Products/1550-s3tau-psi-center-designA.csv); echo "grid seed = $X0"
./grid_estimator_tau $COMMON lambdas=0.1,0.3,0.7,0.9 x0=$X0 output_csv=../Products/1550-s3tau-psi-coarse-designA.csv > ../Products/1550-s3tau-psi-coarse-designA.Rout 2>&1
echo "coarse grid done"
