#!/bin/bash
# Detection-form screening (Hans, 2026-09-28), full-rank rows (tax row + row6 = eps*psi), design A, no year intercepts,
# Nelder-Mead only (screen), grid_estimator_tau2. Seven single-point jobs, 4 at a time x 3 threads:
#   power_scale  q = (e/Mbar)^k,        k in {0.25, 0.5, 0.75}
#   exp_scale    q = 1 - exp(-e/Mbar),  lambda1 = 1 (form 2 with the scale)
#   linear_new   q = lambda*e (levels), lambda in {1e-7, 5.427e-7, 3e-6} -- old headline q with the corrected rows
# All seeded from the 1550 center fit (same rows; delta's from the exp form at lambda1 = 0.5). maxtime=3600 s per pass.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
X0=$(Rscript -e "r <- read.csv('$P/1550-s3tau-psi-center-designA.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat', paste0('gamma',1:10))])), sep=',')" 2>/dev/null)
run() { ./grid_estimator_tau2 mode=lambdagrid qform=$1 row6=eps_psi input_csv=$IN lambdas=$2 x0=$X0 algo=neldermead \
          n_burn=1000 n_keep=3000 n_threads=3 maxtime=3600 base_seed=20260829 output_csv=$P/1557-$1-$2.csv > $P/1557-$1-$2.Rout 2>&1; echo "done $1 $2"; }
run power_scale 0.25 & run power_scale 0.5 & run power_scale 0.75 & run exp_scale 1 &
wait
run linear_new 1e-7 & run linear_new 5.427e-7 & run linear_new 3e-6 &
wait
echo "all done"
