#!/bin/bash
# Refined kink grid (Hans, 2026-09-29): the 3x3 (1558) fits better with smaller scale kappa and smaller share s beyond
# the kink (best kappa = 0.5, s = 0.2, TS 726; k at its 0.02 bound in 4 cells). Grid kappa in {0.25, 0.35, 0.5} x
# s in {0.05, 0.1, 0.2}, each from two k starts (0.5, 0.9), delta/gamma seeded from the best 1558 cell. 18 single-point
# processes, 4 at a time x 3 threads, maxtime=3600 s per pass. Same rows (tax row, eps*psi, share row), design A.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
seed() { Rscript -e "r <- read.csv('$P/1558-kink-k0.5-s0.2.csv'); cat(sprintf('%.15g', c(unlist(r[1,c('delta0_hat','delta1_hat','delta2_hat')]), $1, unlist(r[1,paste0('gamma',1:11)]))), sep=',')" 2>/dev/null; }
X05=$(seed 0.5); X09=$(seed 0.9)
run() { local X=$X05; [ "$3" = "0.9" ] && X=$X09
  ./grid_estimator_kink mode=lambdagrid qform=power_kink kink_share=$2 row6=eps_psi input_csv=$IN lambdas=$1 x0=$X \
    algo=neldermead n_burn=1000 n_keep=3000 n_threads=3 maxtime=3600 base_seed=20260829 \
    output_csv=$P/1559-kink-k$1-s$2-k0$3.csv > $P/1559-kink-k$1-s$2-k0$3.Rout 2>&1; echo "done kappa=$1 s=$2 kstart=$3"; }
n=0
for K in 0.25 0.35 0.5; do for S in 0.05 0.1 0.2; do for K0 in 0.5 0.9; do
  run $K $S $K0 & n=$((n+1)); if [ $((n % 4)) -eq 0 ]; then wait; fi
done; done; done
wait; echo "all done"
