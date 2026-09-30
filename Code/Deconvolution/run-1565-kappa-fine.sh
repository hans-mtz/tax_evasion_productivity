#!/bin/bash
# Fine kappa grid at k = 0.3 (Hans, 2026-09-29): 8 points strictly between 0.4 and 0.6, s estimated, KINK_S build
# (12 rows). Every point seeded from the best 1563 fit (k=0.3, kappa=0.5, TS 588.8). 4 at a time x 3 threads,
# maxtime=3600 s per pass.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
X0=$(Rscript -e "r <- read.csv('$P/1563-kinks-k0.3-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))])), sep=',')" 2>/dev/null)
echo "seed = $X0"
run() { ./grid_estimator_kinks mode=lambdagrid qform=power_kink k_fixed=0.3 kink_share=0.22 row6=eps_psi input_csv=$IN lambdas=$1 x0=$X0 \
          algo=neldermead n_burn=1000 n_keep=3000 n_threads=3 maxtime=3600 base_seed=20260829 \
          output_csv=$P/1565-kappa-fine-k0.3-kappa$1.csv > $P/1565-kappa-fine-k0.3-kappa$1.Rout 2>&1; echo "done kappa=$1"; }
n=0
for KA in 0.422 0.444 0.467 0.489 0.511 0.533 0.556 0.578; do
  run $KA & n=$((n+1)); if [ $((n % 4)) -eq 0 ]; then wait; fi
done
wait; echo "all done"
