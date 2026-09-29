#!/bin/bash
# S2 (Thesis/PLAN.md §9b): design A vs all-corrected at a few lambdas, linear q, moment set A.
# Same 3 lambdas for both designs (low / S1 minimum / high); every point seeded from the SAME point: S1's endpoint at
# lambda=3.501e-7 (Code/Products/1541-s1-lambdagrid-allcorr.csv). 3 points per 12-thread process = 4 threads/point.
# maxtime=1800 s per pass. Run sequentially (allcorr, then designA); caffeinate bound to this script.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
X0=$(Rscript -e 'r <- read.csv("../Products/1541-s1-lambdagrid-allcorr.csv"); r <- r[abs(r$lambda-3.501e-7)<1e-12,];
  cat(sprintf("%.15g", unlist(r[c("delta0_hat","delta1_hat","delta2_hat",paste0("gamma",1:9))])), sep=",")' 2>/dev/null)
echo "x0 = $X0"
LAMS="3.912e-08,3.501e-07,8.412e-07"
for D in allcorr designA; do
  ./grid_estimator mode=lambdagrid input_csv=../Products/1532-stage2-input-$D-trim0.005.csv \
    lambdas=$LAMS x0=$X0 algo=neldermead n_burn=1000 n_keep=3000 n_threads=12 maxtime=1800 base_seed=20260829 \
    output_csv=../Products/1542-s2-$D.csv > ../Products/1542-s2-$D.Rout 2>&1
  echo "done $D"
done
