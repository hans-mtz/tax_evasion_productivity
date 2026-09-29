#!/bin/bash
# S3 center refit (Thesis/PLAN.md §9b): NEW detection function q = lambda1*(1-exp(-e/Mbar_{j,t-1})) (qform=exp_scale,
# binary grid_estimator_s3), design A input, moment set A. One point, lambda1 = 0.5, all 12 threads on it.
# Seed: S2's design-A fit at lambda = 3.501e-7 (Code/Products/1542-s2-designA.csv) -- same design, linear-q optimum;
# purpose of this refit is a proper seed for the lambda1 grid (the linear-q point may be a poor start under the new q).
# maxtime=1800 s per pass; caffeinate bound to the process.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
X0=$(Rscript -e 'r <- read.csv("../Products/1542-s2-designA.csv"); r <- r[abs(r$lambda-3.501e-7)<1e-12,];
  cat(sprintf("%.15g", unlist(r[c("delta0_hat","delta1_hat","delta2_hat",paste0("gamma",1:9))])), sep=",")' 2>/dev/null)
echo "x0 = $X0"
nohup ./grid_estimator_s3 mode=lambdagrid qform=exp_scale input_csv=../Products/1532-stage2-input-designA-trim0.005.csv \
  lambdas=0.5 x0=$X0 algo=neldermead n_burn=1000 n_keep=3000 n_threads=12 maxtime=1800 base_seed=20260829 \
  output_csv=../Products/1543-s3-center-designA.csv > ../Products/1543-s3-center-designA.Rout 2>&1 &
P=$!
nohup caffeinate -dims -w $P >/dev/null 2>&1 &
echo "S3 center launched, PID $P"
