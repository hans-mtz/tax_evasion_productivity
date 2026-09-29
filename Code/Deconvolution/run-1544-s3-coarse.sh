#!/bin/bash
# S3 (Thesis/PLAN.md §9b): new q = lambda1*(1-exp(-e/Mbar_{j,t-1})), design A, moment set A, grid_estimator_s3.
# Step 1: polish the center (lambda1=0.5) from its own endpoint (1543), all 12 threads.
# Step 2: coarse lambda1 grid {0.1,0.3,0.7,0.9}, every point seeded from the polished center (no chaining),
#         one process, 4 points x 3 threads. maxtime=1800 s per pass. Refinement follows once the passing region is seen.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
seed() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat',paste0('gamma',1:9))])), sep=',')" 2>/dev/null; }
IN=../Products/1532-stage2-input-designA-trim0.005.csv
COMMON="mode=lambdagrid qform=exp_scale input_csv=$IN algo=neldermead n_burn=1000 n_keep=3000 n_threads=12 maxtime=1800 base_seed=20260829"

X0=$(seed ../Products/1543-s3-center-designA.csv); echo "center seed = $X0"
./grid_estimator_s3 $COMMON lambdas=0.5 x0=$X0 output_csv=../Products/1544-s3-center2-designA.csv > ../Products/1544-s3-center2-designA.Rout 2>&1
echo "center polished"

X0=$(seed ../Products/1544-s3-center2-designA.csv); echo "grid seed = $X0"
./grid_estimator_s3 $COMMON lambdas=0.1,0.3,0.7,0.9 x0=$X0 output_csv=../Products/1544-s3-coarse-designA.csv > ../Products/1544-s3-coarse-designA.Rout 2>&1
echo "coarse grid done"
