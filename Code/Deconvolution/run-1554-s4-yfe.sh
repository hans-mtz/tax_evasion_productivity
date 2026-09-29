#!/bin/bash
# S4 (Thesis/PLAN.md §9b): year intercepts delta0_82..91 + rows psi*1{year} (grid_estimator_yfe, YEAR_FE build, D_G_A=20,
# 33 free parameters), on top of: new q (exp_scale), design A, tax row psi*ln tau_P, row6 = eps*psi. One point per process.
# (1) center refit lambda1 = 0.5, 12 threads, seeded from the 1550 center (delta0_t = 0, gamma11..20 = 0);
# (2) coarse grid {0.1,0.3,0.7,0.9}, four single-point processes x 3 threads, seeded from (1). maxtime=3600 s per pass.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
C="mode=lambdagrid qform=exp_scale row6=eps_psi input_csv=$IN algo=neldermead n_burn=1000 n_keep=3000 maxtime=3600 base_seed=20260829"
X0=$(Rscript -e "r <- read.csv('$P/1550-s3tau-psi-center-designA.csv'); cat(sprintf('%.15g', c(unlist(r[1,c('delta0_hat','delta1_hat','delta2_hat')]), rep(0,10), unlist(r[1,paste0('gamma',1:10)]), rep(0,10))), sep=',')" 2>/dev/null)
echo "center seed = $X0"
[ -f $P/1554-yfe-l0.5.csv ] || ./grid_estimator_yfe $C lambdas=0.5 n_threads=12 x0=$X0 output_csv=$P/1554-yfe-l0.5.csv > $P/1554-yfe-l0.5.Rout 2>&1
echo "center done"
X0=$(Rscript -e "r <- read.csv('$P/1554-yfe-l0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat', paste0('d0yr',82:91), paste0('gamma',1:20))])), sep=',')" 2>/dev/null)
echo "grid seed = $X0"
for L in 0.1 0.3 0.7 0.9; do   # plain loop: BSD xargs -I caps the command at 255 bytes (33-value seed too long)
  ./grid_estimator_yfe $C lambdas=$L n_threads=3 x0=$X0 output_csv=$P/1554-yfe-l$L.csv > $P/1554-yfe-l$L.Rout 2>&1 &
done
wait
echo "grid done"
