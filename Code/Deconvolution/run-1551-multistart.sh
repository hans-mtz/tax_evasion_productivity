#!/bin/bash
# Optimizer check (log 2026-09-28): tax row + row6=eps_psi, design A, lambda1 = 0.1, four dispersed starts, 3 threads each.
# s1 = current best (1550 coarse, lambda1 = 0.1) -- control; s2..s4 = (delta0,delta1,delta2) from earlier optima with gamma = 0.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products
BEST=$(Rscript -e "r <- read.csv('$P/1550-s3tau-psi-coarse-designA.csv'); r <- r[abs(r\$lambda-0.1)<1e-9,]; cat(sprintf('%.15g', unlist(r[c('delta0_hat','delta1_hat','delta2_hat',paste0('gamma',1:10))])), sep=',')" 2>/dev/null)
Z="0,0,0,0,0,0,0,0,0,0"
printf "s1 %s\ns2 5.3,5.15,0.78,%s\ns3 3.46,4.3,0.54,%s\ns4 4.3,8.3,1.8,%s\n" "$BEST" "$Z" "$Z" "$Z" | xargs -P 4 -L 1 bash -c '
  ./grid_estimator_tau mode=lambdagrid qform=exp_scale row6=eps_psi input_csv='"$P"'/1546-stage2-input-designA-tau-trim0.005.csv \
    lambdas=0.1 x0=$1 algo=neldermead n_burn=1000 n_keep=3000 n_threads=3 maxtime=1800 base_seed=20260829 \
    output_csv='"$P"'/1551-ms-$0.csv > '"$P"'/1551-ms-$0.Rout 2>&1; echo "done $0"'
