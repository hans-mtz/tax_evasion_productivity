#!/bin/bash
# Step C.1 (Hans, 2026-09-30): nested single pass vs Nelder-Mead two passes without nesting, one point.
# k = 0.75, seed 30, no kink (qform=power_nokink), 9 rows (6, 7, 10, 12 dropped), interior + plant ids (1592),
# rho=prop21 (D from 1593 at the 1585 k0.75 start; row 10 = 0, dropped), sampler=is, proposal=mix, cluster=plant,
# n_keep 1000. Same start: delta, kappa from the 1585 k0.75 s30 fit, gamma 0. Nested: fixed inner start, 1 pass.
# NM: joint over theta and gamma, 2 passes, initial steps, maxeval 200 x free dims. Both at 6 threads, in parallel.
# Then adiag at each fit with R = 1000 (own) and 4R = 4000.
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1592-stage2-input-designA-interior-plant-trim0.005.csv; B=./grid_estimator_s2; ST=$P/1585-interior-k0.75-s20260830.csv
D=$(sed -n 's/rho_D=//p' $P/1593-rhoD.txt | awk -F, 'BEGIN{OFS=","}{$11=0; print}'); echo "rho_D=$D" > $P/1594-rhoD.txt
X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
C="qform=power_nokink k_fixed=0.75 row6=eps_psi drop_rows=6,7,12 input_csv=$IN n_burn=0 rho=prop21 rho_D=$D sampler=is proposal=mix cluster=plant base_seed=20260830"
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
fit() { local T=$1; shift
  /usr/bin/time -p $B mode=lambdagrid $C n_keep=1000 lambdas=$KAP x0=$X0 n_threads=6 maxtime=21600 output_csv=$P/$T.csv "$@" > $P/$T.Rout 2>&1
  for R in 1000 4000; do $B mode=adiag $C n_keep=$R par=$(getpar $P/$T.csv) n_threads=6 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T $(date +%H:%M)"; }
fit 1594-nested-1pass nested=1 n_passes=1 &
fit 1594-nm-2pass algo=neldermead n_passes=2 &
wait; echo "all done"
