#!/bin/bash
# Phase 1 test (Hans, 2026-09-30): with Schennach's Prop. 2.1 dominating measure (rho=prop21) and hashed seeds,
# does chain length still matter, and do seeds? One point: 10-row system (interior only, rows 6, 7, 12 dropped),
# kinked power q, k = 0.75 fixed, kappa and s estimated (grid_estimator_kf3). n_keep in {1000, 5000} x seeds {30, 31}.
# Same start for all four: delta, kappa, s from the 1585 k=0.75 s30 fit, gamma 0. D from mode=rhoD at that start,
# fixed for all four. n_burn 1000, three NM passes. adiag at each fit's own seed and n_keep.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1585-stage2-input-designA-interior-trim0.005.csv; B=./grid_estimator_kf3
START=$P/1585-interior-k0.75-s20260830.csv
PAR0=$(Rscript -e "r <- read.csv('$START'); cat(sprintf('%.15g', c(unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat')]), rep(0,13))), sep=',')" 2>/dev/null)
X0=$(Rscript -e "r <- read.csv('$START'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat')]), rep(0,13))), sep=',')" 2>/dev/null)
KAP=$(Rscript -e "r <- read.csv('$START'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
C="rho=prop21 qform=power_kink k_fixed=0.75 row6=eps_psi drop_rows=6,7,12 input_csv=$IN n_burn=1000"
D=$($B mode=rhoD qform=power_kink k_fixed=0.75 row6=eps_psi drop_rows=6,7,12 input_csv=$IN n_burn=1000 n_keep=1000 base_seed=20260830 output_csv=/dev/null par=$PAR0 | sed -n 's/RHO_D: //p')
echo "rho_D=$D" > $P/1591-rhoD.txt
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
run() { local NK=$1 S=$2 T=1591-nk$1-s$2
  $B mode=lambdagrid $C rho_D=$D n_keep=$NK lambdas=$KAP x0=$X0 algo=neldermead n_passes=3 \
    n_threads=3 maxtime=7200 base_seed=$S output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  $B mode=adiag $C rho_D=$D n_keep=$NK par=$(getpar $P/$T.csv) n_threads=3 base_seed=$S output_csv=/dev/null > $P/$T-adiag.txt 2>&1
  echo "done nk=$NK s=$S"; }
for NK in 1000 5000; do for S in 20260830 20260831; do run $NK $S & done; done
wait; echo "all done"
