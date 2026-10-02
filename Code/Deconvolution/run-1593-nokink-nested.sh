#!/bin/bash
# No kink + nested solver (Hans, 2026-09-30): qform=power_nokink (q = (e/(kappa Mbar))^k, FOC ceiling as a support
# restriction), interior only with plant ids (1592), 9 rows (6, 7, 10, 12 dropped; chi2_9 crit 16.9), rho=prop21,
# sampler=is, cluster=plant, nested=1 (outer NM over delta, kappa; inner L-BFGS over gamma, analytic gradient; fixed
# inner start within a pass; pass 2 from the best (theta, gamma) of pass 1). k in {0.25, 0.5, 0.75, 1} x seeds {30, 31},
# kappa estimated. Same start for every fit (no chaining): delta, kappa from the 1585 k=0.75 s30 fit, gamma 0; D from
# mode=rhoD at that start (k=0.75), fixed for all. n_keep 1000, outer maxeval 200 per pass. Work-stealing queue:
# 4 fits at a time x 3 threads, ordered by k so each k's two seeds run together. adiag at each fit's own seed.
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1592-stage2-input-designA-interior-plant-trim0.005.csv; B=./grid_estimator_kf5; ST=$P/1585-interior-k0.75-s20260830.csv
PAR0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
D=$($B mode=rhoD qform=power_nokink k_fixed=0.75 row6=eps_psi drop_rows=6,7,12 input_csv=$IN n_burn=0 n_keep=1000 base_seed=20260830 output_csv=/dev/null par=$PAR0 | sed -n 's/RHO_D: //p')
echo "rho_D=$D" > $P/1593-rhoD.txt
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
run() { local K=$1 S=$2 T=1593-k$1-s$2
  local X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
  local C="qform=power_nokink k_fixed=$K row6=eps_psi drop_rows=6,7,12 input_csv=$IN n_burn=0 rho=prop21 rho_D=$D sampler=is cluster=plant"
  $B mode=lambdagrid $C n_keep=1000 lambdas=$KAP x0=$X0 nested=1 n_passes=2 maxeval=200 n_threads=3 maxtime=14400 base_seed=$S \
    output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  $B mode=adiag $C n_keep=1000 par=$(getpar $P/$T.csv) n_threads=3 base_seed=$S output_csv=/dev/null > $P/$T-adiag.txt 2>&1
  echo "done k=$K s=$S $(date +%H:%M)"; }
export -f run getpar; export P IN B ST D KAP
printf "%s\n" "0.25 20260830" "0.25 20260831" "0.5 20260830" "0.5 20260831" "0.75 20260830" "0.75 20260831" "1 20260830" "1 20260831" \
  | xargs -P 4 -L 1 bash -c 'run $0 $1'
echo "all done"
