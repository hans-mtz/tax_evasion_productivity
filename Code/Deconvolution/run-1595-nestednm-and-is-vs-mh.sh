#!/bin/bash
# (a) Nested with Nelder-Mead inner (Schennach App. G: simplex over gamma at fixed theta) at the 1594 point: k = 0.75,
#     seed 30, same start and system as 1594 (no kink, 9 rows, rho=prop21, IS + proposal=mix (now 3 components),
#     plant-clustered), n_keep 1000, 1 pass; adiag at R = 1000 and 4000.
# (b) IS vs MH evidence at the 1594 NM-2pass point (fixed theta, gamma; adiag only):
#     precision: 5 seeds x {IS + mix, MH (uniform proposal)} at R = 1000; smoothness: TS along gamma x c,
#     c in {0.98, 0.985, ..., 1.02}, both samplers, seed 30.
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1592-stage2-input-designA-interior-plant-trim0.005.csv; B=./grid_estimator_s2; ST=$P/1585-interior-k0.75-s20260830.csv
D=$(sed -n 's/rho_D=//p' $P/1594-rhoD.txt)
X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
C="qform=power_nokink k_fixed=0.75 row6=eps_psi drop_rows=6,7,12 input_csv=$IN n_burn=0 rho=prop21 rho_D=$D cluster=plant"
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', c(unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat')]), $2 * unlist(r[1, paste0('gamma',1:13)]))), sep=',')" 2>/dev/null; }
( /usr/bin/time -p $B mode=lambdagrid $C sampler=is proposal=mix n_keep=1000 lambdas=$KAP x0=$X0 nested=1 inner_algo=neldermead n_passes=1 \
    n_threads=6 maxtime=21600 base_seed=20260830 output_csv=$P/1595-nested-nm-1pass.csv > $P/1595-nested-nm-1pass.Rout 2>&1
  for R in 1000 4000; do $B mode=adiag $C sampler=is proposal=mix n_keep=$R par=$(getpar $P/1595-nested-nm-1pass.csv 1) n_threads=6 \
    base_seed=20260830 output_csv=/dev/null > $P/1595-nested-nm-1pass-adiag-R$R.txt 2>&1; done
  echo "done nested-nm $(date +%H:%M)" ) &
(
  NM=$P/1594-nm-2pass.csv
  for S in 20260830 20260831 20260832 20260833 20260834; do
    $B mode=adiag $C sampler=is proposal=mix n_keep=1000 par=$(getpar $NM 1) n_threads=6 base_seed=$S output_csv=/dev/null > $P/1595-prec-is-s$S.txt 2>&1
    $B mode=adiag $C sampler=mh n_burn=1000 n_keep=1000 par=$(getpar $NM 1) n_threads=6 base_seed=$S output_csv=/dev/null > $P/1595-prec-mh-s$S.txt 2>&1
  done
  for c in 0.98 0.985 0.99 0.995 1 1.005 1.01 1.015 1.02; do
    $B mode=adiag $C sampler=is proposal=mix n_keep=1000 par=$(getpar $NM $c) n_threads=6 base_seed=20260830 output_csv=/dev/null > $P/1595-line-is-c$c.txt 2>&1
    $B mode=adiag $C sampler=mh n_burn=1000 n_keep=1000 par=$(getpar $NM $c) n_threads=6 base_seed=20260830 output_csv=/dev/null > $P/1595-line-mh-c$c.txt 2>&1
  done
  echo "done is-vs-mh $(date +%H:%M)" ) &
wait; echo "all done"
