#!/bin/bash
# Diagnostic for the stalled 1605 medians fits (2026-10-01), one point each (k = 0.75), on the MacBook, 5 threads each:
#  d1: exact replication of 1600-i_med (old input 1598, old D 1600-rhoD-med, rows 19-20 dropped, maxeval 8000, delta box
#      60) with the current binary -- must reproduce TS 1115.14.
#  d2: same old settings (maxeval 8400 = 400 x 21, delta box 60) but the new stage-2 targets (input 1604, D 1604-rhoD-med,
#      all 9 median rows live).
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; ST=$P/1594-nm-2pass.csv; RS=/usr/local/bin/Rscript
KAP=$($RS -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
X0=$($RS -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
E="qform=power_nokink k_fixed=0.75 row6=eps_psi n_burn=0 rho=prop21 sampler=is proposal=mix cluster=plant base_seed=20260830 ind_rows=median"
run() { local T=$1; shift
  ./grid_estimator_ind5b mode=lambdagrid $E n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=5 maxtime=43200 kappa_max=20 \
    output_csv=$P/$T.csv "$@" > $P/$T.Rout 2>&1; echo "done $T $(date +%H:%M)"; }
run 1606-d1-replicate-macbook input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv rho_D=$(sed -n 's/rho_D=//p' $P/1600-rhoD-med.txt) drop_rows=12,7,5,19,20 maxeval=8000 &
run 1606-d2-newtargets-macbook input_csv=$P/1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv rho_D=$(sed -n 's/rho_D=//p' $P/1604-rhoD-med.txt) drop_rows=12,7,5 maxeval=8400 &
wait; echo "all done"
