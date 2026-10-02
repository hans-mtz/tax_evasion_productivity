#!/bin/bash
# Moment re-test ladder on the fixed estimator (Hans, 2026-09-30): "drop one at a time until it breaks", starting from
# the full row set. One rung per call: run-1597-ladder.sh <tag> <drop_rows> <input: all|interior>.
# Fixed estimator: grid_estimator_s2, joint NM 2 passes (initial steps, maxeval 200 x free dims), sampler=is,
# proposal=mix (3 components), rho=prop21 with ONE D for every rung (rhoD at the start, all rows live), cluster=plant,
# seed 30 (hashed), n_keep 1000; qform=power_nokink, k = 0.75 fixed, kappa estimated. Start: theta from the 1594 NM
# point, gamma 0. adiag at R = 1000 (own) and 4R = 4000.
set -uo pipefail
TAG=$1; DROP=$2; WHICH=${3:-all}
cd "$(dirname "$0")/../C-estimator"
P=../Products; B=./grid_estimator_s2; ST=$P/1594-nm-2pass.csv
IN=$P/1596-stage2-input-designA-plant-trim0.005.csv; [ "$WHICH" = "interior" ] && IN=$P/1592-stage2-input-designA-interior-plant-trim0.005.csv
PAR0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('kappa_hat','delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
if [ ! -s $P/1597-rhoD.txt ]; then   # one D for the whole ladder: all rows live, full sample, at the start
  D=$($B mode=rhoD qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1596-stage2-input-designA-plant-trim0.005.csv n_burn=0 n_keep=1000 \
      base_seed=20260830 output_csv=/dev/null par=$PAR0 | sed -n 's/RHO_D: //p'); echo "rho_D=$D" > $P/1597-rhoD.txt
fi
D=$(sed -n 's/rho_D=//p' $P/1597-rhoD.txt)
C="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$D sampler=is proposal=mix cluster=plant base_seed=20260830"
[ -n "$DROP" ] && C="$C drop_rows=$DROP"
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
T=1597-$TAG
# From rung 3 on (Hans, 2026-09-30): kappa_max=20 and maxeval = 400 x free dims (4 theta + live gamma rows; rows
# 0-9, 11, 12 under no kink, minus the dropped ones). Rungs 1-2 ran with kappa_max 5 and 200 x free dims.
NLIVE=12; [ -n "$DROP" ] && NLIVE=$((12 - $(echo "$DROP" | tr ',' '\n' | grep -vc '^10$')))
MAXEV=$((400 * (4 + NLIVE)))
/usr/bin/time -p $B mode=lambdagrid $C n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=${NT:-6} maxtime=43200 \
  maxeval=$MAXEV kappa_max=20 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
for R in 1000 4000; do $B mode=adiag $C n_keep=$R par=$(getpar $P/$T.csv) n_threads=6 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
echo "done $T $(date +%H:%M)"
