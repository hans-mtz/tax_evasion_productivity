#!/bin/bash
# S1-S3 test fits (Hans, 2026-10-01), k = 0.7, one fit each, Mac mini, 6 threads each:
#   iib : design iib (medians) -- input 1604, D = 1610-rhoD-med-boundedout (median rows inf = out of rho, S1),
#         drop_rows 12,7,5, ind_rows=median
#   i   : design i (eps by industry) -- input 1598, D = 1599-rhoD-ind, drop_rows 1,12,7,5, ind_rows=eps
# Both: null-direction guard (S2, built in), gamma_init=solve (S3), IS + mix, rho=prop21, plant clusters, seed 30,
# start theta from the 1594 NM point with k = 0.7, gamma 0 before the warm start (no chaining), joint NM 2 passes,
# maxeval 800 x 21, delta box 100, kappa bound 20. adiag at R = 1000 and 4000.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products; ST=$P/1594-nm-2pass.csv; K=0.7
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local T=$1 IN=$2 DF=$3 DROP=$4 IR=$5
  local B="qform=power_nokink k_fixed=$K row6=eps_psi input_csv=$P/$IN n_burn=0 rho=prop21 rho_D=$(sed -n 's/rho_D=//p' $P/$DF) sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=$DROP ind_rows=$IR"
  /usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=6 gamma_init=solve \
    maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=6 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T $(date +%H:%M)"; }
fit 1611-iib-k0.7 1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv 1610-rhoD-med-boundedout.txt 12,7,5 median &
fit 1611-i-k0.7 1598-stage2-input-designA-interior-plant-k-trim0.005.csv 1599-rhoD-ind.txt 1,12,7,5 eps &
wait; echo "all done"
