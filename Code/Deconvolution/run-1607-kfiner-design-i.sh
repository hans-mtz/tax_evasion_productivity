#!/bin/bash
# Finer k grid on design i (Hans, 2026-10-01): k in {0.71, 0.72, 0.725, 0.73, 0.74} between the 1604/1600-i soft-test minima 0.7 and 0.75;
# with the pooled eps row replaced by eps x industry (rows 0, 2-4, 6, 8, 9, 11 + 13-21; interior; grid_estimator_ind5b,
# ind_rows=eps). delta bounds +-100 (delta_max) and maxeval 800 x 21 (was +-60, 400 x 21). kappa estimated (bound 20).
# Same as 1600-i otherwise: joint NM 2 passes, IS + mix, rho=prop21 with the 1599 industry D (fixed
# for every k), plant clusters, seed 30, n_keep 1000, start theta from the 1594 NM point with k at the grid value,
# gamma 0 (no chaining). 2 threads each (Mac mini, 5 in parallel). adiag at R = 1000 and 4000.
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv; ST=$P/1594-nm-2pass.csv
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local K=$1 T=1607-i-k$1
  local X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
  local B="qform=power_nokink k_fixed=$K row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
  /usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=2 \
    maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=2 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T $(date +%H:%M)"; }
fit 0.71 & fit 0.72 & fit 0.725 & fit 0.73 & fit 0.74 &
wait; echo "all done"
