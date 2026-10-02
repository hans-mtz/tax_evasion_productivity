#!/bin/bash
# k grid on design i (Hans, 2026-10-01): params of interest k (and delta_1, delta_2, profiled). Design i = rung-5 base
# with the pooled eps row replaced by eps x industry (rows 0, 2-4, 6, 8, 9, 11 + 13-21; interior; grid_estimator_ind5b,
# ind_rows=eps). Concave k only: {0.25, 0.5, 0.9} (k = 0.75 = 1600-i). kappa estimated (bound 20), delta, gamma free.
# Same as 1600-i otherwise: joint NM 2 passes, maxeval 400 x 21, IS + mix, rho=prop21 with the 1599 industry D (fixed
# for every k), plant clusters, seed 30, n_keep 1000, start theta from the 1594 NM point with k at the grid value,
# gamma 0 (no chaining). 4 threads each, in parallel. adiag at R = 1000 and 4000.
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv; ST=$P/1594-nm-2pass.csv
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local K=$1 T=1602-i-k$1
  local X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
  local B="qform=power_nokink k_fixed=$K row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
  /usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=4 \
    maxtime=43200 maxeval=8400 kappa_max=20 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=4 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T $(date +%H:%M)"; }
fit 0.25 & fit 0.5 & fit 0.9 &
wait; echo "all done"
