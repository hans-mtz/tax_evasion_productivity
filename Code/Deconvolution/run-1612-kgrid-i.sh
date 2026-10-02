#!/bin/bash
# Coarse k grid on design i (eps by industry; the leading design), Hans 2026-10-01: k in {0.4, 0.5, 0.6, 0.7, 0.8}.
# New binary (correlation-scaled null guard, continuous penalty), gamma_init=solve, IS + mix, rho=prop21 (1599-rhoD-ind),
# plant clusters, seed 30, start theta from the 1594 NM point with k at the grid value, gamma 0 before the warm start
# (no chaining), joint NM 2 passes, maxeval 800 x 21, delta box 100, kappa bound 20. Mac mini: 3 fits at a time x 4
# threads. adiag at R = 1000 and 4000.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products; ST=$P/1594-nm-2pass.csv; IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local K=$1 T=1612-i-k$1
  local X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
  local B="qform=power_nokink k_fixed=$K row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
  /usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=4 gamma_init=solve \
    maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=4 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T $(date +%H:%M)"; }
export -f fit getp; export P ST IN KAP DI
printf "%s\n" 0.7 0.6 0.8 0.5 0.4 | xargs -P 3 -I{} bash -c 'fit {}'
echo "all done"
