#!/bin/bash
# (delta1, delta2) grid on design i with k profiled (Hans, 2026-10-01): delta1 in {20, 23, 26} x delta2 in {2.9, 3.3, 3.7}
# pinned (delta1_fixed/delta2_fixed); k free in [0.3, 1.0] (start 0.7), delta0, kappa, gamma free. gamma_init=solve,
# IS + mix, rho=prop21 (1599-rhoD-ind), plant clusters, seed 30, start theta from the 1594 NM point (delta1/2 pinned, k 0.7),
# no chaining, joint NM 2 passes, maxeval 800 x 21, delta box 100, kappa bound 20. Mac mini, 3 fits x 4 threads.
# adiag at R = 1000 and 4000. A second seed only for irregular points (decided after).
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products; ST=$P/1594-nm-2pass.csv; IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.7, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local D1=$1 D2=$2 T=1614-i-d1_$1-d2_$2
  local B="qform=power_nokink k_fixed=0.7 row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
  /usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B k_free=1 k_min=0.3 k_max=1.0 delta1_fixed=$D1 delta2_fixed=$D2 n_keep=1000 lambdas=$KAP x0=$X0 \
    algo=neldermead n_passes=2 n_threads=4 gamma_init=solve maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  local K=$(Rscript -e "cat(read.csv('$P/$T.csv')\$k_hat)" 2>/dev/null)
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag ${B/k_fixed=0.7/k_fixed=$K} n_keep=$R par=$(getp $P/$T.csv) n_threads=4 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T k=$K $(date +%H:%M)"; }
export -f fit getp; export P ST IN KAP DI X0
for a in 20 23 26; do for b in 2.9 3.3 3.7; do echo "$a $b"; done; done | xargs -P 3 -L 1 bash -c 'fit $0 $1'
echo "all done"
