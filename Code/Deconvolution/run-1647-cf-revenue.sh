#!/bin/bash
# Revenue check (Hans, 2026-10-05): under linear q the revenue CI was unbounded, so the counterfactual moved to claimed
# deductions; q is now concave (power) and the revenue units bug is fixed. Does a revenue target (row = t1/pgdp - C(Delta),
# t1's variance in Omega) give bounded sets? Delta = -0.3 and +0.3; claims (level) at the same Delta for comparison.
# Same configuration as run-1642/1645 (operating point 1616, seed, R = 1000, cf_cold=1), binary grid_estimator_ind5b_cf5.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PAR n_threads=3 cf_cold=1"
for tg in revenue level; do for d in -0.3 0.3; do
  o=$P/1647-cf-$tg-D$d
  (/usr/bin/time -p ./grid_estimator_ind5b_cf5 $B cf_target=$tg deltas=$d output_csv=$o.csv > $o.Rout 2>&1; echo "done $tg $d $(date +%H:%M)") &
done; done
wait; echo "all done"
