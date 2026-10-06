#!/bin/bash
# Revenue rerun with the audited solver (Hans, 2026-10-05): 1647 left gamma10 at its start (TS_min stuck at the operating 23.689);
# binary grid_estimator_ind5b_cf6 with cf_multi=1 (several gamma starts per profiled solve, hard set = union of accepted T).
# Revenue and level at Delta = -0.3 and +0.3, otherwise the 1647 configuration (1616 operating point, seed, R = 1000, cf_cold=1).
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PAR n_threads=3 cf_cold=1 cf_multi=1"
for tg in revenue level; do for d in -0.3 0.3; do
  o=$P/1650-cf-$tg-D$d
  (/usr/bin/time -p ./grid_estimator_ind5b_cf6 $B cf_target=$tg deltas=$d output_csv=$o.csv > $o.Rout 2>&1; echo "done $tg $d $(date +%H:%M)") &
done; done
wait; echo "all done"
