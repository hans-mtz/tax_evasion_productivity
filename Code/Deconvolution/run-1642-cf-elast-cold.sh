#!/bin/bash
# Cold-start rerun of the 1638 claims elasticity (Hans, 2026-10-03): 1638 had identical hard and soft lower bounds (suspect
# profile jump from the warm gamma path). Same configuration as run-1638, binary grid_estimator_ind5b_cf3 with cf_cold=1, 12 threads.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PAR n_threads=12"
(/usr/bin/time -p ./grid_estimator_ind5b_cf3 $B cf_cold=1 cf_target=elast_claims deltas=0 output_csv=$P/1642-cf-elast-claims-cold.csv > $P/1642-cf-elast-claims-cold.Rout 2>&1; echo "done elast $(date +%H:%M)") &
wait; echo "all done"
