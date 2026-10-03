#!/bin/bash
# Behavioural change at small Delta (Hans, 2026-10-03: "it is an elasticity"): diff_beh at Delta = +-1, +-1.5, +-2%.
# Same configuration as run-1638: operating point 1616 (k = 0.75, kappa = 0.5, 0.5% trim), 1598 input, D 1599, IS + mix,
# plant clusters, seed 30, R = 1000, binary grid_estimator_ind5b_cf2; 12 threads.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PAR n_threads=12"
(/usr/bin/time -p ./grid_estimator_ind5b_cf2 $B cf_target=diff_beh deltas=-0.02,-0.015,-0.01,0.01,0.015,0.02 output_csv=$P/1639-cf-diffbeh-small.csv > $P/1639-cf-diffbeh-small.Rout 2>&1; echo "done diffbeh $(date +%H:%M)") &
wait; echo "all done"
