#!/bin/bash
# VAT gap among interior firms (Hans, 2026-10-03): T = E[L]/E[P], L = tau_P (1-q) e, P = t1/pgdp - tau_P M (static, Delta = 0),
# plus E[tau_P M] (true_credit) so the denominator's set is visible. theta fixed at the operating point 1616, gamma free.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PAR n_threads=${NT:-4}"
(/usr/bin/time -p ./grid_estimator_ind5b_cf4 $B cf_cold=1 cf_target=gap deltas=0 output_csv=$P/1644-cf-gap.csv > $P/1644-cf-gap.Rout 2>&1; echo "done gap $(date +%H:%M)") &
(/usr/bin/time -p ./grid_estimator_ind5b_cf4 $B cf_cold=1 cf_target=true_credit deltas=0 output_csv=$P/1644-cf-true-credit.csv > $P/1644-cf-true-credit.Rout 2>&1; echo "done true_credit $(date +%H:%M)") &
wait; echo "all done"
