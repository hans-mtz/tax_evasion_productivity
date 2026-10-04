#!/bin/bash
# Recheck of the interior loss_t1 upper bound (Hans, 2026-10-04): 1645-int had conservative upper 7.1% vs soft 4.9%.
# Same run plus cf_grid, gamma at each T started from the profile optimum at T_hat (cf_cold=0), to compare with the cold trace.
# Revenue lost to undetected overreporting as a share of sales tax owed on sales (Hans, 2026-10-04): T = E[L]/E[t1/pgdp + x],
# L = tau_P (1-q) e, static. x = extra t1/pgdp per interior firm from other firms (1621 totals / 12,050): 0 interior only;
# 130.86 (A) adds the trimmed top 0.5%; 2031.17 (B) adds every other firm (trimmed, corners in all industries). Their L = 0.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PAR n_threads=${NT:-12}"
(/usr/bin/time -p ./grid_estimator_ind5b_cf4 $B cf_target=loss_t1 cf_t1_extra=0 cf_grid=0.0550,0.0600,0.0625,0.0650,0.0675,0.0700,0.0725,0.0750,0.0800 deltas=0 output_csv=$P/1646-cf-loss-t1-int-profile-warm.csv > $P/1646-cf-loss-t1-int-profile-warm.Rout 2>&1; echo "done profile $(date +%H:%M)") &
wait; echo "all done"
