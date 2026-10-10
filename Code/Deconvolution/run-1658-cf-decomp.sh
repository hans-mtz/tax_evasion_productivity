#!/bin/bash
# 1658 (2026-10-09, Hans): read-only check of the asymmetry in the overreporting response (ch. 8, R4). Binary grid_estimator_ind5b_cf7,
# mode=cfprofile cf_target=diff_evasion cf_decomp=1: at the operating weights (point 1616, gamma of the fit, no gamma solve), split the
# overreporting response at +-1% and +-2% by the draw's current detection probability q (bins: draws that stop at the cut, then
# q < 0.02, 0.05, 0.15, rest). Same inputs, seed and draws as run-1654-cf-targets.sh (TAG 1655); the bins must add up to the
# "T at operating gamma" of 1655-cf-diff_evasion-mr1-D+-0.01/0.02. Output Products/1658-cf-decomp-evasion.{csv,Rout}.
set -uo pipefail
export LC_ALL=C PATH=/usr/local/bin:$PATH
cd "$(dirname "$0")/../C-estimator"
P=../Products; NT=${NT:-8}
PARV=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PARV n_threads=$NT cf_mresp=1"
o=$P/1658-cf-decomp-evasion
/usr/bin/time -p ./grid_estimator_ind5b_cf7 $B cf_target=diff_evasion cf_decomp=1 deltas=0.01,0.02 output_csv=$o.csv > $o.Rout 2>&1; echo "exit $?"
