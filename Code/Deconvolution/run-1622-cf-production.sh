#!/bin/bash
# Counterfactual production run (Hans, 2026-10-02): mode=cfprofile at the passing operating point, design i,
# k = 0.75, kappa = 0.5 (1616-i-k0.75-kappa0.5, TS 23.7 < chi2_17 27.6). Delta in {-0.10, -0.05, 0, 0.05, 0.10, 0.20}.
# theta fixed, gamma free; IS + mix, rho=prop21 (1599-rhoD-ind; credit row out of rho), plant clusters, seed 30,
# n_keep 1000. Then 1621-cf-economy.R: (A) evader industries, (B) whole economy. Usage: run-1622-cf-production.sh <threads>
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
NT=${1:-4}; P=../Products; T=1622-cf-i-k0.75-kappa0.5
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
/usr/bin/time -p ./grid_estimator_ind5b_cf mode=cfprofile $B n_keep=1000 par=$PAR n_threads=$NT deltas=-0.1,-0.05,0,0.05,0.1,0.2 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
cd ../..; Rscript Code/Deconvolution/1621-cf-economy.R cf_csv=Code/Products/$T.csv out=Code/Products/1621-cf-economy-i-k0.75-kappa0.5.csv > Code/Products/1621-cf-economy-i-k0.75-kappa0.5.Rout 2>&1
echo "all done $(date +%H:%M)"
