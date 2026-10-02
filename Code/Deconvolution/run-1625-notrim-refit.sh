#!/bin/bash
# Untrimmed refit of the operating point (Hans, 2026-10-02, priority): design i on 1624 (no top-0.5% trim), k = 0.75 and
# kappa = 0.5 pinned, delta's started from the trimmed best fit 1616-i-k0.75-kappa0.5, gamma_init=solve, same D
# (1599-rhoD-ind; only the sample changes), IS + mix, plant clusters, seed 30, joint NM 2 passes, maxeval 800 x 21,
# delta box 100. Mac mini, 12 threads. adiag at R = 1000 and 4000. Then, if TS <= chi2_17 (27.59): the counterfactual
# (level over the 6 Deltas, claims elasticity, behavioural difference at -5% / +10%, overreporting elasticity of u as a
# sanity check is NOT included -- elast_x is tail-dominated) with grid_estimator_ind5b_cf2, and 1621 with trim = 0.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products; T=1625-i-notrim-k0.75-kappa0.5; IN=$P/1624-stage2-input-designA-interior-plant-k-notrim.csv
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
X0=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, 0.5, rep(0,22))), sep=',')" 2>/dev/null)
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
B="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
/usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B kappa_fixed=0.5 n_keep=1000 lambdas=0.5 x0=$X0 algo=neldermead n_passes=2 n_threads=12 \
  gamma_init=solve maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=12 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
TS=$(grep -o 'TS = 2 n Lhat = [0-9.]*' $P/$T-adiag-R1000.txt | awk '{print $NF}'); echo "fit done $(date +%H:%M) TS $TS"
if awk -v t="$TS" 'BEGIN{exit !(t <= 27.59)}'; then
  PAR=$(getp $P/$T.csv); C="mode=cfprofile $B par=$PAR n_keep=1000 n_threads=12"
  ./grid_estimator_ind5b_cf2 $C cf_target=level deltas=-0.1,-0.05,0,0.05,0.1,0.2 output_csv=$P/1626-cf-level-notrim.csv > $P/1626-cf-level-notrim.Rout 2>&1
  ./grid_estimator_ind5b_cf2 $C cf_target=elast_claims deltas=0 output_csv=$P/1626-cf-elast-claims-notrim.csv > $P/1626-cf-elast-claims-notrim.Rout 2>&1
  ./grid_estimator_ind5b_cf2 $C cf_target=diff_beh deltas=-0.05,0.1 output_csv=$P/1626-cf-diffbeh-notrim.csv > $P/1626-cf-diffbeh-notrim.Rout 2>&1
  cd ../..; Rscript Code/Deconvolution/1621-cf-economy.R cf_csv=Code/Products/1626-cf-level-notrim.csv out=Code/Products/1621-cf-economy-notrim.csv trim=0 > Code/Products/1621-cf-economy-notrim.Rout 2>&1
  echo "counterfactual done $(date +%H:%M)"
else echo "does not pass: counterfactual skipped"; fi
echo "all done"
