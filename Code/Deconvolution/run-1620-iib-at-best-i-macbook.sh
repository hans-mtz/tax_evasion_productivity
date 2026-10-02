#!/bin/bash
# Design iib (deconvolution medians, robustness) at design i's best point (Hans, 2026-10-02): k = 0.75, kappa = 0.5 pinned
# (1616-i-k0.75-kappa0.5, TS 23.7); delta0-2 and gamma free, start delta from that design-i fit. gamma_init=solve, median
# rows out of rho (1610-rhoD-med-boundedout), IS + mix, plant clusters, seed 30, joint NM 2 passes, maxeval 800 x 21,
# delta box 100. MacBook, 9 threads (3 left for Hans). Waits for the 1617 kappa profile to finish. adiag at R = 1000 and 4000.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../Products"
until grep -q "all done" 1617-launch-macbook.log 2>/dev/null; do sleep 120; done
cd ../C-estimator
RS=/usr/local/bin/Rscript; P=../Products; T=1620-iib-k0.75-kappa0.5-macbook
IN=$P/1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv; D=$(sed -n 's/rho_D=//p' $P/1610-rhoD-med-boundedout.txt)
X0=$($RS -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, 0.5, rep(0,22))), sep=',')" 2>/dev/null)
getp() { $RS -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
B="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$D sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=12,7,5 ind_rows=median"
echo "start $T $(date +%H:%M)"
/usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B kappa_fixed=0.5 n_keep=1000 lambdas=0.5 x0=$X0 algo=neldermead n_passes=2 n_threads=9 \
  gamma_init=solve maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=9 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
echo "done $T $(date +%H:%M)"; echo "all done"
