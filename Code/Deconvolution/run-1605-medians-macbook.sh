#!/bin/bash
# Medians design on the MacBook (Hans, 2026-10-01): rung-5 base (rows 0-4, 6, 8, 9, 11; interior) + 9 median rows
# (ind_rows=median, rows 13-21) with targets from the stage-2-sample deconvolution (1603, all 9 industries; input 1604).
# k in {0.5, 0.65, 0.75}; kappa estimated (bound 20); delta bounds +-100; maxeval 800 x 21 (4 theta + 17 live gamma).
# Joint NM 2 passes, IS + mix, rho=prop21 (ladder D + D for the median rows: 1604-rhoD-med.txt), plant clusters,
# seed 30, n_keep 1000; start theta from the 1594 NM point with k at the grid value, gamma 0.
# MacBook: 10 of 12 cores (threads 4 + 3 + 3; Hans keeps 2). Outputs suffixed -macbook.
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv; ST=$P/1594-nm-2pass.csv; RS=/usr/local/bin/Rscript
KAP=$($RS -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
DM=$(sed -n 's/rho_D=//p' $P/1604-rhoD-med.txt)
getp() { $RS -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local K=$1 NT=$2 T=1605-med-k$1-macbook
  local X0=$($RS -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
  local B="qform=power_nokink k_fixed=$K row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DM sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=12,7,5 ind_rows=median"
  ./grid_estimator_ind5b mode=lambdagrid $B n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=$NT \
    maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=$NT output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T $(date +%H:%M)"; }
fit 0.5 4 & fit 0.65 3 & fit 0.75 3 &
wait; echo "all done"
