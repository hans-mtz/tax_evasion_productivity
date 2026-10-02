#!/bin/bash
# Design ii at the Ecuador target (Hans, 2026-10-01): audit row with p = 0.108 (Carrillo et al. 2022: share of
# incorporated firms with a detected ghost deduction, an upper bound; well inside the no-kink ceiling 1/(1+k) = 0.571).
# G = top 10% of capital within industry (ii_k) and, as robustness, top 10% of V (ii_v). Otherwise identical to
# run-1600 ii_k / ii_v: rung-5 base (rows 0-4, 6, 8, 9, 11 + row 10), interior, no kink, k = 0.75, kappa estimated
# (bound 20), joint NM 2 passes, maxeval 400 x 14, IS + mix, rho=prop21 (same D as 1600 ii), plant clusters, seed 30,
# n_keep 1000; same start (1594 NM theta, gamma 0). 4 threads each.
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv; ST=$P/1594-nm-2pass.csv
X0=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
DA=$(sed -n 's/rho_D=//p' $P/1599-rhoD-audit.txt)
B="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DA sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=12,7,5 audit_p=0.108"
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
fit() { local G=$1 T=1601-ii_$1-p0108
  /usr/bin/time -p ./grid_estimator_s2 mode=lambdagrid $B audit_group=$G n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=4 \
    maxtime=43200 maxeval=5600 kappa_max=20 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./grid_estimator_s2 mode=adiag $B audit_group=$G n_keep=$R par=$(getp $P/$T.csv) n_threads=4 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T $(date +%H:%M)"; }
fit k & fit v &
wait; echo "all done"
