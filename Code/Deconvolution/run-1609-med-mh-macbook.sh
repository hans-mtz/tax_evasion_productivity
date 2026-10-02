#!/bin/bash
# MH test on the medians design (Hans, 2026-10-01): one point, k = 0.75, same as 1605-med-k0.75 (input 1604 with the
# stage-2 deconvolution medians, D = 1604-rhoD-med, rows 0, 2-4, 6, 8, 9, 11 + 9 median rows, interior, rho=prop21,
# plant clusters, seed 30, start theta from the 1594 NM point, gamma 0) but sampler=mh (n_burn 1000, n_keep 1000,
# MH's own uniform proposal; proposal=mix is IS-only) instead of IS. Joint NM 2 passes, maxeval 800 x 21, delta box 100,
# kappa bound 20. MacBook, 10 threads. adiag (MH) at R = 1000 and 4000.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products; ST=$P/1594-nm-2pass.csv; RS=/usr/local/bin/Rscript; T=1609-med-mh-k0.75-macbook
IN=$P/1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv; D=$(sed -n 's/rho_D=//p' $P/1604-rhoD-med.txt)
KAP=$($RS -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
X0=$($RS -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
getp() { $RS -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
B="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=1000 rho=prop21 rho_D=$D sampler=mh cluster=plant base_seed=20260830 drop_rows=12,7,5 ind_rows=median"
/usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=10 \
  maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=10 output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
echo "done $T $(date +%H:%M)"; echo "all done"
