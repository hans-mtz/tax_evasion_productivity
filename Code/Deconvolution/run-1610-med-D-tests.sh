#!/bin/bash
# D tests on the medians design (Hans, 2026-10-01), k = 0.7, one fit each. Same as 1605 (input 1604 with the stage-2
# deconvolution medians, rows 0-4, 6, 8, 9, 11 + 9 median rows, interior, IS + mix, rho=prop21, plant clusters, seed 30,
# start theta from the 1594 NM point with k = 0.7, gamma 0, no chaining); joint NM 2 passes, maxeval 800 x 21, delta box
# 100, kappa bound 20. Only D differs:
#   M0 ones       : D = 1 on every row (benchmark)
#   M1 current    : 1604-rhoD-med (pooled sd under the uniform proposal)
#   M2 boundedout : as M1 but median rows 13-21 = inf (left out of the rho penalty; bounded rows cannot break properness)
#   M3 within     : as M1 but median rows 13-21 = within-industry sd (exact: E_j[g^2] = 1/4 for a +-1/2 indicator)
# Usage: run-1610-med-D-tests.sh <tag> <Dfile> <threads> [suffix]. adiag at R = 1000 and 4000.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
TAG=$1; DF=$2; NT=$3; SUF=${4:-}
P=../Products; ST=$P/1594-nm-2pass.csv; RS=$(command -v Rscript || echo /usr/local/bin/Rscript); T=1610-med-k0.7-$TAG$SUF
IN=$P/1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv; D=$(sed -n 's/rho_D=//p' $P/$DF)
KAP=$($RS -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
X0=$($RS -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.7, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
getp() { $RS -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
B="qform=power_nokink k_fixed=0.7 row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$D sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=12,7,5 ind_rows=median"
/usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=$NT \
  maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=$NT output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
echo "done $T $(date +%H:%M)"
