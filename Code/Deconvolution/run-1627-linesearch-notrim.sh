#!/bin/bash
# Manual line search on the untrimmed sample (Hans, 2026-10-02), from the center (k, kappa) = (0.75, 0.5) (1625, TS 46.9).
# Usage: run-1627-linesearch-notrim.sh "<k> <kappa>" ["<k> <kappa>" ...]; all given points run in parallel, threads
# split evenly over 12. Each fit: design i on 1624 (no trim), k and kappa pinned, delta's started from the 1625 center fit
# (same start for every point), gamma_init=solve, D 1599-rhoD-ind, IS + mix, plant clusters, seed 30, joint NM 2 passes,
# maxeval 800 x 21, delta box 100; adiag at R = 1000 and 4000. One line "done <tag> TS <TS>" per finished point.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1624-stage2-input-designA-interior-plant-k-notrim.csv; DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
NT=$(( 12 / $# )); [ $NT -lt 1 ] && NT=1
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local K=$1 KA=$2 T=1627-i-notrim-k$1-kappa$2
  local X0=$(Rscript -e "r <- read.csv('$P/1625-i-notrim-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, $KA, rep(0,22))), sep=',')" 2>/dev/null)
  local B="qform=power_nokink k_fixed=$K row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
  ./grid_estimator_ind5b mode=lambdagrid $B kappa_fixed=$KA n_keep=1000 lambdas=$KA x0=$X0 algo=neldermead n_passes=2 n_threads=$NT \
    gamma_init=solve maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=$NT output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T TS $(grep -o 'TS = 2 n Lhat = [0-9.]*' $P/$T-adiag-R1000.txt | awk '{print $NF}') $(date +%H:%M)"; }
for pt in "$@"; do fit $pt & done; wait; echo "all done"
