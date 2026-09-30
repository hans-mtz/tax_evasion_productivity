#!/bin/bash
# k grid on today's system, everything else profiled (Hans, 2026-09-30; "start over"): AK2020 cut, EPSVAR 13 rows with
# row 6 dropped, kinked power q; kappa ESTIMATED (grid_estimator_kf, bounds [0.02, 5]), s estimated, delta, gamma free.
# k in {0.25, 0.5, 0.75, 1, 1.5, 2}. One shared start for every k (no chaining): delta from the 1577 k=1 AK-cut fit,
# kappa 0.556, s 0.2, gamma zero except gamma13 = -1. n_keep=3000, NM two passes, 3 at a time x 4 threads; adiag per fit.
# Waits for the 1578 noise run to finish first.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv; B=./grid_estimator_kf
while ! grep -q "all done" $P/1578-launch.log; do sleep 60; done
D=$(Rscript -e "r <- read.csv('$P/1577-akcut-k1-gm1.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
run() { local K=$1
  local X0="$D,$K,0.2,0.556,0,0,0,0,0,0,0,0,0,0,0,0,-1"
  local C="cut=ak qform=power_kink k_fixed=$K row6=eps_psi drop_rows=6 input_csv=$IN n_burn=1000 n_keep=3000"
  $B mode=lambdagrid $C lambdas=0.556 x0=$X0 algo=neldermead n_threads=4 maxtime=3600 base_seed=20260829 \
    output_csv=$P/1579-kgrid-kf-k$K.csv > $P/1579-kgrid-kf-k$K.Rout 2>&1
  $B mode=adiag $C par=$(getpar $P/1579-kgrid-kf-k$K.csv) n_threads=4 base_seed=20260829 output_csv=/dev/null \
    > $P/1579-kgrid-kf-k$K-adiag.txt 2>&1
  echo "done k=$K"; }
n=0
for K in 0.25 0.5 0.75 1 1.5 2; do
  run $K & n=$((n+1)); if [ $((n % 3)) -eq 0 ]; then wait; fi
done
wait; echo "all done"
