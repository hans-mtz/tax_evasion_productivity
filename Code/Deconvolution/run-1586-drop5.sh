#!/bin/bash
# Drop row 5 (eps*lnM) (Hans, 2026-09-30): interior-only input (n = 12,050), rows 5, 6, 7, 12 dropped (9 rows, chi2_9 crit 16.9);
# otherwise as run-1585: AK cut, kinked power q, kappa and s estimated, n_keep=1000, n_burn=1000, three NM passes, shared
# start (delta from 1579 k=1, kappa 0.556, s 0.2, gamma 0). k in {0.5, 0.75} x seeds {30, 31}, paired with 1585.
# adiag at own seed; re-scored at seeds 40, 41 (n_keep 3000).
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1585-stage2-input-designA-interior-trim0.005.csv; B=./grid_estimator_kf3
D=$(Rscript -e "r <- read.csv('$P/1579-kgrid-kf-k1.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
run() { local K=$1 S=$2 T=1586-drop5-k$1-s$2
  local C="cut=ak qform=power_kink k_fixed=$K row6=eps_psi drop_rows=5,6,7,12 input_csv=$IN n_burn=1000"
  $B mode=lambdagrid $C n_keep=1000 lambdas=0.556 x0="$D,$K,0.2,0.556,0,0,0,0,0,0,0,0,0,0,0,0,0" algo=neldermead n_passes=3 \
    n_threads=3 maxtime=3600 base_seed=$S output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  local PAR=$(getpar $P/$T.csv)
  $B mode=adiag $C n_keep=1000 par=$PAR n_threads=3 base_seed=$S output_csv=/dev/null > $P/$T-adiag.txt 2>&1
  for E in 20260840 20260841; do
    $B mode=adiag $C n_keep=3000 par=$PAR n_threads=3 base_seed=$E output_csv=/dev/null > $P/$T-eval-e$E.txt 2>&1
  done
  echo "done k=$K s=$S"; }
for K in 0.5 0.75; do for S in 20260830 20260831; do run $K $S & done; done
wait; echo "all done"
