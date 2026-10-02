#!/bin/bash
# A (reduced, Hans 2026-09-30): one n_keep=10000 fit (seed 31), same point/system/start as run-1580/1581, 12 threads;
# then common-seed re-evaluation (seeds 40, 41, n_keep 10000) of it and of the two MacBook n_keep=1000 fits.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv; B=./grid_estimator_eps_ak2
K=0.5; C="cut=ak qform=power_kink k_fixed=$K row6=eps_psi drop_rows=6 input_csv=$IN n_burn=1000"
D=$(Rscript -e "r <- read.csv('$P/1569-drop-6.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
X0="$D,$K,0.2,0,0,0,0,0,0,0,0,0,0,0,0,0"
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
$B mode=lambdagrid $C n_keep=10000 lambdas=0.556 x0=$X0 algo=neldermead n_threads=12 maxtime=10800 base_seed=20260831 \
  output_csv=$P/1582-A-nk10000-s20260831.csv > $P/1582-A-nk10000-s20260831.Rout 2>&1
echo "done fit"
for F in 1582-A-nk10000-s20260831 1581-A-nk1000-s20260830-macbook 1581-A-nk1000-s20260831-macbook; do for E in 20260840 20260841; do
  $B mode=adiag $C n_keep=10000 par=$(getpar $P/$F.csv) n_threads=12 base_seed=$E output_csv=/dev/null > $P/1582-eval-$F-e$E.txt 2>&1
done; done
echo "all done"
