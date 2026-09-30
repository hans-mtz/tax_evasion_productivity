#!/bin/bash
# Noise under the AK2020 cut (Hans, 2026-09-30; every earlier seed-spread number was under the old relative cut).
# Three 1577 fits (k=1 gm1, k=0.5 g0, k=0.5 gm1). (a) MC noise at FIXED (theta, gamma): adiag at seeds 30, 31, 32
# (1577 used 29). (b) Refit noise: refit from the 1577 start with seeds 30 and 31, then adiag at the fit (own seed).
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv; B=./grid_estimator_eps_ak
D=$(Rscript -e "r <- read.csv('$P/1569-drop-6.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
C0() { echo "cut=ak qform=power_kink k_fixed=$1 row6=eps_psi drop_rows=6 input_csv=$IN n_burn=1000 n_keep=3000"; }
point() { local K=$1 G=$2 G13=0; [ "$G" = "gm1" ] && G13=-1; local C=$(C0 $K)
  local PAR=$(getpar $P/1577-akcut-k$K-$G.csv)
  for S in 20260830 20260831 20260832; do
    $B mode=adiag $C par=$PAR n_threads=4 base_seed=$S output_csv=/dev/null > $P/1578-noise-k$K-$G-fixed-s$S.txt 2>&1
  done
  local X0="$D,$K,0.2,0,0,0,0,0,0,0,0,0,0,0,0,$G13"
  for S in 20260830 20260831; do
    $B mode=lambdagrid $C lambdas=0.556 x0=$X0 algo=neldermead n_threads=4 maxtime=3600 base_seed=$S \
      output_csv=$P/1578-noise-k$K-$G-refit-s$S.csv > $P/1578-noise-k$K-$G-refit-s$S.Rout 2>&1
    $B mode=adiag $C par=$(getpar $P/1578-noise-k$K-$G-refit-s$S.csv) n_threads=4 base_seed=$S output_csv=/dev/null \
      > $P/1578-noise-k$K-$G-refit-s$S-adiag.txt 2>&1
  done
  echo "done k=$K $G"; }
point 1 gm1 & point 0.5 g0 & point 0.5 gm1 &
wait; echo "all done"
