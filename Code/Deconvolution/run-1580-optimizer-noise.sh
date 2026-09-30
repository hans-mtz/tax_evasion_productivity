#!/bin/bash
# B) optimizer vs noise (Hans, 2026-09-30). One point: k=0.5, start g0, same system as 1577/1578 (AK cut, EPSVAR,
# row 6 dropped, kappa fixed 0.556, s free, n_keep=3000). Seeds 29/30/31, compared with the baseline (2 NM passes:
# 1577 s29, 1578 s30/s31).
#  B1 = 3rd NM pass: restart from each baseline fit with its own seed (NM two passes -> passes 3 and 4; Lhat_pass1 = pass 3).
#  B2 = NM then BOBYQA (algo2=bobyqa), from the shared start.
#  B3 = NM, simulated annealing 1200 s, NM polish (sa_time=1200), from the shared start.
# Then every fit (baseline + B1-B3) re-evaluated at common seeds 40 and 41 with n_keep=10000 (adiag, no refit).
# Waits for 1579 (k grid) to finish first.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv; B=./grid_estimator_eps_ak2
while ! grep -q "all done" $P/1579-launch.log; do sleep 60; done
K=0.5
C="cut=ak qform=power_kink k_fixed=$K row6=eps_psi drop_rows=6 input_csv=$IN n_burn=1000"
D=$(Rscript -e "r <- read.csv('$P/1569-drop-6.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
X0="$D,$K,0.2,0,0,0,0,0,0,0,0,0,0,0,0,0"
getx() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
base() { case $1 in 20260829) echo $P/1577-akcut-k0.5-g0.csv;; *) echo $P/1578-noise-k0.5-g0-refit-s$1.csv;; esac; }
fit() { local TAG=$1 S=$2 X=$3; shift 3
  $B mode=lambdagrid $C n_keep=3000 lambdas=0.556 x0=$X algo=neldermead n_threads=4 maxtime=3600 base_seed=$S "$@" \
    output_csv=$P/1580-$TAG-s$S.csv > $P/1580-$TAG-s$S.Rout 2>&1; }
SEEDS="20260829 20260830 20260831"
for S in $SEEDS; do fit B1-nm3 $S $(getx $(base $S)) & done; wait; echo "done B1"
for S in $SEEDS; do fit B2-nm-bobyqa $S $X0 algo2=bobyqa & done; wait; echo "done B2"
for S in $SEEDS; do fit B3-nm-sa-nm $S $X0 sa_time=1200 & done; wait; echo "done B3"
# common-seed evaluation, 3 at a time
n=0
for S in $SEEDS; do for F in $(base $S) $P/1580-B1-nm3-s$S.csv $P/1580-B2-nm-bobyqa-s$S.csv $P/1580-B3-nm-sa-nm-s$S.csv; do
  for E in 20260840 20260841; do
    ( $B mode=adiag $C n_keep=10000 par=$(getpar $F) n_threads=4 base_seed=$E output_csv=/dev/null \
        > $P/1580-eval-$(basename $F .csv)-e$E.txt 2>&1 ) &
    n=$((n+1)); if [ $((n % 3)) -eq 0 ]; then wait; fi
  done; done; done
wait; echo "all done"
