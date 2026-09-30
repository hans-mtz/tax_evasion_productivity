#!/bin/bash
# A) chain length vs noise (Hans, 2026-09-30). Same point/system/start as run-1580 (k=0.5 g0, AK cut, EPSVAR, row 6
# dropped, kappa fixed 0.556, s free, 2 NM passes), n_burn=1000; n_keep in {1000, 10000} (3000 = the 1577/1578
# baseline); seeds 29/30/31. Then each fit re-evaluated at common seeds 40, 41 with n_keep=10000 (as run-1580).
# Machine-agnostic: BIN (default ./grid_estimator_eps_ak2) and TAG suffix (e.g. -macbook) via env.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv; B=${BIN:-./grid_estimator_eps_ak2}; SFX=${TAG:-}
RS=${RSCRIPT:-Rscript}
K=0.5
C="cut=ak qform=power_kink k_fixed=$K row6=eps_psi drop_rows=6 input_csv=$IN n_burn=1000"
D=$($RS -e "r <- read.csv('$P/1569-drop-6.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
X0="$D,$K,0.2,0,0,0,0,0,0,0,0,0,0,0,0,0"
getpar() { $RS -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
SEEDS="20260829 20260830 20260831"
for NK in 1000 10000; do
  MT=3600; [ $NK -eq 10000 ] && MT=10800
  for S in $SEEDS; do
    $B mode=lambdagrid $C n_keep=$NK lambdas=0.556 x0=$X0 algo=neldermead n_threads=4 maxtime=$MT base_seed=$S \
      output_csv=$P/1581-A-nk$NK-s$S$SFX.csv > $P/1581-A-nk$NK-s$S$SFX.Rout 2>&1 &
  done; wait; echo "done nk=$NK"
done
n=0
for NK in 1000 10000; do for S in $SEEDS; do for E in 20260840 20260841; do
  F=$P/1581-A-nk$NK-s$S$SFX.csv
  ( $B mode=adiag $C n_keep=10000 par=$(getpar $F) n_threads=4 base_seed=$E output_csv=/dev/null \
      > $P/1581-eval-A-nk$NK-s$S$SFX-e$E.txt 2>&1 ) &
  n=$((n+1)); if [ $((n % 3)) -eq 0 ]; then wait; fi
done; done; done
wait; echo "all done"
