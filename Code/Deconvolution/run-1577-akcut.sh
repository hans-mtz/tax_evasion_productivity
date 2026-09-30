#!/bin/bash
# AK2020 truncation rule vs ours (Hans, 2026-09-29): cut=ak = AK's objMCcu (Omega/n, keep eigenvalues > 0, dropped
# rows removed before the eigendecomposition). Same EPSVAR system, starts, seeds and settings as run-1574; three points:
# min (k=1 gm1), max (k=0.5 g0), random (k=0.5 gm1, set.seed(20260929) among the other eight).
# Per point: adiag under cut=ak at the OLD 1574 estimates (no refit), then refit from the 1574 start, then adiag at the fit.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv; B=./grid_estimator_eps_ak
D=$(Rscript -e "r <- read.csv('$P/1569-drop-6.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
getpar() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null; }
run() { local K=$1 G=$2 G13=0; [ "$G" = "gm1" ] && G13=-1
  local X0="$D,$K,0.2,0,0,0,0,0,0,0,0,0,0,0,0,$G13"
  local C="cut=ak qform=power_kink k_fixed=$K row6=eps_psi drop_rows=6 input_csv=$IN n_burn=1000 n_keep=3000"
  $B mode=adiag $C par=$(getpar $P/1574-kgrid-k$K-$G.csv) n_threads=4 base_seed=20260829 output_csv=/dev/null \
    > $P/1577-akcut-k$K-$G-adiag-old.txt 2>&1
  $B mode=lambdagrid $C lambdas=0.556 x0=$X0 algo=neldermead n_threads=4 maxtime=3600 base_seed=20260829 \
    output_csv=$P/1577-akcut-k$K-$G.csv > $P/1577-akcut-k$K-$G.Rout 2>&1
  $B mode=adiag $C par=$(getpar $P/1577-akcut-k$K-$G.csv) n_threads=4 base_seed=20260829 output_csv=/dev/null \
    > $P/1577-akcut-k$K-$G-adiag.txt 2>&1
  echo "done k=$K $G"; }
run 1 gm1 & run 0.5 g0 & run 0.5 gm1 &
wait; echo "all done"
