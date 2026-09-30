#!/bin/bash
# Coarse k grid on the eps-variance system (Hans, 2026-09-29): EPSVAR build, rows = KINK_S set minus row 6 plus row 12
# (eps^2 - sig2eps_j), kappa FIXED at 0.556, s ESTIMATED (start 0.2), k in {0.2, 0.3, 0.5, 0.75, 1}. theta (delta0-2)
# started from the drop-6 fit; two gamma starts per k: all zeros ("g0"), and zeros except gamma13 (row 12) = -1 ("gm1").
# n_keep=3000, two NM passes, 4 at a time x 3 threads; then adiag at the fit seed (tilted var(eps), row t's).
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv
D=$(Rscript -e "r <- read.csv('$P/1569-drop-6.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')])), sep=',')" 2>/dev/null)
run() { local K=$1 G=$2 G13=0; [ "$G" = "gm1" ] && G13=-1
  local X0="$D,$K,0.2,0,0,0,0,0,0,0,0,0,0,0,0,$G13"
  local C="qform=power_kink k_fixed=$K row6=eps_psi drop_rows=6 input_csv=$IN n_burn=1000 n_keep=3000"
  ./grid_estimator_eps mode=lambdagrid $C lambdas=0.556 x0=$X0 algo=neldermead n_threads=3 maxtime=3600 base_seed=20260829 \
    output_csv=$P/1574-kgrid-k$K-$G.csv > $P/1574-kgrid-k$K-$G.Rout 2>&1
  local PAR=$(Rscript -e "r <- read.csv('$P/1574-kgrid-k$K-$G.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null)
  ./grid_estimator_eps mode=adiag $C par=$PAR n_threads=3 base_seed=20260829 output_csv=/dev/null > $P/1574-kgrid-k$K-$G-adiag.txt 2>&1
  echo "done k=$K $G"; }
n=0
for K in 0.2 0.3 0.5 0.75 1; do for G in g0 gm1; do
  run $K $G & n=$((n+1)); if [ $((n % 4)) -eq 0 ]; then wait; fi
done; done
wait; echo "all done"
