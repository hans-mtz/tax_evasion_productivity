#!/bin/bash
# eps-variance row (Hans, 2026-09-29): EPSVAR build, rows = 12-row KINK_S set minus row 6, plus row 12 = eps^2 - sig2eps_j
# (corporate first-stage variance by industry; corner and interior firms). Best point k=0.3, kappa=0.556, s=0.20 fixed,
# n_keep=3000, two NM passes from the drop-6 fit (gamma13 = 0). Then adiag at the fit seed and seeds 30/31.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv
C="qform=power_kink k_fixed=0.3 s_fixed=0.2 row6=eps_psi drop_rows=6 input_csv=$IN n_burn=1000 n_keep=3000"
X0=$(Rscript -e "r <- read.csv('$P/1569-drop-6.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))]), 0)), sep=',')" 2>/dev/null)
./grid_estimator_eps mode=lambdagrid $C lambdas=0.556 x0=$X0 algo=neldermead n_threads=12 maxtime=3600 base_seed=20260829 \
  output_csv=$P/1573-epsvar.csv > $P/1573-epsvar.Rout 2>&1
PAR=$(Rscript -e "r <- read.csv('$P/1573-epsvar.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null)
for SEED in 20260829 20260830 20260831; do
  echo "===== seed $SEED"
  ./grid_estimator_eps mode=adiag $C par=$PAR n_threads=12 base_seed=$SEED output_csv=/dev/null 2>&1 | sed -n '/Lhat (recomputed)/,$p'
done > $P/1573-epsvar-adiag.txt
echo "all done"
