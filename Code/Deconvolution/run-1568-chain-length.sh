#!/bin/bash
# Chain-length test (Hans, 2026-09-29): s FIXED at 0.20, k = 0.3, kappa = 0.556 (best point so far), KINK_S rows (12),
# n_keep in {3000, 10000, 30000}, n_burn = 1000 throughout (one change at a time). Nelder-Mead, two passes, from the
# polished fit 1566-polish-kappa0.556 with s set to 0.20. After each fit: adiag at base_seed 20260830 and 20260831
# (parameters and gamma held at that fit) to measure how the seed spread shrinks with chain length.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
X0=$(Rscript -e "r <- read.csv('$P/1566-polish-kappa0.556.csv'); v <- unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))]); v[5] <- 0.2; cat(sprintf('%.15g', v), sep=',')" 2>/dev/null)
echo "seed = $X0"
run() { ./grid_estimator_kinks2 mode=lambdagrid qform=power_kink k_fixed=0.3 s_fixed=0.2 row6=eps_psi input_csv=$IN lambdas=0.556 x0=$X0 \
          algo=neldermead n_burn=1000 n_keep=$1 n_threads=4 maxtime=14400 base_seed=20260829 \
          output_csv=$P/1568-chain-nkeep$1.csv > $P/1568-chain-nkeep$1.Rout 2>&1
        PAR=$(Rscript -e "r <- read.csv('$P/1568-chain-nkeep$1.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))])), sep=',')" 2>/dev/null)
        for SEED in 20260829 20260830 20260831; do
          TS=$(./grid_estimator_kinks2 mode=adiag qform=power_kink k_fixed=0.3 s_fixed=0.2 row6=eps_psi input_csv=$IN par=$PAR n_burn=1000 n_keep=$1 n_threads=4 base_seed=$SEED output_csv=/dev/null 2>&1 | grep "Lhat (recomputed)" | sed 's/.*TS = 2 n Lhat = \([0-9.]*\).*/\1/')
          echo "n_keep=$1 seed=$SEED TS=$TS" | tee -a $P/1568-chain-seed-spread.txt
        done; echo "done n_keep=$1"; }
run 3000 & run 10000 & run 30000 &
wait; echo "all done"
