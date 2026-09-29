#!/bin/bash
# After the refined kink grid (1559): (A) is k bounded by concavity? best cell kappa=0.5, s=0.2 with k up to 2 (convex
# allowed; SOC holds for k<3), starts k=0.2, 0.95 and 1.5; (B) dense grid around the best cell, kappa in {0.4,0.5,0.6} x
# s in {0.15,0.2,0.25}, k up to 2, starts 0.2, 0.9 and 1.3 (low start added: local-optimum check). Seeded as below.
# delta/gamma from the lowest-TS fit among 1558/1559. grid_estimator_kink2 (k_max option). 30 single-point processes, 4 at a time x 3 threads, maxtime=3600 s per pass.
# Waits for the 1559 driver (PID $1) to finish.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
[ -n "${1:-}" ] && while kill -0 "$1" 2>/dev/null; do sleep 30; done
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
BEST=$(Rscript -e "f <- c(list.files('$P', '^1558-kink-.*[.]csv$', full.names=TRUE), list.files('$P', '^1559-kink-.*[.]csv$', full.names=TRUE));
  f <- f[file.size(f) > 50]; ts <- sapply(f, function(x) { r <- read.csv(x); 2*r\$n*r\$Lhat }); cat(f[which.min(ts)])" 2>/dev/null)
echo "seed file: $BEST"
seed() { Rscript -e "r <- read.csv('$BEST'); cat(sprintf('%.15g', c(unlist(r[1,c('delta0_hat','delta1_hat','delta2_hat')]), $1, unlist(r[1,paste0('gamma',1:11)]))), sep=',')" 2>/dev/null; }
run() { local X; X=$(seed $3)
  ./grid_estimator_kink2 mode=lambdagrid qform=power_kink kink_share=$2 k_max=2 row6=eps_psi input_csv=$IN lambdas=$1 x0=$X \
    algo=neldermead n_burn=1000 n_keep=3000 n_threads=3 maxtime=3600 base_seed=20260829 \
    output_csv=$P/1560-kink-k$1-s$2-k0$3.csv > $P/1560-kink-k$1-s$2-k0$3.Rout 2>&1; echo "done kappa=$1 s=$2 kstart=$3"; }
JOBS="0.5:0.2:0.2 0.5:0.2:0.95 0.5:0.2:1.5"
for K in 0.4 0.5 0.6; do for S in 0.15 0.2 0.25; do for K0 in 0.2 0.9 1.3; do JOBS="$JOBS $K:$S:$K0"; done; done; done
n=0
for J in $JOBS; do IFS=: read K S K0 <<< "$J"; run $K $S $K0 & n=$((n+1)); if [ $((n % 4)) -eq 0 ]; then wait; fi; done
wait; echo "all done"
