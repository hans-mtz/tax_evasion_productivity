#!/bin/bash
# Noise checks after the fine kappa grid (Hans, 2026-09-29). Points: the kappa with the lowest TS among the 11 k=0.3
# fits (1563 at 0.4/0.5/0.6 + 1565's 8) and its two neighbours in kappa. At each point:
#  (1) Monte Carlo noise: adiag at base_seed 20260830 and 20260831 with all parameters and gamma held at the fit;
#  (2) convergence: a third Nelder-Mead pass (two more passes) from the fit's own endpoint, same seed;
#  (3) optimizer noise: refit from the delta/gamma of the k=1 best fit (1563 k=1 kappa=0.4), k=0.3 fixed, s start 0.22.
# Decision rule (fixed in advance): a TS difference between kappas counts only if it exceeds the larger of the (1) band
# and the (3) restart spread; each point's TS is the best across passes/starts. Waits for PID $1 (the 1565 driver).
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
[ -n "${1:-}" ] && while kill -0 "$1" 2>/dev/null; do sleep 30; done
P=../Products; IN=$P/1546-stage2-input-designA-tau-trim0.005.csv
PTS=$(Rscript -e "f <- c(sprintf('$P/1563-kinks-k0.3-kappa%s.csv', c(0.4,0.5,0.6)), Sys.glob('$P/1565-kappa-fine-k0.3-kappa*.csv')); f <- f[file.size(f) > 50]
  d <- do.call(rbind, lapply(f, function(x) { r <- read.csv(x); data.frame(f=x, kappa=r\$lambda, TS=2*r\$n*r\$Lhat) })); d <- d[order(d\$kappa),]
  i <- which.min(d\$TS); j <- if (i == 1) 1:3 else if (i == nrow(d)) (nrow(d)-2):nrow(d) else (i-1):(i+1); cat(d\$f[j])" 2>/dev/null)
echo "points: $PTS"
par()  { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))])), sep=',')" 2>/dev/null; }
x0()   { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))])), sep=',')" 2>/dev/null; }
XALT=$(Rscript -e "r <- read.csv('$P/1563-kinks-k1-kappa0.4.csv'); v <- unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat', paste0('gamma',1:12))]); v[4] <- 0.3; v[5] <- 0.22; cat(sprintf('%.15g', v), sep=',')" 2>/dev/null)
# (1) Monte Carlo noise at fixed parameters
for F in $PTS; do
  PAR=$(par $F); KA=$(Rscript -e "cat(read.csv('$F')\$lambda)" 2>/dev/null)
  for SEED in 20260829 20260830 20260831; do
    TS=$(./grid_estimator_kinks2 mode=adiag qform=power_kink k_fixed=0.3 row6=eps_psi input_csv=$IN par=$PAR n_burn=1000 n_keep=3000 n_threads=12 base_seed=$SEED output_csv=/dev/null 2>&1 | grep "Lhat (recomputed)" | sed 's/.*TS = 2 n Lhat = \([0-9.]*\).*/\1/')
    echo "MC kappa=$KA seed=$SEED TS=$TS" | tee -a $P/1566-mc-noise.txt
  done
done
# (2) polish from own endpoint and (3) alternative start: 6 single-point fits, 3 at a time x 4 threads
run() { ./grid_estimator_kinks2 mode=lambdagrid qform=power_kink k_fixed=0.3 kink_share=0.22 row6=eps_psi input_csv=$IN lambdas=$1 x0=$2 \
          algo=neldermead n_burn=1000 n_keep=3000 n_threads=4 maxtime=3600 base_seed=20260829 \
          output_csv=$P/1566-$3-kappa$1.csv > $P/1566-$3-kappa$1.Rout 2>&1; echo "done $3 kappa=$1"; }
n=0
for F in $PTS; do KA=$(Rscript -e "cat(read.csv('$F')\$lambda)" 2>/dev/null)
  run $KA "$(x0 $F)" polish & n=$((n+1)); if [ $((n % 3)) -eq 0 ]; then wait; fi
  run $KA "$XALT" altstart & n=$((n+1)); if [ $((n % 3)) -eq 0 ]; then wait; fi
done
wait; echo "all done"
