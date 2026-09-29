#!/bin/bash
# S3 variants with ONE global detection scale s for every firm (no industry-year Mbar), Hans 2026-09-28:
#   q = lambda1*(1-exp(-e/s)), support e < s. Design A input, moment set A, grid_estimator_s3 qform=exp_scale;
#   the estimator reads the scale from the Mbar column, so each variant is the design-A input with Mbar := s.
#   s0 = 110,469 = mean reported M* over all firms (corporations included), all industries and years (1532).
#   (i)  level fixed lambda1 = 0.5, scale grid s = s0 x {0.03,0.1,0.3,1,3,10};
#   (ii) scale fixed s = s0, level grid lambda1 in {0.1,0.3,0.5,0.7,0.9}.
# Every point seeded from the polished center (1544-s3-center2-designA.csv); 11 single-point jobs, 3 concurrent x
# 4 threads; maxtime=1800 s per pass.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products
S0=110469
X0=$(Rscript -e "r <- read.csv('$P/1544-s3-center2-designA.csv'); cat(sprintf('%.15g', unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat',paste0('gamma',1:9))])), sep=',')" 2>/dev/null)
echo "seed = $X0"

# inputs: design A with a constant scale
Rscript -e "d <- read.csv('$P/1532-stage2-input-designA-trim0.005.csv');
  for (m in c(0.03,0.1,0.3,1,3,10)) { d\$Mbar <- $S0*m; write.csv(d, sprintf('$P/1545-input-designA-s%g.csv', m), row.names=FALSE, quote=FALSE) }" 2>/dev/null

JOBS=$(mktemp)
for m in 0.03 0.1 0.3 1 3 10; do echo "$m 0.5 i"; done >> $JOBS          # (i)  grid the scale, lambda1 = 0.5
for l in 0.1 0.3 0.5 0.7 0.9; do echo "1 $l ii"; done >> $JOBS            # (ii) s = s0, grid lambda1
cat $JOBS | xargs -P 3 -L 1 bash -c '
  m=$0; l=$1; v=$2
  ./grid_estimator_s3 mode=lambdagrid qform=exp_scale input_csv='"$P"'/1545-input-designA-s$m.csv lambdas=$l \
    x0='"$X0"' algo=neldermead n_burn=1000 n_keep=3000 n_threads=4 maxtime=1800 base_seed=20260829 \
    output_csv='"$P"'/1545-$v-s$m-l$l.csv > '"$P"'/1545-$v-s$m-l$l.Rout 2>&1
  echo "done $v s=$m lambda1=$l"'
rm -f $JOBS
echo "all done"
