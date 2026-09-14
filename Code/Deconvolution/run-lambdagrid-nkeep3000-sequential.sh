#!/bin/bash
# lambda-grid rerun, n_burn=1000/n_keep=3000, INDEPENDENT same-point seeding
# from lag_m's own best fit (no chaining across lambda), one lambda per
# invocation to match the (delta1,delta2) grid's methodology exactly
# (2026-09-09). Sequential, not concurrent: the R driver auto-generates the
# SAME output filename regardless of which lambda is passed (the tag doesn't
# include lambda), so concurrent calls would race and overwrite each other --
# rename the output immediately after each call instead. n_cores=4 per call
# (the delta1,delta2 grid rerun is using the other 8 cores concurrently).
set -euo pipefail
cd "$(dirname "$0")/../.."

PAR="3.2411576554474,4.1538810604154,0.508582490563503,0.00064447638164912,0.000266931500722254,-0.00320709266345527,0.0173026816261523,0.0671075607612255,0.0361581141572298,-0.331783440956923,8.08433700184273e-06,3.21236234943358e-11,0.0933072166415534"
LAMBDAS="1e-9 2.15e-9 4.64e-9 1e-8 2.15e-8 4.64e-8 1e-7 2.15e-7 4.64e-7 1e-6 2.15e-6 4.64e-6 1e-5 2.15e-5 4.64e-5 1e-4"
AUTO_FILE="Code/Products/1211-stage2-elvis-AB-lag_m-A-include_zero-nburn1000-nkeep3000-maxevalb2000-trim0.005.RData"

for lam in $LAMBDAS; do
  echo "=== lambda=${lam} ==="
  Rscript Code/Deconvolution/1211-stage2-elvis-driver-AB.R \
    ins=lag_m moment_set=A corner_mode=include_zero n_burn=1000 n_keep=3000 \
    lambda_grid="${lam}" trim_top_pct=0.005 par_init="${PAR}" \
    maxeval_b=2000 xtol_rel_b=1e-4 maxtime_b=1800 n_cores=4 refine=FALSE \
    > "Code/Deconvolution/1251-lambdagrid-lag_m-nkeep3000-lam${lam}.Rout" 2>&1
  mv "${AUTO_FILE}" "Code/Products/1251-lambdagrid-lag_m-nkeep3000-lam${lam}.RData"
  echo "  -> saved Code/Products/1251-lambdagrid-lag_m-nkeep3000-lam${lam}.RData"
done
echo "LAMBDA GRID RERUN FINISHED"
