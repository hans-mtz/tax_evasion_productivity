#!/bin/bash
# Robustness check (2026-09-14): does the tight CV-adjusted hard-CI at
# Delta=-0.04 (fixed-theta run, Code/Products/1300-cv-16delta-final.csv)
# survive freeing theta=(delta0,lambda,delta1,delta2), seeded (not fixed) at
# the exact same operating point and gamma anchor used in the fixed-theta
# exercise? Tests the same 5 R candidates already tested there (same
# precision, pulled directly from 1300). Uses the newly-added row9_mode=cv
# support in revgrid_indep mode (grid_estimator.cpp, patched today -- this
# mode previously only supported the raw R moment).
set -e

ROOT="/Volumes/SSD Hans/Github/Tax_Evasion_Productivity"
PRODUCTS="$ROOT/Code/Products"
BIN="$ROOT/Code/C-estimator/grid_estimator"
INPUT="$PRODUCTS/1260-stage2-revenue-input-lag_m-trim0.005.csv"
OUT="$PRODUCTS/1302-revgrid-indep-cv-check-deltam0_04.csv"

X0="3.46356,5.427e-07,4.3,0.54,0.0149401262989576,-0.0240124349620528,0.251633918064792,-0.261339700381921,0.05385831520302,-1.75944434105163,3.04708748407887e-05,4.02410443759356e-08,9.0848977003403,-3.44834396328732e-06"
DELTAS="-0.04"
RVALS="1686.81134931443,1718.60868476472,1768.22438289362,1793.58184097988,1817.04020402562"

N_THREADS=12
N_BURN=1000
N_KEEP=3000
MAXTIME=1800
BASE_SEED=20260829
CV_BETA=0.998838
CV_MU_C=2507.602596

echo "==== revgrid_indep CV check: Delta=-0.04, 5 R values, theta FREE (seeded at fixed-theta point) ===="
"$BIN" mode=revgrid_indep input_csv="$INPUT" output_csv="$OUT" \
    x0=$X0 deltas=$DELTAS rvals="$RVALS" \
    n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
    row9_mode=cv cv_beta=$CV_BETA cv_mu_c=$CV_MU_C

echo "==== DONE: $OUT ===="
