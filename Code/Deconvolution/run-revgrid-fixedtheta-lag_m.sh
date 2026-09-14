#!/bin/bash
# Fixed-theta (Delta,R) test-inversion grid for lag_m, no-eta mechanism,
# theta_smooth held COMPLETELY FIXED at the 3D cube's global minimum (first
# point in this project to clear even the hard test): (delta0,lambda,
# delta1,delta2)=(3.46356,5.427e-07,4.3,0.54). Only gamma (all 10) is ever
# optimized -- per AK2020's own counterfactual practice (checked directly
# in their code, 2026-09-11), fixing the structural/support parameter and
# profiling only gamma, after the first attempt (theta merely SEEDED there,
# left free) produced a vacuous all-pass result.
# Stage 1: one anchor cell (Delta=0, R=baseline) refines gamma10 (the new
#   revenue moment's own multiplier, no prior estimate) from a random small
#   guess; gamma1-9 already come from the cube's own converged fit at this
#   exact theta.
# Stage 2: broadcast the anchor's full converged gamma (all 10, identical)
#   to every other cell -- no chaining, same-point independent seeding.
# 2026-09-12.
set -e
cd "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Code/C-estimator"

INPUT=../Products/1260-stage2-revenue-input-lag_m-trim0.005.csv
OUTDIR=../Products
N_THREADS=12
N_BURN=1000
N_KEEP=3000
MAXTIME=1800
BASE_SEED=20260829

THETA="3.46356,5.427e-07,4.3,0.54"
GAMMA_FROM_CUBE="0.0143883116027861,-0.0119013123755461,0.0630942173107609,-0.0783100943504263,0.0578349092582138,-0.907539328124065,1.27340923253472e-05,7.34383264616891e-09,2.64362954929756"

echo "==== STAGE 1: anchor cell (Delta=0, R=1701.932208, gamma10=1e-5 random guess) ===="
GAMMA0_ANCHOR="${GAMMA_FROM_CUBE},1e-5"
ANCHOR_CSV="$OUTDIR/1290-revgrid-fixedtheta-anchor-lag_m.csv"
./grid_estimator mode=revgrid_fixedtheta input_csv=$INPUT output_csv=$ANCHOR_CSV \
    theta=$THETA gamma0=$GAMMA0_ANCHOR deltas=0 rvals=1701.932208 \
    n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED

echo "==== Parsing anchor result into Stage 2 gamma seed ===="
# Columns: Delta,R,delta0,lambda,delta1,delta2,gamma1..10,Lhat,...
GAMMA0_STAGE2=$(tail -1 "$ANCHOR_CSV" | cut -d',' -f7-16)
echo "Stage 2 gamma0 (gamma1..10): $GAMMA0_STAGE2"
if [ -z "$GAMMA0_STAGE2" ]; then echo "ERROR: could not parse anchor output"; exit 1; fi

echo "==== STAGE 2: 7 Delta batches, 5 R-values each, all seeded identically from the anchor's gamma ===="

run_batch () {
    local tag=$1 delta=$2 rvals=$3
    local out="$OUTDIR/1290-revgrid-fixedtheta-lag_m-delta${tag}.csv"
    echo "--- Delta=$delta ---"
    ./grid_estimator mode=revgrid_fixedtheta input_csv=$INPUT output_csv=$out \
        theta=$THETA gamma0=$GAMMA0_STAGE2 deltas=$delta rvals=$rvals \
        n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED
}

run_batch "m04"  -0.04  "1424,1602.5,1780.6,1958.6,2136.7"
run_batch "m03"  -0.03  "1417.5,1594.7,1771.8,1949,2126.2"
run_batch "m02"  -0.02  "1409.7,1585.9,1762.1,1938.3,2114.5"
run_batch "m01"  -0.01  "1399.4,1574.3,1749.2,1924.1,2099.1"
run_batch "0"    0      "1361.5,1531.7,1701.9,1872.1,2042.3"
run_batch "p001" 0.001  "1333.9,1500.6,1667.4,1834.1,2000.8"
run_batch "p01"  0.01   "1086.3,1222,1357.8,1493.6,1629.4"

echo "==== Combining all 8 output files (anchor + 7 batches) into one CSV ===="
COMBINED="$OUTDIR/1290-revgrid-fixedtheta-lag_m-combined.csv"
head -1 "$ANCHOR_CSV" > "$COMBINED"
for f in "$ANCHOR_CSV" "$OUTDIR/1290-revgrid-fixedtheta-lag_m-deltam04.csv" "$OUTDIR/1290-revgrid-fixedtheta-lag_m-deltam03.csv" \
         "$OUTDIR/1290-revgrid-fixedtheta-lag_m-deltam02.csv" "$OUTDIR/1290-revgrid-fixedtheta-lag_m-deltam01.csv" \
         "$OUTDIR/1290-revgrid-fixedtheta-lag_m-delta0.csv" "$OUTDIR/1290-revgrid-fixedtheta-lag_m-deltap001.csv" \
         "$OUTDIR/1290-revgrid-fixedtheta-lag_m-deltap01.csv"; do
    tail -n +2 "$f" >> "$COMBINED"
done

echo "==== DONE. Combined output: $COMBINED ===="
wc -l "$COMBINED"
