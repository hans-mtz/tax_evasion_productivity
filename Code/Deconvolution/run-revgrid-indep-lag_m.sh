#!/bin/bash
# Overnight (Delta,R) test-inversion grid for lag_m, no-eta mechanism.
# Stage 1: solve ONE anchor cell (Delta=0, R=baseline) with a random small
#   guess for gamma10 (the new revenue moment's own multiplier -- no prior
#   estimate the way gamma1-9 have from the lag_m lambdagrid fit).
# Stage 2: broadcast the anchor's converged (delta0,lambda,delta1,delta2,
#   gamma1-10) as the IDENTICAL seed for all 35 (Delta,R) cells -- same-point
#   independent seeding, no chaining -- via 7 separate revgrid_indep calls
#   (one per Delta, since the CLI's R-grid is shared across Deltas in one
#   call; each Delta has its own R-grid here, so 7 calls it is), each
#   internally two-level work-stealing its own 5 R-values across all threads.
# 2026-09-10.
set -e
cd "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Code/C-estimator"

INPUT=../Products/1260-stage2-revenue-input-lag_m-trim0.005.csv
OUTDIR=../Products
N_THREADS=12
N_BURN=1000
N_KEEP=3000
MAXTIME=1800   # seconds per NM pass (2 passes/cell) -- 60 min/cell worst case
BASE_SEED=20260829

LAGM_BASE="3.45909754704929,3.501e-07,4.3148781844601,0.541429816967675,0.0103815354498918,-0.0625192041968964,0.0573745869149237,0.15407824623302,0.183220598366824,-0.639931568312248,6.99376628181819e-06,3.67119063253863e-10,0.452288435176922"

echo "==== STAGE 1: anchor cell (Delta=0, R=1700.920471, gamma10=1e-5 random guess) ===="
X0_ANCHOR="${LAGM_BASE},1e-5"
ANCHOR_CSV="$OUTDIR/1282-revgrid-anchor-lag_m.csv"
./grid_estimator mode=revgrid_indep input_csv=$INPUT output_csv=$ANCHOR_CSV \
    x0=$X0_ANCHOR deltas=0 rvals=1700.920471 \
    n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED

echo "==== Parsing anchor result into Stage 2 seed ===="
# Columns: Delta,R,delta0_hat,lambda_hat,delta1_hat,delta2_hat,gamma1..10,Lhat,...
X0_STAGE2=$(tail -1 "$ANCHOR_CSV" | cut -d',' -f3-16)
echo "Stage 2 x0 (delta0,lambda,delta1,delta2,gamma1..10): $X0_STAGE2"
if [ -z "$X0_STAGE2" ]; then echo "ERROR: could not parse anchor output"; exit 1; fi

echo "==== STAGE 2: 7 Delta batches, 5 R-values each, all seeded identically from the anchor ===="

run_batch () {
    local tag=$1 delta=$2 rvals=$3
    local out="$OUTDIR/1282-revgrid-lag_m-delta${tag}.csv"
    echo "--- Delta=$delta ---"
    ./grid_estimator mode=revgrid_indep input_csv=$INPUT output_csv=$out \
        x0=$X0_STAGE2 deltas=$delta rvals=$rvals \
        n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED
}

run_batch "m04"  -0.04  "1434.2,1613.5,1792.7,1972.0,2151.3"
run_batch "m03"  -0.03  "1427.3,1605.7,1784.1,1962.5,2140.9"
run_batch "m02"  -0.02  "1419.4,1596.8,1774.2,1951.6,2129.1"
run_batch "m01"  -0.01  "1408.5,1584.6,1760.6,1936.7,2112.7"
run_batch "0"    0      "1360.7,1530.8,1700.9,1871.0,2041.1"
run_batch "p001" 0.001  "1318.2,1483.0,1647.8,1812.5,1977.3"
run_batch "p01"  0.01   "937.3,1054.4,1171.6,1288.8,1405.9"

echo "==== Combining all 8 output files (anchor + 7 batches) into one CSV ===="
COMBINED="$OUTDIR/1282-revgrid-lag_m-combined.csv"
head -1 "$ANCHOR_CSV" > "$COMBINED"
for f in "$ANCHOR_CSV" "$OUTDIR/1282-revgrid-lag_m-deltam04.csv" "$OUTDIR/1282-revgrid-lag_m-deltam03.csv" \
         "$OUTDIR/1282-revgrid-lag_m-deltam02.csv" "$OUTDIR/1282-revgrid-lag_m-deltam01.csv" \
         "$OUTDIR/1282-revgrid-lag_m-delta0.csv" "$OUTDIR/1282-revgrid-lag_m-deltap001.csv" \
         "$OUTDIR/1282-revgrid-lag_m-deltap01.csv"; do
    tail -n +2 "$f" >> "$COMBINED"
done

echo "==== DONE. Combined output: $COMBINED ===="
wc -l "$COMBINED"
