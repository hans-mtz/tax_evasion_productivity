#!/bin/bash
# WIDER follow-up pass, same fixed theta as 1290 (the 3D cube's global min),
# same gamma seed (reused directly from the 1290 anchor's own converged
# output -- no need to re-solve, it already fit the center of the grid
# well and NM refines per-cell regardless). Launched because 1290's own
# +-20% R-grid was still vacuous (max TS=0.45 vs threshold 18.3) -- fixing
# theta did NOT fix this; a quadratic extrapolation on 1290's own (cleaner,
# theta-fixed) data now consistently points to needing +-133% to +-168%
## deviation across all 7 Delta legs (vs. noisy/inconsistent estimates from
# the earlier free-theta attempt). R-grid widened to baseline x
# {-0.5,0.25,1.0,1.75,2.5} to comfortably bracket that range. 2026-09-12,
# launched autonomously overnight while the user sleeps, per their own
# explicit request for a second, wider pass.
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
GAMMA0="0.0346259131274546,-0.0314423872203614,0.213345547568563,-0.269758367960626,0.0579244974907063,-1.13805034160568,1.45248121626455e-05,1.42024529746241e-08,4.37374834787322,1.5341498688692e-06"

run_batch () {
    local tag=$1 delta=$2 rvals=$3
    local out="$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-delta${tag}.csv"
    echo "--- Delta=$delta ---"
    ./grid_estimator mode=revgrid_fixedtheta input_csv=$INPUT output_csv=$out \
        theta=$THETA gamma0=$GAMMA0 deltas=$delta rvals=$rvals \
        n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED
}

run_batch "m04"  -0.04  "-890.3,445.1,1780.6,3116,4451.4"
run_batch "m03"  -0.03  "-885.9,443,1771.8,3100.7,4429.6"
run_batch "m02"  -0.02  "-881.1,440.5,1762.1,3083.7,4405.3"
run_batch "m01"  -0.01  "-874.6,437.3,1749.2,3061.1,4373"
run_batch "0"    0      "-851,425.5,1701.9,2978.4,4254.8"
run_batch "p001" 0.001  "-833.7,416.8,1667.4,2917.9,4168.4"
run_batch "p01"  0.01   "-678.9,339.5,1357.8,2376.2,3394.6"

echo "==== Combining all 7 output files into one CSV ===="
COMBINED="$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-combined.csv"
FIRST="$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-deltam04.csv"
head -1 "$FIRST" > "$COMBINED"
for f in "$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-deltam04.csv" "$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-deltam03.csv" \
         "$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-deltam02.csv" "$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-deltam01.csv" \
         "$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-delta0.csv" "$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-deltap001.csv" \
         "$OUTDIR/1291-revgrid-fixedtheta-wide-lag_m-deltap01.csv"; do
    tail -n +2 "$f" >> "$COMBINED"
done

echo "==== DONE. Combined output: $COMBINED ===="
wc -l "$COMBINED"
