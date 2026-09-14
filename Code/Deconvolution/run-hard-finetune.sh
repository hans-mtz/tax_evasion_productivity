#!/bin/bash
# Step 2: hard-test-targeted fine-tuning of the CV-adjusted-R grid, for
# Delta in {-0.07,-0.06,-0.05,-0.04,-0.03,-0.02,-0.01,0,0.005,0.01,0.02}
# (the Deltas whose hard-test bounds were found unresolved -- see the
# gap analysis in Research-log/log.md). Uses ALL previously-tested points
# (coarse + soft-fine) at each Delta to find the TS_hard=2n*Lhat crossing
# bracket, then runs ONE new bisection point per side. Same fixed theta,
# same CV anchor gamma, same n_burn/n_keep/threads/maxtime as every prior
# stage of this grid. 2026-09-13.
set -e
ROOT="/Volumes/SSD Hans/Github/Tax_Evasion_Productivity"
PRODUCTS="$ROOT/Code/Products"
BIN="$ROOT/Code/C-estimator/grid_estimator"
INPUT="$PRODUCTS/1260-stage2-revenue-input-lag_m-trim0.005.csv"
THETA="3.46356,5.427e-07,4.3,0.54"
N_THREADS=12
N_BURN=1000
N_KEEP=3000
MAXTIME=1800
BASE_SEED=20260829
CV_BETA=0.998838
CV_MU_C=2507.602596
GAMMA0_CV="0.0149401262989576,-0.0240124349620528,0.251633918064792,-0.261339700381921,0.05385831520302,-1.75944434105163,3.04708748407887e-05,4.02410443759356e-08,9.0848977003403,-3.44834396328732e-06"

DELTAS=(-0.07 -0.06 -0.05 -0.04 -0.03 -0.02 -0.01 0 0.005 0.01 0.02)

delta_tag () { echo "$1" | sed 's/-/m/; s/\./_/'; }
get_rvals () { local d=$1 csv=$2; LC_ALL=C awk -F, -v d="$d" 'NR>1 && $1+0==d+0 {printf "%s%s", (n++?",":""), $2}' "$csv"; }

run_batch () {
    local delta=$1 rvals=$2
    local tag=$(delta_tag "$delta")
    local out="$PRODUCTS/1299-hardfine-cv-delta${tag}.csv"
    echo "--- hardfine cv Delta=$delta rvals=$rvals ---"
    "$BIN" mode=revgrid_fixedtheta input_csv="$INPUT" output_csv="$out" \
        theta=$THETA gamma0=$GAMMA0_CV deltas=$delta rvals="$rvals" \
        n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
        row9_mode=cv cv_beta=$CV_BETA cv_mu_c=$CV_MU_C
}

echo "==== STAGE 1: generate hard-test fine-grid targets ===="
Rscript "$ROOT/Code/Deconvolution/1299-make-hard-fine-grid.R" "$PRODUCTS/1298-cv-all14-combined.csv" "$PRODUCTS/1299-hard-fine-grid.csv"

echo "==== STAGE 2: HARD FINE GRID EXECUTION (11 Deltas x up to 2 points = up to 22 cells) ===="
for d in "${DELTAS[@]}"; do
    rvals=$(get_rvals "$d" "$PRODUCTS/1299-hard-fine-grid.csv")
    if [ -n "$rvals" ]; then run_batch "$d" "$rvals"; else echo "  (no hard-fine points for Delta=$d)"; fi
done

echo "==== Combining hard-fine outputs ===="
combined="$PRODUCTS/1299-hardfine-cv-combined.csv"
rm -f "$combined"
first=1
for d in "${DELTAS[@]}"; do
    tag=$(delta_tag "$d")
    f="$PRODUCTS/1299-hardfine-cv-delta${tag}.csv"
    if [ -f "$f" ]; then
        if [ $first -eq 1 ]; then head -1 "$f" > "$combined"; first=0; fi
        tail -n +2 "$f" >> "$combined"
    fi
done

echo "==== STEP 2 (hard-test fine-tuning) DONE ===="
