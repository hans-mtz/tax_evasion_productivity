#!/bin/bash
# Extends the CV-adjusted-R grid with 3 new Delta points (-0.07,-0.06,0.005),
# same procedure as the original 11-point grid: coarse (5 pts/Delta) then an
# adaptive fine grid bisecting toward the SOFT (min-subtracted CHT) test's
# chi-sq(10,.95) crossing. Uses the SAME fixed theta, SAME CV anchor gamma,
# SAME n_burn/n_keep/threads/maxtime as the original run. The fine-grid
# generation step combines these new points with the ORIGINAL 11-Delta
# coarse+fine data so the global-min-subtraction stays consistent with the
# existing grid (not a new, locally-different minimum from just 3 far-out
# points). 2026-09-13.
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

DELTAS=(0.005 -0.06 -0.07)

delta_tag () { echo "$1" | sed 's/-/m/; s/\./_/'; }
get_rvals () { local d=$1 csv=$2; LC_ALL=C awk -F, -v d="$d" 'NR>1 && $1+0==d+0 {printf "%s%s", (n++?",":""), $2}' "$csv"; }

run_batch () {
    local delta=$1 rvals=$2 stage=$3
    local tag=$(delta_tag "$delta")
    local out="$PRODUCTS/1298-${stage}-cv-delta${tag}.csv"
    echo "--- $stage cv Delta=$delta rvals=$rvals ---"
    "$BIN" mode=revgrid_fixedtheta input_csv="$INPUT" output_csv="$out" \
        theta=$THETA gamma0=$GAMMA0_CV deltas=$delta rvals="$rvals" \
        n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
        row9_mode=cv cv_beta=$CV_BETA cv_mu_c=$CV_MU_C
}

echo "==== STAGE 1: COARSE (3 new Deltas x 5 points = 15 cells) ===="
for d in "${DELTAS[@]}"; do
    rvals=$(get_rvals "$d" "$PRODUCTS/1298-ext-coarse-grid.csv")
    run_batch "$d" "$rvals" coarse
done

echo "==== Combining new coarse outputs ===="
combined_new="$PRODUCTS/1298-coarse-cv-combined.csv"
first=1
for d in "${DELTAS[@]}"; do
    tag=$(delta_tag "$d")
    f="$PRODUCTS/1298-coarse-cv-delta${tag}.csv"
    if [ $first -eq 1 ]; then head -1 "$f" > "$combined_new"; first=0; fi
    tail -n +2 "$f" >> "$combined_new"
done

echo "==== Merging with original 11-Delta grid for consistent global-min soft test ===="
merged="$PRODUCTS/1298-merged-for-finegrid.csv"
head -1 "$PRODUCTS/1294-coarse-cv-combined.csv" > "$merged"
tail -n +2 "$PRODUCTS/1294-coarse-cv-combined.csv" >> "$merged"
tail -n +2 "$PRODUCTS/1294-fine-cv-combined.csv" >> "$merged"
tail -n +2 "$combined_new" >> "$merged"

echo "==== STAGE 2: adaptive fine-grid generation (soft test, all 14 Deltas, but we'll only RUN the 3 new ones) ===="
Rscript "$ROOT/Code/Deconvolution/1295-make-fine-grid.R" "$merged" "$PRODUCTS/1298-fine-grid-all.csv"

echo "==== STAGE 3: FINE GRID EXECUTION (new Deltas only) ===="
for d in "${DELTAS[@]}"; do
    rvals=$(get_rvals "$d" "$PRODUCTS/1298-fine-grid-all.csv")
    if [ -n "$rvals" ]; then run_batch "$d" "$rvals" fine; else echo "  (no fine points for Delta=$d)"; fi
done

echo "==== Combining new fine outputs ===="
combined_fine_new="$PRODUCTS/1298-fine-cv-combined.csv"
rm -f "$combined_fine_new"
first=1
for d in "${DELTAS[@]}"; do
    tag=$(delta_tag "$d")
    f="$PRODUCTS/1298-fine-cv-delta${tag}.csv"
    if [ -f "$f" ]; then
        if [ $first -eq 1 ]; then head -1 "$f" > "$combined_fine_new"; first=0; fi
        tail -n +2 "$f" >> "$combined_fine_new"
    fi
done

echo "==== EXT STAGE 1 (soft-tuned new points) DONE ===="
