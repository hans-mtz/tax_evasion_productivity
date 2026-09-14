#!/bin/bash
# Full coarse -> fine (Delta,target) pipeline for BOTH the control-variate-
# adjusted-R moment and the revenue-Loss moment, Delta in {-0.05,...,0.05}
# step 0.01 (11 points), theta genuinely FIXED (only gamma optimized per
# cell, same-point independent seeding from each moment type's own
# anchor-solved gamma10 -- no chaining, matching the project's standing
# convention). Detached/autonomous: coarse grid, then adaptive fine grid
# (bisection toward the chi-sq(10,.95) crossing) computed and launched
# automatically with no human check-in required. Uses ABSOLUTE paths
# throughout (no relative cd-chaining) -- deliberate, after finding two real
# path bugs (a wrong `cd ..` and a doubled Code/Code/Products path) in the
# first draft during pre-launch review. 2026-09-12.
set -e
# NOTE (2026-09-13, post-run fix): an earlier version of this script exported
# LC_ALL=C globally to fix awk's comma-decimal locale bug below -- but that
# also broke Rscript's Unicode rendering in 1296's plot titles/subtitles
# (Delta/chi-squared symbols rendered as ".."), since it inherited the same
# non-UTF-8 locale. Scoped the fix to just the awk call in get_rvals()
# instead (see below) so Rscript keeps running under the system's normal
# UTF-8 locale. Data/bounds were never affected by this -- purely cosmetic,
# fixed post-hoc by re-running 1296 standalone under a normal shell.

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

# Filled in from the anchor solves (Code/Products/1294-cv-anchor.csv /
# 1294-loss-anchor.csv), gamma1..10 (10 comma values each). Anchors
# converged 2026-09-12: CV Lhat=0.000155148 (345s, conv=4, wander=0.225);
# Loss Lhat=0.000290032 (345s, conv=4, wander=1.996 -- larger wander than
# CV's, i.e. the two-pass refinement moved gamma more between passes, but
# still XTOL_REACHED on both passes -- the best anchor available from this
# seeding, same device used for the original R-moment's own gamma10).
GAMMA0_CV="0.0149401262989576,-0.0240124349620528,0.251633918064792,-0.261339700381921,0.05385831520302,-1.75944434105163,3.04708748407887e-05,4.02410443759356e-08,9.0848977003403,-3.44834396328732e-06"
GAMMA0_LOSS="0.027858082997624,-0.0568497995580438,0.265395152698368,-0.204057341299654,0.0488113333532436,-0.765988060378964,2.53439407135088e-05,5.95655458460896e-08,10.1345817159284,0.000181255874599813"

DELTAS=(-0.05 -0.04 -0.03 -0.02 -0.01 0 0.01 0.02 0.03 0.04 0.05)

delta_tag () { echo "$1" | sed 's/-/m/; s/\./_/'; }

run_batch () {
    local moment=$1 delta=$2 rvals=$3 gamma0=$4 stage=$5
    local tag=$(delta_tag "$delta")
    local out="$PRODUCTS/1294-${stage}-${moment}-delta${tag}.csv"
    echo "--- $stage $moment Delta=$delta rvals=$rvals ---"
    "$BIN" mode=revgrid_fixedtheta input_csv="$INPUT" output_csv="$out" \
        theta=$THETA gamma0=$gamma0 deltas=$delta rvals="$rvals" \
        n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
        row9_mode=$moment cv_beta=$CV_BETA cv_mu_c=$CV_MU_C
}

echo "==== STAGE 1: COARSE GRID (11 Deltas x 2 moment types x 5 points = 110 cells) ===="
Rscript -e "
suppressMessages(library(tidyverse))
d <- read.csv('$PRODUCTS/1294-cv-loss-grid-design.csv')
mults <- c(-6,-3,0,3,6)
cv_grid <- d %>% rowwise() %>% do(data.frame(Delta=.\$Delta, target=.\$R_cv_center + mults*.\$R_cv_se_naive)) %>% ungroup()
loss_grid <- d %>% rowwise() %>% do(data.frame(Delta=.\$Delta, target=.\$Loss_center + mults*.\$Loss_se_naive)) %>% ungroup()
write.csv(cv_grid, '$PRODUCTS/1294-cv-coarse-grid.csv', row.names=FALSE)
write.csv(loss_grid, '$PRODUCTS/1294-loss-coarse-grid.csv', row.names=FALSE)
"

get_rvals () {
    local d=$1 csv=$2
    LC_ALL=C awk -F, -v d="$d" 'NR>1 && $1+0==d+0 {printf "%s%s", (n++?",":""), $2}' "$csv"
}

for d in "${DELTAS[@]}"; do
    rvals_cv=$(get_rvals "$d" "$PRODUCTS/1294-cv-coarse-grid.csv")
    run_batch cv "$d" "$rvals_cv" "$GAMMA0_CV" coarse
done
for d in "${DELTAS[@]}"; do
    rvals_loss=$(get_rvals "$d" "$PRODUCTS/1294-loss-coarse-grid.csv")
    run_batch loss "$d" "$rvals_loss" "$GAMMA0_LOSS" coarse
done

echo "==== Combining coarse outputs ===="
combine () {
    local moment=$1 stage=$2
    local combined="$PRODUCTS/1294-${stage}-${moment}-combined.csv"
    local first=1
    rm -f "$combined"
    for d in "${DELTAS[@]}"; do
        tag=$(delta_tag "$d")
        f="$PRODUCTS/1294-${stage}-${moment}-delta${tag}.csv"
        if [ -f "$f" ]; then
            if [ $first -eq 1 ]; then head -1 "$f" > "$combined"; first=0; fi
            tail -n +2 "$f" >> "$combined"
        fi
    done
    # if no fine points existed for this moment (nothing matched above), leave
    # $combined absent -- 1296's script checks file.exists() before reading it
}
combine cv coarse
combine loss coarse

echo "==== STAGE 2: adaptive fine-grid generation ===="
Rscript "$ROOT/Code/Deconvolution/1295-make-fine-grid.R" "$PRODUCTS/1294-coarse-cv-combined.csv" "$PRODUCTS/1294-fine-grid-cv.csv"
Rscript "$ROOT/Code/Deconvolution/1295-make-fine-grid.R" "$PRODUCTS/1294-coarse-loss-combined.csv" "$PRODUCTS/1294-fine-grid-loss.csv"

echo "==== STAGE 3: FINE GRID EXECUTION ===="
for d in "${DELTAS[@]}"; do
    rvals=$(get_rvals "$d" "$PRODUCTS/1294-fine-grid-cv.csv")
    if [ -n "$rvals" ]; then run_batch cv "$d" "$rvals" "$GAMMA0_CV" fine; else echo "  (no fine points for cv Delta=$d)"; fi
done
for d in "${DELTAS[@]}"; do
    rvals=$(get_rvals "$d" "$PRODUCTS/1294-fine-grid-loss.csv")
    if [ -n "$rvals" ]; then run_batch loss "$d" "$rvals" "$GAMMA0_LOSS" fine; else echo "  (no fine points for loss Delta=$d)"; fi
done

echo "==== Combining fine outputs ===="
combine cv fine
combine loss fine

echo "==== STAGE 4: final combine + plot ===="
cd "$ROOT" && Rscript "$ROOT/Code/Deconvolution/1296-cv-loss-final-plot.R"

echo "==== ALL DONE ===="
