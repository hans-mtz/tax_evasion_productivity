#!/bin/bash
# Sequential adaptive search for the CV-adjusted-R hard-test crossing point
# (the Delta where a tax cut first becomes statistically distinguishable
# from Delta=0's own hard-test interval). Efficient one-point-per-candidate
# design: since cuts only raise revenue in this range, testing the single
# candidate R = Delta=0's own hard-test upper bound (1744.6285220744) at a
# new Delta tells us everything needed -- REJECTED there means that Delta's
# whole interval sits above 1744.63 (full separation); still passing means
# it doesn't yet.
#
# Algorithm (per the user's explicit design, 2026-09-13):
#  1. Test -10%. If it separates, walk BACK toward zero (-9%, -8%; -7% is
#     already known from the full grid to NOT separate) until the first
#     point that does NOT separate -- that brackets the crossing to 1pp.
#  2. If -10% does NOT separate, escalate to -15%, confirm it separates,
#     then walk back in 1% steps (-14%,-13%,...) stopping at the first
#     point that does NOT separate.
#  3. Once bracketed, run the FULL standard grid (coarse+fine, both soft
#     and hard tests -- matching every other Delta in this project) at the
#     Delta identified as the crossing (the smallest cut that fully
#     separates), to get real upper/lower bounds there.
# Same fixed theta, same CV anchor gamma, same n_burn/n_keep/threads as
# every other stage of this grid.
set -e
export LC_ALL=C
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

N=32232
QC=18.3070380533
R_TEST=1744.6285220744

delta_tag () { echo "$1" | sed 's/-/m/; s/\./_/'; }

get_lhat () {
    local csv=$1
    LC_ALL=C awk -F, 'NR==1{for(i=1;i<=NF;i++) if($i=="Lhat") c=i} NR==2{print $c}' "$csv"
}

# Returns 1 (true) if Delta separates from Delta=0's hard bound, 0 otherwise.
# Logs the point and result to the shared crossing-search log CSV.
test_point () {
    local delta=$1
    local tag=$(delta_tag "$delta")
    local out="$PRODUCTS/1300-crossing-cv-delta${tag}.csv"
    echo "--- testing crossing candidate Delta=$delta (R=$R_TEST) ---" >&2
    "$BIN" mode=revgrid_fixedtheta input_csv="$INPUT" output_csv="$out" \
        theta=$THETA gamma0=$GAMMA0_CV deltas=$delta rvals=$R_TEST \
        n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
        row9_mode=cv cv_beta=$CV_BETA cv_mu_c=$CV_MU_C >&2
    local lhat=$(get_lhat "$out")
    local ts_hard=$(echo "2 * $N * $lhat" | bc -l)
    local separates=$(echo "$ts_hard > $QC" | bc -l)
    echo "$delta,$lhat,$ts_hard,$separates" >> "$PRODUCTS/1300-crossing-search-log.csv"
    echo "RESULT: Delta=$delta Lhat=$lhat TS_hard=$ts_hard separates=$separates" >&2
    echo "$separates"
}

echo "Delta,Lhat,TS_hard,separates" > "$PRODUCTS/1300-crossing-search-log.csv"

echo "==== CROSSING SEARCH: testing -10% first ===="
sep_m10=$(test_point -0.10)

CROSSING_DELTA=""

if [ "$sep_m10" = "1" ]; then
    echo "==== -10% SEPARATES. Walking back toward zero: -9%, -8% ===="
    sep_m09=$(test_point -0.09)
    if [ "$sep_m09" = "1" ]; then
        sep_m08=$(test_point -0.08)
        if [ "$sep_m08" = "1" ]; then
            echo "==== -8% separates; -7% already known (full grid) to NOT separate -> crossing is Delta=-0.08 ===="
            CROSSING_DELTA="-0.08"
        else
            echo "==== -8% does NOT separate; -9% does -> crossing is Delta=-0.09 ===="
            CROSSING_DELTA="-0.09"
        fi
    else
        echo "==== -9% does NOT separate; -10% does -> crossing is Delta=-0.10 ===="
        CROSSING_DELTA="-0.10"
    fi
else
    echo "==== -10% does NOT separate. Escalating to -15% ===="
    sep_m15=$(test_point -0.15)
    if [ "$sep_m15" != "1" ]; then
        echo "==== ANOMALY: -15% ALSO does not separate. Stopping for manual review -- this contradicts every trend seen so far. ===="
        exit 1
    fi
    echo "==== -15% separates. Walking back in 1% steps: -14%,-13%,-12%,-11% ===="
    CROSSING_DELTA="-0.15"
    for d in -0.14 -0.13 -0.12 -0.11; do
        sep=$(test_point "$d")
        if [ "$sep" != "1" ]; then
            echo "==== $d does NOT separate; crossing is the previous point, Delta=$CROSSING_DELTA ===="
            break
        fi
        CROSSING_DELTA="$d"
    done
fi

echo "==== CROSSING IDENTIFIED: Delta=$CROSSING_DELTA ===="
echo "$CROSSING_DELTA" > "$PRODUCTS/1300-crossing-delta.txt"
echo "==== CROSSING SEARCH DONE ===="
