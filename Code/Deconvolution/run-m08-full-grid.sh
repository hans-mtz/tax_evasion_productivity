#!/bin/bash
# Full standard grid (coarse + soft-targeted fine + hard-targeted fine) for
# the newly-identified crossing point, Delta=-0.08 -- the smallest tax cut
# found to be statistically distinguishable from Delta=0 under the hard
# test (see run-crossing-search.sh). Same fixed theta, same CV anchor
# gamma, same n_burn/n_keep/threads as every other stage of this grid.
# 2026-09-13.
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

DELTA=-0.08

get_rvals () { local d=$1 csv=$2; LC_ALL=C awk -F, -v d="$d" 'NR>1 && $1+0==d+0 {printf "%s%s", (n++?",":""), $2}' "$csv"; }

run_batch () {
    local rvals=$1 stage=$2
    local out="$PRODUCTS/1300-${stage}-cv-deltam0_08.csv"
    echo "--- $stage cv Delta=$DELTA rvals=$rvals ---"
    "$BIN" mode=revgrid_fixedtheta input_csv="$INPUT" output_csv="$out" \
        theta=$THETA gamma0=$GAMMA0_CV deltas=$DELTA rvals="$rvals" \
        n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
        row9_mode=cv cv_beta=$CV_BETA cv_mu_c=$CV_MU_C
}

echo "==== STAGE 1: COARSE (5 points) ===="
rvals=$(get_rvals "$DELTA" "$PRODUCTS/1300-m08-coarse-grid.csv")
run_batch "$rvals" coarse

echo "==== STAGE 2: soft-targeted fine-grid generation (merged with full existing dataset for a consistent global min) ===="
merged="$PRODUCTS/1300-merged-for-soft-finegrid.csv"
head -1 "$PRODUCTS/1299-cv-all-final-combined.csv" > "$merged"
tail -n +2 "$PRODUCTS/1299-cv-all-final-combined.csv" >> "$merged"
tail -n +2 "$PRODUCTS/1300-coarse-cv-deltam0_08.csv" >> "$merged"
Rscript "$ROOT/Code/Deconvolution/1295-make-fine-grid.R" "$merged" "$PRODUCTS/1300-fine-grid-soft-m08.csv"

echo "==== STAGE 3: soft fine execution (Delta=-0.08 only) ===="
rvals_soft=$(get_rvals "$DELTA" "$PRODUCTS/1300-fine-grid-soft-m08.csv")
if [ -n "$rvals_soft" ]; then run_batch "$rvals_soft" finesoft; else echo "(no soft fine points needed)"; fi

echo "==== Combining coarse + soft-fine for Delta=-0.08 ===="
all_m08="$PRODUCTS/1300-all-cv-deltam0_08.csv"
head -1 "$PRODUCTS/1300-coarse-cv-deltam0_08.csv" > "$all_m08"
tail -n +2 "$PRODUCTS/1300-coarse-cv-deltam0_08.csv" >> "$all_m08"
if [ -f "$PRODUCTS/1300-finesoft-cv-deltam0_08.csv" ]; then
    tail -n +2 "$PRODUCTS/1300-finesoft-cv-deltam0_08.csv" >> "$all_m08"
fi

echo "==== STAGE 4: hard-targeted fine-grid generation (Delta=-0.08 only, using all points tested at this Delta) ===="
Rscript -e "
suppressMessages(library(tidyverse))
n <- 32232; dg <- 10; qc <- qchisq(0.95, dg)
df <- read.csv('$all_m08') %>% mutate(TS_hard = 2*n*Lhat, pass_hard = TS_hard<=qc) %>% arrange(R)
n_sub <- nrow(df)
lower_bracket <- NULL
for (i in 1:(n_sub-1)) if (!df\$pass_hard[i] && df\$pass_hard[i+1]) { lower_bracket <- c(i,i+1); break }
upper_bracket <- NULL
for (i in n_sub:2) if (!df\$pass_hard[i] && df\$pass_hard[i-1]) { upper_bracket <- c(i-1,i); break }
interp <- function(idx) { r_lo<-df\$R[idx[1]]; r_hi<-df\$R[idx[2]]; ts_lo<-df\$TS_hard[idx[1]]; ts_hi<-df\$TS_hard[idx[2]]; r_lo+(r_hi-r_lo)*(qc-ts_lo)/(ts_hi-ts_lo) }
rows <- list()
if (!is.null(lower_bracket)) rows[[length(rows)+1]] <- tibble(Delta=-0.08, target=interp(lower_bracket)) else cat('lower side OPEN\n')
if (!is.null(upper_bracket)) rows[[length(rows)+1]] <- tibble(Delta=-0.08, target=interp(upper_bracket)) else cat('upper side OPEN\n')
out <- bind_rows(rows)
write.csv(out, '$PRODUCTS/1300-fine-grid-hard-m08.csv', row.names=FALSE)
print(out)
"

echo "==== STAGE 5: hard fine execution (Delta=-0.08 only) ===="
rvals_hard=$(get_rvals "$DELTA" "$PRODUCTS/1300-fine-grid-hard-m08.csv")
if [ -n "$rvals_hard" ]; then run_batch "$rvals_hard" finehard; else echo "(no hard fine points needed)"; fi

echo "==== Final combine for Delta=-0.08 ===="
final="$PRODUCTS/1300-final-cv-deltam0_08.csv"
head -1 "$PRODUCTS/1300-coarse-cv-deltam0_08.csv" > "$final"
tail -n +2 "$PRODUCTS/1300-coarse-cv-deltam0_08.csv" >> "$final"
if [ -f "$PRODUCTS/1300-finesoft-cv-deltam0_08.csv" ]; then tail -n +2 "$PRODUCTS/1300-finesoft-cv-deltam0_08.csv" >> "$final"; fi
if [ -f "$PRODUCTS/1300-finehard-cv-deltam0_08.csv" ]; then tail -n +2 "$PRODUCTS/1300-finehard-cv-deltam0_08.csv" >> "$final"; fi

echo "==== M08 FULL GRID DONE ===="
