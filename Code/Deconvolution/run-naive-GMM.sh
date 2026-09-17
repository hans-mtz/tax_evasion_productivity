#!/bin/bash
# Runs in parallel the naive GMM for the two instruments (lag_m and lag_2_cal_W) 
# and two models, exactly identified (4 moments-4 parameters) and overidentified (5 moments-4 parameters)
#
# Usage:
#   Code/Deconvolution/run-naive-GMM.sh # smoke defaults (S=250 n_cores=1/process)
#   Code/Deconvolution/run-naive-GMM.sh 500 3 # S=500 draws/firm, 3 cores/process
#
# N_CORES is PER PROCESS: this script already launches 4 configs (ins x
# corner_mode) as separate parallel OS processes, and 1210's own n_cores arg
# (2026-08-28, RcppParallel-parallelized over firms inside the C++ worker --
# a persistent TBB thread pool, not per-call forking) spins up ADDITIONAL
# threads inside each of those. So (N_CORES x 4) is the total worker count
# this script can run at once -- on a 12-core machine, N_CORES=3 uses the
# whole machine across the 4-way sweep without oversubscribing; N_CORES=1
# (default) is safe everywhere but leaves cores idle during the sweep.
#
# Run from the repo root (paths below are repo-relative, matching the R
# scripts' own load()/source() calls).

set -euo pipefail

# N_BURN="${1:-50}"
# N_KEEP="${2:-500}"
# GRID_PROBS="${3:-0.9,0.75,0.5,0.25,0.1,0.05,0.02,0.01}"
S="${1:-250}" # draws/firm for warm start (1201-stage2-warmstart.R)
N_CORES="${2:-1}"
MODELS="${3:-TRUE FALSE}" # TRUE=exactly identified, FALSE=overidentified


echo "Launching 4 configs in parallel: S=$S n_cores=$N_CORES/process"
pids=()
for ins in lag_m lag_2_cal_W; do
    for model in $MODELS ; do
        tag="${ins}-${model}-S${S}"
        R CMD BATCH "--args ins_only=${ins} S=${S} include_lnM=${model}" \
            Code/Deconvolution/1201-MSM.R \
            "Code/Deconvolution/1201-${tag}.Rout" &
        pids+=($!)
        echo "  started $tag (pid $!)"
    done
done

for pid in "${pids[@]}"; do wait "$pid"; done
echo "All 4 configs finished -- check Code/Deconvolution/1201-*.Rout and Code/Products/1201-MSM-*.RData"
