#!/bin/bash
# Reproduces the old hardcoded 2x2 (ins x corner_mode) sweep that used to live
# inside 1210-stage2-elvis-driver.R, now that the driver takes CLI args and
# runs one config per invocation (see utils-cli.R). Runs the 4 configs as
# separate background R processes -- free process-level parallelism, no code
# change needed beyond what's already in the driver.
#
# Usage:
#   Code/Deconvolution/run-stage2-elvis-sweep.sh                            # smoke defaults (n_burn=50, n_keep=500, n_cores=1/process)
#   Code/Deconvolution/run-stage2-elvis-sweep.sh 100 1000 "0.9,0.75,0.5,0.25,0.1,0.05,0.02,0.01" 3   # wide grid, 3 cores/process
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

N_BURN="${1:-50}"
N_KEEP="${2:-500}"
GRID_PROBS="${3:-0.9,0.75,0.5,0.25,0.1,0.05,0.02,0.01}"
N_CORES="${4:-1}"

echo "Epsilon targets (synchronous -- 1205 and 1211 both depend on its output)"
R CMD BATCH Code/Deconvolution/1206-stage2-eps-targets.R Code/Deconvolution/1206-stage2-eps-targets.Rout

echo "Warm start (synchronous, both ins choices -- 1211 depends on its output; S=250 draws/firm, ~1min)"
R CMD BATCH Code/Deconvolution/1205-stage2-warmstart.R Code/Deconvolution/1205-stage2-warmstart.Rout

echo "Launching 4 configs in parallel: n_burn=$N_BURN n_keep=$N_KEEP n_cores=$N_CORES/process"
pids=()
for ins in lag_m lag_2_cal_W; do
    for moment_set in A ; do
        tag="${ins}-${moment_set}-nburn${N_BURN}-nkeep${N_KEEP}"
        R CMD BATCH "--args ins=${ins} moment_set=${moment_set} n_burn=${N_BURN} n_keep=${N_KEEP} grid_probs=${GRID_PROBS} n_cores=${N_CORES}" \
            Code/Deconvolution/1211-stage2-elvis-driver-AB.R \
            "Code/Deconvolution/1211-${tag}.Rout" &
        pids+=($!)
        echo "  started $tag (pid $!)"
    done
done

for pid in "${pids[@]}"; do wait "$pid"; done
echo "All 4 configs finished -- check Code/Deconvolution/1211-*.Rout and Code/Products/1211-stage2-elvis-*.RData"
