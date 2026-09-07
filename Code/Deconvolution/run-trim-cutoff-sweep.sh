#!/bin/bash
# Trim-cutoff sweep (2026-09-05) -- runs 1211-stage2-elvis-driver-AB.R across
# many trim_top_pct levels x both ins choices, each with a wide continuous
# log-spaced lambda grid (16 points, 1e-9 to 1e-4, factor ~2.15 spacing),
# batched 4-at-a-time (3 cores each = 12 total) to avoid oversubscribing a
# 12-core machine. Purpose: build the CLAUDE.md/paper figure -- x=trim
# threshold, y=lambda point estimate + 95% test-inversion CI -- documenting
# where the trim-implied lambda stabilizes before over-trimming moves it
# again (see CLAUDE.md's trim-robustness entries for why this matters).
#
# Reuses trim=0 from Phase 0's already-resolved fine sweep (Code/Products/
# 1218-stage2-lambda-ci.RData) -- NOT rerun here.

set -euo pipefail
cd "$(dirname "$0")/../.."

## 2026-09-05, corrected: 4-concurrent-jobs-at-3-cores-each caused severe
## contention (20+ min, zero points finished, vs 66s solo at n_cores=5) --
## every other successful run today used 2 concurrent at 5 cores each.
## Switched back to that, and split the 14 trim levels across two machines
## (this Mac mini + the user's MacBook, see CLAUDE.md 2026-09-05 SSH entry)
## -- this script takes the trim-level LIST as $1 (space-separated, quoted)
## so the same script runs both halves.
TRIMS="${1:-0.001 0.0025 0.005 0.0075 0.01 0.015 0.02 0.03 0.04 0.05 0.07 0.10 0.15 0.20}"
INS="lag_m lag_2_cal_W"
LAMBDA_GRID="1e-9,2.15e-9,4.64e-9,1e-8,2.15e-8,4.64e-8,1e-7,2.15e-7,4.64e-7,1e-6,2.15e-6,4.64e-6,1e-5,2.15e-5,4.64e-5,1e-4"
MAX_CONCURRENT=2
N_CORES_PER_JOB=5

# Fixed-group batching (not wait -n): this machine's /bin/bash is 3.2
# (arm64 macOS ships an old bash for licensing reasons), which lacks
# `wait -n` (needs 4.3+). Launch MAX_CONCURRENT jobs, wait for the whole
# group, then launch the next -- slightly less efficient than start-as-
# soon-as-one-frees-up, but portable and simple.
jobs_list=()
for trim in $TRIMS; do
  for ins in $INS; do
    jobs_list+=("${ins}:${trim}")
  done
done

i=0
n=${#jobs_list[@]}
while [ "$i" -lt "$n" ]; do
  batch_pids=()
  batch_end=$((i + MAX_CONCURRENT))
  if [ "$batch_end" -gt "$n" ]; then batch_end=$n; fi
  while [ "$i" -lt "$batch_end" ]; do
    entry="${jobs_list[$i]}"
    ins="${entry%%:*}"
    trim="${entry##*:}"
    log="Code/Deconvolution/1211-${ins}-A-trimsweep${trim}.Rout"
    echo "Launching ins=${ins} trim=${trim}"
    Rscript Code/Deconvolution/1211-stage2-elvis-driver-AB.R \
      ins="${ins}" moment_set=A corner_mode=include_zero n_burn=500 n_keep=1000 \
      lambda_grid="${LAMBDA_GRID}" trim_top_pct="${trim}" \
      maxeval_b=2000 xtol_rel_b=1e-4 maxtime_b=90 n_cores="${N_CORES_PER_JOB}" refine=FALSE \
      > "${log}" 2>&1 &
    batch_pids+=($!)
    i=$((i + 1))
  done
  wait "${batch_pids[@]}"
  echo "Batch finished (jobs $((i - ${#batch_pids[@]})) to $((i - 1)))"
done
echo "ALL TRIM-SWEEP JOBS FINISHED"
