#!/bin/bash
# Launch N_SHARDS concurrent grid_estimator processes, each covering an
# interleaved slice of the grid (idx % N_SHARDS == shard_id) and using
# N_THREADS_PER_SHARD threads for its own per-firm loop -- so
# N_SHARDS * N_THREADS_PER_SHARD should equal (or stay under) the machine's
# core count. Each shard writes its own output CSV
# (<output_prefix>-shard<k>.csv); combine them afterward (they're disjoint,
# a plain rbind/read_csv+bind_rows is enough, no merge logic needed).
#
# Usage: run_grid_shards.sh <n_shards> <n_threads_per_shard> <output_prefix> [extra grid_estimator args...]
# Example (3 points at a time, 4 threads each, 12 cores total):
#   ./run_grid_shards.sh 3 4 ../Products/1226-grid-lag_m \
#     input_csv=../Products/1225-stage2-grid-input-lag_m-trim0.005.csv \
#     n_burn=500 n_keep=2000
# Example (1 point at a time, all 12 threads):
#   ./run_grid_shards.sh 1 12 ../Products/1226-grid-lag_m \
#     input_csv=../Products/1225-stage2-grid-input-lag_m-trim0.005.csv \
#     n_burn=500 n_keep=2000

set -euo pipefail
cd "$(dirname "$0")"

N_SHARDS="$1"; shift
N_THREADS="$1"; shift
OUT_PREFIX="$1"; shift
EXTRA_ARGS=("$@")

pids=()
for ((k=0; k<N_SHARDS; k++)); do
  log="${OUT_PREFIX}-shard${k}.Rout"
  echo "=== launching shard ${k}/${N_SHARDS} (n_threads=${N_THREADS}) -> ${log} ==="
  ./grid_estimator "${EXTRA_ARGS[@]}" \
    output_csv="${OUT_PREFIX}-shard${k}.csv" \
    n_threads="${N_THREADS}" shard_id="${k}" n_shards="${N_SHARDS}" \
    > "${log}" 2>&1 &
  pids+=("$!")
done

echo "Launched ${N_SHARDS} shards, PIDs: ${pids[*]}"
for pid in "${pids[@]}"; do wait "$pid"; done
echo "ALL SHARDS FINISHED"
