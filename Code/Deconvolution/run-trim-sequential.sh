#!/bin/bash
# Trim-cutoff sweep, CROSS-TRIM warm-starting (2026-09-05) -----------------
# Fixes the noise in the first attempt (1221's plot): every trim level there
# started from the SAME untrimmed-sample warm start, independent of every
# other trim level -- fine for small trims, a poor start for heavily-trimmed
# ones, and (with the loosened BOBYQA tolerances needed to stop hanging) let
# different trim levels land in very different-quality local optima with no
# real economic signal in the zigzag that produced.
#
# Fix: process trim levels in INCREASING order, threading the FINAL_PAR:...
# line each run prints (see 1211-stage2-elvis-driver-AB.R's par_init/
# par_init_override wiring) into the next run's par_init CLI arg via a plain
# shell variable -- so each trim level's BOBYQA start is the PREVIOUS trim
# level's own converged fit, not a reset every time. Only the first trim
# level in $TRIMS uses the naive untrimmed warm start (par_init empty).
#
# Usage: run-trim-sequential.sh <ins> "<trim1> <trim2> ..." [start_par_csv]
#   start_par_csv (optional): a comma-separated 12-value par to seed the
#   FIRST trim level in THIS invocation instead of the naive warm start --
#   the cross-machine "overtake" mechanic (seed from wherever the other
#   machine's chain left off, continue on this one).

set -euo pipefail
cd "$(dirname "$0")/../.."

INS="$1"
TRIMS="$2"
PAR="${3:-}"

LAMBDA_GRID="1e-9,2.15e-9,4.64e-9,1e-8,2.15e-8,4.64e-8,1e-7,2.15e-7,4.64e-7,1e-6,2.15e-6,4.64e-6,1e-5,2.15e-5,4.64e-5,1e-4"

for trim in $TRIMS; do
  log="Code/Deconvolution/1211-${INS}-A-trimseq${trim}.Rout"
  echo "=== ins=${INS} trim=${trim} (par_init: ${PAR:-<naive warm start>}) ==="
  out=$(Rscript Code/Deconvolution/1211-stage2-elvis-driver-AB.R \
    ins="${INS}" moment_set=A corner_mode=include_zero n_burn=500 n_keep=1000 \
    lambda_grid="${LAMBDA_GRID}" trim_top_pct="${trim}" \
    maxeval_b=2000 xtol_rel_b=1e-4 maxtime_b=90 n_cores=10 refine=FALSE \
    ${PAR:+par_init="${PAR}"} \
    2>&1 | tee "${log}")
  new_par=$(echo "$out" | grep "^FINAL_PAR:" | sed 's/^FINAL_PAR://')
  if [ -z "$new_par" ]; then
    echo "ERROR: no FINAL_PAR captured for ins=${INS} trim=${trim} -- stopping (check ${log})"
    exit 1
  fi
  PAR="$new_par"
  echo "  -> carried par forward: ${PAR}"
done

echo "SEQUENTIAL TRIM SWEEP FINISHED (ins=${INS})"
