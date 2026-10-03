#!/bin/bash
# Walk outward from the center (k, kappa) = (0.75, 0.4) on the untrimmed sample (Hans, 2026-10-03), kappa pinned (<= 1),
# R = 500, every point started from the center fit 1627-i-notrim-k0.75-kappa0.4. Wave A = nearest ring:
# (0.75, 0.38), (0.75, 0.36), (0.74, 0.4), (0.73, 0.4); wave B only if nothing passes the hard test (TS <= 27.59):
# (0.75, 0.34), (0.75, 0.32). (0.75, 0.3) and (0.75, 0.4) are already done.
set -uo pipefail
cd "$(dirname "$0")/../Products"
L=../Deconvolution/run-1627-linesearch-notrim.sh; export NK=500 START=1627-i-notrim-k0.75-kappa0.4 TAGP=1631 KMAX=1
passed() { grep -h "^done" 1631-wave*.log 2>/dev/null | awk '{if ($4+0 > 0 && $4+0 <= 27.59) f=1} END{exit !f}'; }
NT=6 bash $L "0.75 0.38" "0.75 0.36" "0.74 0.4" "0.73 0.4" > 1631-waveA.log 2>&1
if passed; then echo "PASS in wave A: stop"; else NT=6 bash $L "0.75 0.34" "0.75 0.32" > 1631-waveB.log 2>&1; passed && echo "PASS in wave B" || echo "no pass"; fi
echo "all done"
