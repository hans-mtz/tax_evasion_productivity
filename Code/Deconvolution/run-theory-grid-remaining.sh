#!/bin/bash
# 2026-09-18: follow-up to run-theory-grid.sh -- reprioritized per user's
# request to match the actual CV-R headline Delta set {-8,-6,-4,-2,0,+0.5,
# +1,+2}% instead of the original generic {-5..+5}% coarse design.
# -5% (done) stands in for -6% (skipped); -4%,-3%,-2% already done/kept.
# Delta=0's k=0 candidate (R=-807.0769) already fitted via the anchor solve
# (1306-theory-anchor.csv) and used to seed every cell -- skipped here to
# avoid redundant work; only its other 4 candidates are run.
set -e
cd "$(dirname "$0")/../C-estimator"

INPUT="../Products/1260-stage2-revenue-input-lag_m-trim0.005.csv"
THETA="3.46356,5.427e-07,4.3,0.54"
GAMMA0="0.0149403663788425,-0.024012665457907,0.251644032481106,-0.261300274129083,0.0538634719812142,-1.75943378489802,3.04697901885883e-05,4.45927961225049e-08,9.08453223211547,-3.80162066614658e-06"
N_THREADS=12
N_BURN=1000
N_KEEP=3000
MAXTIME=1800
BASE_SEED=20260829
OUT="../Products/1306-theory-remaining"

echo "=== Delta=0 (4 candidates, k=0 already done via anchor) ==="
./grid_estimator input_csv=$INPUT output_csv=${OUT}-delta0.csv mode=revgrid_fixedtheta \
  theta=$THETA gamma0=$GAMMA0 deltas=0 rvals="-895.2827,-851.1798,-762.9741,-718.8712" \
  n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
  row9_mode=theory

echo "=== Delta=-0.08 ==="
./grid_estimator input_csv=$INPUT output_csv=${OUT}-deltam08.csv mode=revgrid_fixedtheta \
  theta=$THETA gamma0=$GAMMA0 deltas=-0.08 rvals="-798.2877,-758.9424,-719.5971,-680.2518,-640.9065" \
  n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
  row9_mode=theory

echo "=== Delta=0.005 ==="
./grid_estimator input_csv=$INPUT output_csv=${OUT}-deltap005.csv mode=revgrid_fixedtheta \
  theta=$THETA gamma0=$GAMMA0 deltas=0.005 rvals="-1068.8107,-1023.7936,-978.7765,-933.7594,-888.7423" \
  n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
  row9_mode=theory

echo "=== Delta=0.01 ==="
./grid_estimator input_csv=$INPUT output_csv=${OUT}-deltap01.csv mode=revgrid_fixedtheta \
  theta=$THETA gamma0=$GAMMA0 deltas=0.01 rvals="-1244.1751,-1197.8330,-1151.4910,-1105.1489,-1058.8069" \
  n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
  row9_mode=theory

echo "=== Delta=0.02 ==="
./grid_estimator input_csv=$INPUT output_csv=${OUT}-deltap02.csv mode=revgrid_fixedtheta \
  theta=$THETA gamma0=$GAMMA0 deltas=0.02 rvals="-1591.1733,-1541.8986,-1492.6240,-1443.3493,-1394.0746" \
  n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
  row9_mode=theory

echo "All remaining batches done."
