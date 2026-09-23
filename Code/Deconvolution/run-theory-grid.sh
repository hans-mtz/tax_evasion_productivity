#!/bin/bash
# 2026-09-18: theory-coefficient (row9_mode=theory, R_i - t1_i, beta fixed
# at exactly 1 by the accounting identity) coarse grid -- same design as the
# CV/Loss coarse grid (1294), same 11 Deltas, 5 candidates/Delta at
# center +/- se_naive*{-6,-3,0,3,6}, theta fixed at the standard operating
# point, gamma0 seeded from the anchor solve (1306-theory-anchor.csv),
# broadcast identically to every cell, no chaining.
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
OUT="../Products/1306-theory-coarse"

declare -a DELTAS=(-0.05 -0.04 -0.03 -0.02 -0.01 0 0.01 0.02 0.03 0.04 0.05)
declare -a RVALS=(
  "-811.7675,-771.5507,-731.3338,-691.1170,-650.9002"
  "-820.7912,-780.0847,-739.3782,-698.6717,-657.9652"
  "-830.1896,-788.9479,-747.7062,-706.4645,-665.2228"
  "-840.4027,-798.5440,-756.6853,-714.8266,-672.9680"
  "-853.0698,-810.4128,-767.7557,-725.0987,-682.4417"
  "-895.2827,-851.1798,-807.0769,-762.9741,-718.8712"
  "-1244.1751,-1197.8330,-1151.4910,-1105.1489,-1058.8069"
  "-1591.1733,-1541.8986,-1492.6240,-1443.3493,-1394.0746"
  "-1936.1006,-1883.3360,-1830.5714,-1777.8069,-1725.0423"
  "-2278.8063,-2222.1158,-2165.4253,-2108.7347,-2052.0442"
  "-2619.1781,-2558.2260,-2497.2739,-2436.3217,-2375.3696"
)

for i in "${!DELTAS[@]}"; do
  d="${DELTAS[$i]}"
  r="${RVALS[$i]}"
  echo "=== Delta=$d ($((i+1))/${#DELTAS[@]}) ==="
  ./grid_estimator input_csv=$INPUT output_csv=${OUT}-delta${i}.csv mode=revgrid_fixedtheta \
    theta=$THETA gamma0=$GAMMA0 deltas=$d rvals="$r" \
    n_burn=$N_BURN n_keep=$N_KEEP n_threads=$N_THREADS maxtime=$MAXTIME base_seed=$BASE_SEED \
    row9_mode=theory
done

# Combine
head -1 ${OUT}-delta0.csv > ${OUT}-combined.csv
for i in "${!DELTAS[@]}"; do
  tail -n +2 ${OUT}-delta${i}.csv >> ${OUT}-combined.csv
done
echo "Combined: ${OUT}-combined.csv"
