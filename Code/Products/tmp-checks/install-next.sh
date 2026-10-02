#!/bin/bash
# swap in the review-4 binaries once the 1600 fits (which call grid_estimator_s2 / _ind5b for their adiag steps) are done
cd "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Code/C-estimator"
while ! grep -q "all done" ../Products/1600-launch.log || ! grep -q "all done" ../Products/1601-launch.log 2>/dev/null; do sleep 60; done
mv grid_estimator_s2_next grid_estimator_s2 && mv grid_estimator_ind5b_next grid_estimator_ind5b && echo "installed $(date +%H:%M)"
