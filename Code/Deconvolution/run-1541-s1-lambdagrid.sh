#!/bin/bash
# S1 (Thesis/PLAN.md §9b): new data (1532, all industries corrected, corner = tau_P==0), old linear q, moment set A.
# 16-point lambda grid = the old no-eta grid's values (1270), for a point-by-point comparison.
# Same-point seeding at every lambda from the old cube optimum (delta0,delta1,delta2)=(3.464,4.3,0.54) + its gamma.
# (delta0,delta1,delta2,gamma1..9) free at each lambda; Nelder-Mead, two passes, maxtime=1800 s per pass (as every past run). caffeinate bound to the PID.
set -euo pipefail
cd "$(dirname "$0")/../C-estimator"
X0="3.46356014640655,4.3,0.54,0.0143883116027861,-0.0119013123755461,0.0630942173107609,-0.0783100943504263,0.0578349092582138,-0.907539328124065,1.27340923253472e-05,7.34383264616891e-09,2.64362954929756"
LAMS="1.628e-08,2.524e-08,3.912e-08,6.064e-08,9.399e-08,1.17e-07,1.457e-07,2.258e-07,3.501e-07,5.427e-07,8.412e-07,6.045e-06,4.345e-05,0.0003123,0.002244,0.01613"
nohup ./grid_estimator mode=lambdagrid input_csv=../Products/1532-stage2-input-allcorr-trim0.005.csv \
  lambdas=$LAMS x0=$X0 algo=neldermead n_burn=1000 n_keep=3000 n_threads=12 maxtime=1800 base_seed=20260829 \
  output_csv=../Products/1541-s1-lambdagrid-allcorr.csv > ../Products/1541-s1-lambdagrid-allcorr.Rout 2>&1 &
P=$!
nohup caffeinate -dims -w $P >/dev/null 2>&1 &
echo "S1 launched, PID $P; log Code/Products/1541-s1-lambdagrid-allcorr.Rout"
