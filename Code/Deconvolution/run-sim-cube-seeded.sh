#!/bin/bash
# ELVIS on the M+K simulated baseline (2026-09-19).
# Stage 1: lambdagrid mode, ONE point at the TRUE lambda=1e-4 (delta0,delta1,delta2,gamma all profiled), all 12 threads (firm-level work-stealing),
#          refit from its own converged (delta0,delta1,delta2,gamma) until L̂ improves <2%, at most 10 rounds.
# Stage 2: 27 cube cells (lambda,delta1,delta2 fixed; delta0,gamma free), each an independent single-point grid3d call seeded from the stage-1
#          (delta0,gamma) -- no chaining. 4 concurrent x 3 threads (xargs queue = dynamic point-level balancing; firm-level work-stealing inside).
set -e
cd "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Code/C-estimator"
IN=../Products/msl/1411-base-elvis-input.csv; OUT=../Products/1431-elvis-sim
COMMON="input_csv=$IN n_burn=1000 n_keep=3000 maxtime=1800 base_seed=20260919"
X0="3.837,3.5,0.5,0,0,0,0,0,0,0,0,0"; PREV=""
for r in $(seq 1 10); do
  ./grid_estimator $COMMON mode=lambdagrid lambdas=0.0001 x0=$X0 algo=neldermead n_threads=12 output_csv=$OUT-lam-r$r.csv > $OUT-lam-r$r.Rout 2>&1
  L=$(Rscript -e 'd<-read.csv(commandArgs(TRUE)[1]); cat(d$Lhat[1])' $OUT-lam-r$r.csv)
  X0=$(Rscript -e 'd<-read.csv(commandArgs(TRUE)[1]); cat(paste(unlist(d[1,c("delta0_hat","delta1_hat","delta2_hat",paste0("gamma",1:9))]), collapse=","))' $OUT-lam-r$r.csv)
  echo "lambda-point round $r: Lhat=$L  (delta0,delta1,delta2,gamma1..9)=$X0"
  if [ -n "$PREV" ] && Rscript -e 'a<-as.numeric(commandArgs(TRUE)); quit(status=as.integer(!(a[2] > 0.98*a[1])))' $PREV $L; then echo "stopped: <2% improvement"; break; fi
  PREV=$L
done
echo "$X0" > $OUT-seed-x0-12.txt
X10=$(echo "$X0" | awk -F, '{printf "%s", $1; for(i=4;i<=12;i++) printf ",%s", $i}')   # delta0,gamma1..9 (delta1,delta2 are fixed per cube cell)
echo "cube seed x0 (delta0,gamma1..9) = $X10"
i=0; tail -n +2 points_sim_cube_27.csv | while IFS= read -r line; do i=$((i+1)); printf 'lambda,delta1,delta2\n%s\n' "$line" > $OUT-pt$i.pts.csv; echo $i; done | \
  xargs -P 4 -I{} sh -c "./grid_estimator $COMMON mode=grid3d algo=neldermead points_csv=$OUT-pt{}.pts.csv x0=$X10 n_threads=3 output_csv=$OUT-pt{}.csv > $OUT-pt{}.Rout 2>&1"
echo "ALL DONE"
