#!/bin/bash
# FINE k x kappa grid on design i (Hans, 2026-10-02): k in {0.725, 0.75, 0.775} x kappa in {0.4, 0.5, 0.6} around the 1616
# best point (0.75, 0.5), which is not refit (8 new points). Mac mini, 4 fits x 3 threads. Otherwise as 1616: (the three
# clusters of kappa-hat across the good fits: ~0.4-0.9, ~3-5, ~8-9), both pinned; delta0-2 and gamma free.
# gamma_init=solve, IS + mix, rho=prop21 (1599-rhoD-ind), plant clusters, seed 30, start delta from the best validated
# fit 1611-i-k0.7 (same start for every point: no chaining), joint NM 2 passes, maxeval 800 x 21, delta box 100.
# Mac mini, 3 fits x 4 threads. adiag at R = 1000 and 4000.
# Rerun 2026-10-03 (Hans): POINTS="0.75 0.6;0.725 0.4;0.775 0.4;0.775 0.5" NP=4 NT=3, binary rebuilt after the migration
# (the copied grid_estimator_ind5b was a 7 KB stub; the rebuild reproduces TS 23.6894 at 1616).
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
RS=$(command -v Rscript || echo /usr/local/bin/Rscript); NT=${NT:-4}; SUF=${SUF:-}
P=../Products; IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv; DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
getp() { $RS -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local K=$1 KA=$2 KFREE=${3:-0} T
  local X0=$($RS -e "r <- read.csv('$P/1611-i-k0.7.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, $KA, rep(0,22))), sep=',')" 2>/dev/null)
  local B="qform=power_nokink k_fixed=$K row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
  local KF=""; if [ "$KFREE" = 1 ]; then KF="k_free=1 k_min=0.4 k_max=1.0"; T=1617-i-kfree-kappa$KA$SUF; else T=1619-i-k$K-kappa$KA$SUF; fi
  /usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B $KF kappa_fixed=$KA n_keep=1000 lambdas=$KA x0=$X0 algo=neldermead n_passes=2 n_threads=$NT \
    gamma_init=solve maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  local KH=$($RS -e "cat(read.csv('$P/$T.csv')\$k_hat)" 2>/dev/null)
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag ${B/k_fixed=$K/k_fixed=$KH} n_keep=$R par=$(getp $P/$T.csv) n_threads=$NT output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T k=$KH $(date +%H:%M)"; }
export -f fit getp; export P IN DI RS NT SUF
if [ -n "${POINTS:-}" ]; then   # POINTS="k kappa;k kappa;..." (2026-10-03: the four points killed on 2026-10-02), NP fits at once
  echo "$POINTS" | tr ';' '\n' | sed 's/$/ 0/' | xargs -P ${NP:-4} -L 1 bash -c 'fit $0 $1 $2'
elif [ "${MODE:-grid}" = grid ]; then
  for k in 0.725 0.75 0.775; do for ka in 0.4 0.5 0.6; do [ "$k $ka" = "0.75 0.5" ] || echo "$k $ka 0"; done; done | xargs -P 4 -L 1 bash -c 'fit $0 $1 $2'
else   # MODE=kfree: kappa profile with k profiled (free in [0.4, 1.0]), same kappa grid
  for ka in 0.5 4.5 9; do echo "0.7 $ka 1"; done | xargs -P 3 -L 1 bash -c 'fit $0 $1 $2'
fi
echo "all done"
