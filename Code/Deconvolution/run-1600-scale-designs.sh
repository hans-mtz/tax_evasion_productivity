#!/bin/bash
# Scale designs (Hans, 2026-10-01), four separate estimates on the ladder base (rung 5: rows 0-4, 6, 8, 9, 11; interior;
# rung 6 (drop row 8) worsened the tilted fit and was reverted;
# no kink, k = 0.75, kappa estimated, bound 20). Same estimator as the ladder: joint NM 2 passes, maxeval 400 x free dims,
# sampler=is, proposal=mix, rho=prop21 (ladder D, with D for the new rows spliced in), cluster=plant, seed 30, n_keep 1000.
# Same start: theta from the 1594 NM point, gamma 0. adiag at R = 1000 and 4R = 4000.
#   i      design i: eps by industry (ind_rows=eps, rows 13-21), pooled row 1 dropped            [grid_estimator_ind5b]
#   i_med  design i robustness: deconvolved medians (ind_rows=median, rows 13-21; 351, 352 rows 19, 20 dropped)
#   ii_k   design ii: audit row (row 10), G = top 10% of capital within industry, p = 0.53     [grid_estimator_s2]
#   ii_v   design ii robustness: G = top 10% of V within industry, p = 0.53
set -uo pipefail
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv; ST=$P/1594-nm-2pass.csv
X0_13=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
X0_22=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
PAR0_22=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', c(unlist(r[1, c('kappa_hat','delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
KAP=$(Rscript -e "r <- read.csv('$ST'); cat(sprintf('%.15g', r\$kappa_hat))" 2>/dev/null)
DA=$(sed -n 's/rho_D=//p' $P/1599-rhoD-audit.txt); DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=0"
if [ ! -s $P/1600-rhoD-med.txt ]; then   # D for the median rows, spliced after the ladder's 13 values
  DM=$(./grid_estimator_ind5b mode=rhoD $B n_keep=1000 base_seed=20260830 output_csv=/dev/null drop_rows=12,7,5,19,20 ind_rows=median par=$PAR0_22 \
       | sed -n 's/RHO_D: //p' | cut -d, -f14-22)
  DM=$(echo $DM | awk -F, 'BEGIN{OFS=","}{$7=1; $8=1; print}')   # rows 19, 20 dropped (351, 352: no median)
  echo "rho_D=$(echo $DA | cut -d, -f1-13 | awk -F, 'BEGIN{OFS=","}{$11=0; print}'),$DM" > $P/1600-rhoD-med.txt
fi
DM=$(sed -n 's/rho_D=//p' $P/1600-rhoD-med.txt)
E="rho=prop21 sampler=is proposal=mix cluster=plant base_seed=20260830"
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:$2))])), sep=',')" 2>/dev/null; }
fit() { local TAG=$1 BIN=$2 NG=$3 X0=$4 NFREE=$5; shift 5
  local T=1600-$TAG
  /usr/bin/time -p ./$BIN mode=lambdagrid $B $E n_keep=1000 lambdas=$KAP x0=$X0 algo=neldermead n_passes=2 n_threads=3 maxtime=43200 \
    maxeval=$((400 * NFREE)) kappa_max=20 output_csv=$P/$T.csv "$@" > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./$BIN mode=adiag $B $E n_keep=$R par=$(getp $P/$T.csv $NG) n_threads=3 output_csv=/dev/null "$@" > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T $(date +%H:%M)"; }
fit i     grid_estimator_ind5b 22 $X0_22 21 rho_D=$DI drop_rows=1,12,7,5 ind_rows=eps &
fit i_med grid_estimator_ind5b 22 $X0_22 20 rho_D=$DM drop_rows=12,7,5,19,20 ind_rows=median &
fit ii_k  grid_estimator_s2    13 $X0_13 14 rho_D=$DA drop_rows=12,7,5 audit_p=0.53 audit_group=k &
fit ii_v  grid_estimator_s2    13 $X0_13 14 rho_D=$DA drop_rows=12,7,5 audit_p=0.53 audit_group=v &
wait; echo "all done"
