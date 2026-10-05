#!/bin/bash
# Smallest trim that passes (Hans, 2026-10-05; old MacBook): operating point (k, kappa) = (0.75, 0.5) pinned, 1616 configuration
# (design i, 17 rows: drop 1,12,7,5, ind_rows=eps, rho=prop21, D 1599, IS + mix, plant clusters, seed 20260830, R = 1000, NM 2 passes,
# gamma_init=solve). Every trim starts from the 1616 fit's deltas (no chaining). Trims run one at a time, 0.4 -> 0.3 -> 0.2 -> 0.1%,
# adiag at R = 1000 and 4000 after each; stop at the first trim whose TS(R1000) exceeds chi2_17,0.95 = 27.59.
set -uo pipefail
export LC_ALL=C PATH=/usr/local/bin:$PATH
cd "$(dirname "$0")/../C-estimator"
P=../Products; NT=${NT:-12}; DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt); CRIT=27.59
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
X0=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, 0.5, rep(0,22))), sep=',')" 2>/dev/null)
echo "start $(date '+%d %H:%M') x0 $X0"
for tr in 0.004 0.003 0.002 0.001; do
  IN=$P/1630-stage2-input-designA-interior-plant-k-trim$tr.csv; T=1648-i-trim$tr-k0.75-kappa0.5
  B="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps"
  /usr/bin/time -p ./grid_estimator_ind5b mode=lambdagrid $B kappa_fixed=0.5 n_keep=1000 lambdas=0.5 x0=$X0 algo=neldermead n_passes=2 n_threads=$NT \
    gamma_init=solve maxtime=43200 maxeval=16800 kappa_max=20 delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in 1000 4000; do ./grid_estimator_ind5b mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=$NT output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  TS=$(grep -o 'TS = 2 n Lhat = [0-9.]*' $P/$T-adiag-R1000.txt | awk '{print $NF}'); TS4=$(grep -o 'TS = 2 n Lhat = [0-9.]*' $P/$T-adiag-R4000.txt | awk '{print $NF}')
  echo "done trim $tr TS(R1000) $TS TS(R4000) $TS4 $(date '+%d %H:%M')"
  if [ -z "$TS" ] || awk -v t="$TS" -v c=$CRIT 'BEGIN{exit !(t > c)}'; then echo "STOP: trim $tr fails (TS $TS > $CRIT or missing)"; break; fi
done
echo "all done $(date '+%d %H:%M')"
