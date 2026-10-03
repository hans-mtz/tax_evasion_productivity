#!/bin/bash
# Trimming analysis and moment variants (2026-10-03, Hans): same fit as run-1627 (design i, warm start, D 1599, IS + mix,
# plant clusters, seed 30, joint NM 2 passes, adiag at R = NK, 1000, 4NK), with env IN (input csv in Products), DROP (drop_rows),
# BIN (binary), START (fit whose deltas seed every point), TAGP and TAGS (tag prefix/suffix). Usage: run-1633-fit.sh "<k> <kappa>" ...
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products; IN=$P/${IN:-1624-stage2-input-designA-interior-plant-k-notrim.csv}; DROP=${DROP:-1,12,7,5}; BIN=${BIN:-grid_estimator_ind5b}; DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
NK=${NK:-1000}; START=${START:-1631-i-notrim-k0.75-kappa0.38-R500}; TAGP=${TAGP:-1633}   # NK = n_keep of the fit; START = validated fit whose delta's seed every point
NT=${NT:-$(( 12 / $# ))}; [ $NT -lt 1 ] && NT=1   # NT env overrides (oversubscribe so firm-level work stealing spreads each fit over P and E cores)
getp() { Rscript -e "r <- read.csv('$1'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null; }
fit() { local K=$1 KA=$2 T=$TAGP-i-${TAGS:-notrim}-k$1-kappa$2-R$NK
  local KS=$KA KFX="kappa_fixed=$KA"; case "$KA" in free*) KS=${KA#free}; KFX="";; esac   # "free<start>": kappa estimated from <start>
  local X0=$(Rscript -e "r <- read.csv('$P/$START.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('delta0_hat','delta1_hat','delta2_hat')]), $K, 0.3, $KS, rep(0,22))), sep=',')" 2>/dev/null)
  local B="qform=power_nokink k_fixed=$K row6=eps_psi input_csv=$IN n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=$DROP ind_rows=eps"
  ./$BIN mode=lambdagrid $B $KFX n_keep=$NK lambdas=$KS x0=$X0 algo=neldermead n_passes=2 n_threads=$NT \
    gamma_init=solve maxtime=43200 maxeval=16800 kappa_max=${KMAX:-20} delta_max=100 output_csv=$P/$T.csv > $P/$T.Rout 2>&1
  for R in $NK 1000 $((4*NK)); do ./$BIN mode=adiag $B n_keep=$R par=$(getp $P/$T.csv) n_threads=$NT output_csv=/dev/null > $P/$T-adiag-R$R.txt 2>&1; done
  echo "done $T TS(R$NK) $(grep -o 'TS = 2 n Lhat = [0-9.]*' $P/$T-adiag-R$NK.txt | awk '{print $NF}') TS(R1000) $(grep -o 'TS = 2 n Lhat = [0-9.]*' $P/$T-adiag-R1000.txt | awk '{print $NF}') $(date +%H:%M)"; }
for pt in "$@"; do fit $pt & done; wait; echo "all done"
