#!/bin/bash
# Generic cfprofile launcher (2026-10-06): env JOBS = space-separated "target:Delta" pairs, NK (draws, default 1000), MRESP (default 1),
# NT (threads per run), PAR (max concurrent runs, default 2), TAG (output prefix). Audited solver: binary grid_estimator_ind5b_cf6,
# cf_multi=1 with 3 starts (cf_op=0 cf_g10=10), operating point 1616, seed 20260830, central difference h = 0.01.
# Outputs Products/<TAG>-cf-<target>-mr<MRESP>-D<Delta>.{csv,Rout}.
set -uo pipefail
export LC_ALL=C PATH=/usr/local/bin:$PATH
cd "$(dirname "$0")/../C-estimator"
P=../Products; NT=${NT:-2}; NK=${NK:-1000}; MRESP=${MRESP:-1}; TAG=${TAG:-1654}; PAR=${PAR:-2}
PARV=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=$NK par=$PARV n_threads=$NT cf_cold=1 cf_multi=1 cf_op=0 cf_g10=10 cf_mresp=$MRESP"
run() { local tg=${1%%:*} d=${1##*:}; local o=$P/$TAG-cf-$tg-mr$MRESP-D$d
  /usr/bin/time -p ./grid_estimator_ind5b_cf6 $B cf_target=$tg deltas=$d output_csv=$o.csv > $o.Rout 2>&1; echo "done $tg D=$d exit $? $(date '+%d %H:%M') $(grep -o 'Delta .*TS_min [0-9.]*' $o.Rout | tail -1)"; }
for j in $JOBS; do while [ $(jobs -rp | wc -l) -ge $PAR ]; do sleep 5; done; run $j & done
wait; echo "all done $(date '+%d %H:%M')"
