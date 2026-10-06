#!/bin/bash
# Revenue counterfactual grid (Hans, 2026-10-05): cf_target=revenue with true M responding (cf_mresp=1, headline), audited solver
# (binary grid_estimator_ind5b_cf6, cf_multi=1 with 3 starts per solve: warm, best-so-far, gamma10 = +10; cf_op=0), operating point
# 1616, R = 1000, seed 20260830. Env: DELTAS (space-separated), NT (threads per run), PAR_RUNS (1 = run the Deltas in parallel),
# MRESP (1 default; 0 = fixed-M robustness), TAG (output prefix, default 1651). Outputs Products/<TAG>-cf-revenue-mr<MRESP>-D<Delta>.csv/.Rout
set -uo pipefail
export LC_ALL=C PATH=/usr/local/bin:$PATH
cd "$(dirname "$0")/../C-estimator"
P=../Products; NT=${NT:-3}; MRESP=${MRESP:-1}; TAG=${TAG:-1651}
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PAR n_threads=$NT cf_cold=1 cf_multi=1 cf_op=0 cf_g10=10 cf_mresp=$MRESP cf_target=revenue"
run() { local o=$P/$TAG-cf-revenue-mr$MRESP-D$1
  /usr/bin/time -p ./grid_estimator_ind5b_cf6 $B deltas=$1 output_csv=$o.csv > $o.Rout 2>&1; echo "done D=$1 exit $? $(date '+%d %H:%M') $(grep -o 'Delta .*TS_min [0-9.]*' $o.Rout | tail -1)"; }
for d in $DELTAS; do if [ "${PAR_RUNS:-1}" = 1 ]; then run $d & else run $d; fi; done
wait; echo "all done $(date '+%d %H:%M')"
