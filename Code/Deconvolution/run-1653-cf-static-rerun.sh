#!/bin/bash
# Static counterfactual targets rerun with the audited solver (Hans, 2026-10-06): 1644 (gap, true_credit) and 1645 (loss_t1 interior,
# (A), (B)) were run with cold single starts, which left gamma10 at 0 (audit 2026-10-05). Binary grid_estimator_ind5b_cf6, cf_multi=1 with
# the grid's 3 starts (cf_op=0 cf_g10=10), otherwise the 1644/1645 configuration. Static targets (Delta = 0, r = 1) do not depend on cf_mresp.
set -uo pipefail
export LC_ALL=C
cd "$(dirname "$0")/../C-estimator"
P=../Products; NT=${NT:-2}
PAR=$(Rscript -e "r <- read.csv('$P/1616-i-k0.75-kappa0.5.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
DI=$(sed -n 's/rho_D=//p' $P/1599-rhoD-ind.txt)
B="mode=cfprofile qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv n_burn=0 rho=prop21 rho_D=$DI sampler=is proposal=mix cluster=plant base_seed=20260830 drop_rows=1,12,7,5 ind_rows=eps n_keep=1000 par=$PAR n_threads=$NT cf_cold=1 cf_multi=1 cf_op=0 cf_g10=10 deltas=0"
run() { local o=$P/1653-cf-$1; shift
  /usr/bin/time -p ./grid_estimator_ind5b_cf6 $B "$@" output_csv=$o.csv > $o.Rout 2>&1; echo "done $(basename $o) exit $? $(date '+%d %H:%M')"; }
run gap cf_target=gap &
run true-credit cf_target=true_credit &
run loss-t1-int cf_target=loss_t1 cf_t1_extra=0 &
run loss-t1-A cf_target=loss_t1 cf_t1_extra=130.86169294605807 &
run loss-t1-B cf_target=loss_t1 cf_t1_extra=2031.1718838174272 &
wait; echo "all done $(date '+%d %H:%M')"
