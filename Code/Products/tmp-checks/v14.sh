cd "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Code/C-estimator"; P=../Products; S=$P/tmp-checks
IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv
# (a) IND rows default mode reproduces 1588 k=0.5 (old settings)
P22=$(Rscript -e "r <- read.csv('$P/1588-ind5-k0.5-s30.csv'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:22))])), sep=',')" 2>/dev/null)
./grid_estimator_ind5b mode=adiag rho=uniform seed=add qform=power_kink k_fixed=0.5 row6=eps_psi drop_rows=5,6,7,12 input_csv=$P/1585-stage2-input-designA-interior-trim0.005.csv n_burn=1000 n_keep=1000 par=$P22 n_threads=4 base_seed=20260830 output_csv=/dev/null > $S/a.txt 2>&1 &
# rung-5 point for (b), (c)
R5=$P/1597-drop12-7-5-nocorner.csv; D=$(sed -n 's/rho_D=//p' $P/1597-rhoD.txt)
P13=$(Rscript -e "r <- read.csv('$R5'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null)
C="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=0 n_keep=1000 rho=prop21 rho_D=$D sampler=is proposal=mix cluster=plant base_seed=20260830 output_csv=/dev/null n_threads=4"
# (c) audit row at the rung-5 point: row 10 live with audit_p=0.53 (s2 build)
./grid_estimator_s2 mode=adiag $C drop_rows=12,7,5 par=$P13 audit_p=0.53 > $S/c.txt 2>&1 &
wait
echo "(a) expect 201.622:"; grep "TS = " $S/a.txt
echo "(c) audit row (row 10) at rung-5 point:"; sed -n '/^row/,/^gamma/p' $S/c.txt | LC_ALL=C awk '$1==10 {printf "row 10 mean %.5f se %.5f t %.1f\n",$2,$3,$4}'; grep "audit moment\|TS = " $S/c.txt
