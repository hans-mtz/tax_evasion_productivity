cd "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Code/C-estimator"; P=../Products; S=$P/tmp-checks
IN=$P/1598-stage2-input-designA-interior-plant-k-trim0.005.csv; D=$(sed -n 's/rho_D=//p' $P/1597-rhoD.txt)
PAR0=$(Rscript -e "r <- read.csv('$P/1594-nm-2pass.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('kappa_hat','delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,13))), sep=',')" 2>/dev/null)
PAR022=$(Rscript -e "r <- read.csv('$P/1594-nm-2pass.csv'); cat(sprintf('%.15g', c(unlist(r[1, c('kappa_hat','delta0_hat','delta1_hat','delta2_hat')]), 0.75, 0.3, r\$kappa_hat, rep(0,22))), sep=',')" 2>/dev/null)
B="qform=power_nokink k_fixed=0.75 row6=eps_psi input_csv=$IN n_burn=0 n_keep=1000 base_seed=20260830 output_csv=/dev/null"
# D for the audit row (row 10) and the industry eps rows (13-21), at the ladder's start point (same as 1597-rhoD)
D10=$(./grid_estimator_s2 mode=rhoD $B drop_rows=12,7,5 audit_p=0.53 par=$PAR0 | sed -n 's/RHO_D: //p' | cut -d, -f11)
DI=$(./grid_estimator_ind5b mode=rhoD $B drop_rows=1,12,7,5 ind_rows=eps par=$PAR022 | sed -n 's/RHO_D: //p' | cut -d, -f14-22)
DA=$(echo $D | awk -F, -v d=$D10 'BEGIN{OFS=","}{$11=d; print}')        # 13 values: ladder D with row 10 from the audit run
DB="$DA,$DI"                                                              # 22 values for the IND5 build
echo "rho_D=$DA" > $P/1599-rhoD-audit.txt; echo "rho_D=$DB" > $P/1599-rhoD-ind.txt
echo "D10=$D10"; echo "D ind rows: $DI"
R5=$P/1597-drop12-7-5-nocorner.csv
P13=$(Rscript -e "r <- read.csv('$R5'); cat(sprintf('%.15g', unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))])), sep=',')" 2>/dev/null)
P22=$(Rscript -e "r <- read.csv('$R5'); cat(sprintf('%.15g', c(unlist(r[1, c('lambda','delta0_hat','delta1_hat','delta2_hat','k_hat','s_hat','kappa_hat', paste0('gamma',1:13))]), rep(0,9))), sep=',')" 2>/dev/null)
C="$B rho=prop21 sampler=is proposal=mix cluster=plant n_threads=4"
./grid_estimator_s2 mode=adiag $C rho_D=$DA drop_rows=12,7,5 par=$P13 audit_p=0.53 > $S/c.txt 2>&1 &
./grid_estimator_s2 mode=adiag $C rho_D=$DA drop_rows=12,7,5 par=$P13 audit_p=0.53 audit_group=v > $S/cv.txt 2>&1 &
./grid_estimator_ind5b mode=adiag $C rho_D=$DB drop_rows=12,7,5 ind_rows=eps par=$P22 > $S/b1.txt 2>&1 &
wait
echo "(c) audit, group K:"; sed -n '/^row/,/^gamma/p' $S/c.txt | LC_ALL=C awk '$1==10 {printf "  row 10 mean %.5f t %.1f\n",$2,$4}'
echo "(c) audit, group V:"; sed -n '/^row/,/^gamma/p' $S/cv.txt | LC_ALL=C awk '$1==10 {printf "  row 10 mean %.5f t %.1f\n",$2,$4}'
echo "(b) row 1 vs sum of industry eps rows x shares (row 1 live here, rows 13-21 live):"
sed -n '/^row/,/^gamma/p' $S/b1.txt | LC_ALL=C awk 'NR>1 && $1!~/gamma/ {m[$1]=$2} END{s=0; for(j=13;j<=21;j++) s+=m[j]; printf "  row1 mean %.6f | sum of rows 13-21 means %.6f\n", m[1], s}'
