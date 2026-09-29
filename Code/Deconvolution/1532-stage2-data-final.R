## PRODUCT: Code/Products/1532-stage2-data-final.RData (stage2_final) and the grid_estimator inputs
##   Code/Products/1532-stage2-input-<design>-trim0.005.csv, design in {allcorr, designA}
## := stage-2 ELVIS input on the final specification (Thesis/PLAN.md §9b, P2). 1503 is left untouched.
##   - corrected estimates (first stage 1501 from corporations; PF 1517, single instrument W~_{it-2}) and uncorrected
##     ones (1531: beta pooled over all firms, same PF step), both kept per row;
##   - evader = the 9 industries where the headline test rejects (sharp, 5%, net share; 1510);
##   - design "allcorr": every industry corrected, corner = tau_P == 0 (the old design; ladder step S1);
##   - design "designA": corrected estimates and interior treatment only in the 9; every firm elsewhere is a corner
##     firm with the uncorrected estimates (ladder step S2);
##   - Mbar = industry mean of reported M* (real, all firms incl. corporations) in t-1; for each industry's first year
##     (1981), the leave-one-out same-year mean, flagged by Mbar_loo (decided 2026-09-28; robustness: drop 1981);
##   - units: M_star is REAL (materials = nom_mats/p_gdp); t1 = sales_tax_sales is NOMINAL; pgdp = p_gdp.
## Trim: top 0.5% of INTERIOR firms by M_star, within each design (as 1260).
library(tidyverse)
load("Code/Products/1501-fs-net.RData")                     # df_net (corrected cal_V, cal_W, epsilon)
load("Code/Products/1517-pf-systems-all-industries.RData")  # res (corrected PF)
load("Code/Products/1531-fs-pf-pooled.RData")               # df_pool, res_pool (uncorrected)
evaders <- c("313", "321", "322", "324", "331", "342", "351", "352", "369")
trim_top_pct <- 0.005

pf_c <- res %>% filter(system == "lag_2_w_eps") %>% transmute(sic_3 = as.character(sic_3), aK_c = alpha_K, aL_c = alpha_L, beta_c = beta)
pf_u <- res_pool %>% transmute(sic_3 = as.character(sic_3), aK_u = alpha_K, aL_u = alpha_L, beta_u = beta)
key <- c("sic_3", "year", "plant")
std <- \(d) d %>% mutate(sic_3 = as.character(sic_3), year = as.numeric(as.character(year)))

## detection reference scale: lagged industry mean; leave-one-out same-year mean in each industry's first year
ref_all <- std(df_net) %>% filter(is.finite(materials), materials > 0)
ref_lag <- ref_all %>% group_by(sic_3, year) %>% summarise(Mbar = mean(materials), Mbar_med = median(materials), n_ref = n(), .groups = "drop") %>%
    mutate(year = year + 1)
first_yr <- ref_all %>% group_by(sic_3) %>% summarise(y0 = min(year))
loo <- ref_all %>% semi_join(first_yr, by = c("sic_3", "year" = "y0")) %>% group_by(sic_3, year) %>%
    mutate(Mbar_l = (sum(materials) - materials) / (n() - 1), n_l = n() - 1L) %>% ungroup() %>%
    select(all_of(key), Mbar_l, n_l)

stage2_final <- std(df_net) %>%
    select(all_of(key), juridical_organization, materials, k, l, m, y, p_gdp, sales_tax_sales,
           sales_tax_rate_purchases, sales_tax_rate_sales, cal_V_c = cal_V, cal_W_c = cal_W, eps_c = epsilon) %>%
    left_join(std(df_pool) %>% select(all_of(key), cal_V_u = cal_V, cal_W_u = cal_W, eps_u = epsilon), by = key) %>%
    left_join(pf_c, by = "sic_3") %>% left_join(pf_u, by = "sic_3") %>%
    left_join(ref_lag, by = c("sic_3", "year")) %>% left_join(loo, by = key) %>%
    mutate(Mbar_loo = is.na(Mbar) & !is.na(Mbar_l), Mbar = coalesce(Mbar, Mbar_l), n_ref = coalesce(n_ref, n_l)) %>%
    select(-Mbar_l, -n_l) %>%
    mutate(corp = juridical_organization == 3, evader = sic_3 %in% evaders, M_star = materials,
           tilde_cal_W_c = cal_W_c - aK_c * k - aL_c * l, tilde_cal_W_u = cal_W_u - aK_u * k - aL_u * l,
           t1 = sales_tax_sales, pgdp = p_gdp)

cat(sprintf("stage2_final: %d rows | industries %d (corrected PF %d, uncorrected PF %d) | Mbar missing %d | LOO rows %d\n",
            nrow(stage2_final), n_distinct(stage2_final$sic_3), sum(!is.na(pf_c$aK_c)), sum(!is.na(pf_u$aK_u)),
            sum(!is.finite(stage2_final$Mbar)), sum(stage2_final$Mbar_loo)))
save(stage2_final, evaders, file = "Code/Products/1532-stage2-data-final.RData")

## grid_estimator inputs -------------------------------------------------------------------------------------------
export <- function(design) {
    b <- stage2_final %>% filter(!corp) %>%
        mutate(use_c = design == "allcorr" | evader,
               cal_V = if_else(use_c, cal_V_c, cal_V_u), tilde_cal_W = if_else(use_c, tilde_cal_W_c, tilde_cal_W_u),
               beta = if_else(use_c, beta_c, beta_u),
               corner = as.integer(sales_tax_rate_purchases == 0 | !use_c)) %>%
        filter(is.finite(cal_V), is.finite(tilde_cal_W), is.finite(beta), is.finite(sales_tax_rate_purchases),
               sales_tax_rate_purchases >= 0, is.finite(M_star), M_star > 0, is.finite(t1), is.finite(pgdp))
    cut <- quantile(b$M_star[b$corner == 0], 1 - trim_top_pct)
    n0 <- sum(b$corner == 0)
    b <- b %>% filter(corner == 1 | M_star <= cut) %>% mutate(row_id = row_number())
    cat(sprintf("[%s] trim: dropped %d of %d interior (M* > %.4g) | n = %d: interior %d, corner %d (tau_P=0 %d, not corrected %d) | Mbar missing (interior) %d\n",
                design, n0 - sum(b$corner == 0), n0, cut, nrow(b), sum(b$corner == 0), sum(b$corner == 1),
                sum(b$sales_tax_rate_purchases == 0), sum(b$corner == 1 & b$sales_tax_rate_purchases > 0),
                sum(b$corner == 0 & !is.finite(b$Mbar))))
    out <- b %>% transmute(M_star, cal_V, tilde_cal_W, sales_tax_rate_purchases, beta, corner, row_id, t1, pgdp,
                           Mbar, year, sic_3, Mbar_loo = as.integer(Mbar_loo))
    stopifnot(all(is.finite(as.matrix(out %>% filter(corner == 0) %>% select(-sic_3)))))
    f <- sprintf("Code/Products/1532-stage2-input-%s-trim%g.csv", design, trim_top_pct)
    write.csv(out, f, row.names = FALSE, quote = FALSE); cat("Saved:", f, "\n")
    invisible(b)
}
a <- export("allcorr"); d <- export("designA")

## interior firms by industry group, and mean V (design A should have no negative-mean industry in the interior)
print(d %>% filter(corner == 0) %>% group_by(sic_3) %>% summarise(n = n(), mean_V = round(mean(cal_V), 3)) %>% as.data.frame())
