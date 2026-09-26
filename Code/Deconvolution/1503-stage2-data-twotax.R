## PRODUCT: Code/Products/1503-stage2-data-twotax.RData := stage-2 ELVIS input on the final specification's inputs
## (plan step A3, Research-log/log.md, 2026-09-26). Same construction and columns as 1200-stage2-data.R, with:
##   - first stage: two-tax (net-of-tax) first stage, 1501-fs-net.RData (df_net: cal_V, cal_W, epsilon; beta);
##   - PF step: ONE set of estimates, joint efficient GMM (m*_{it-1} + W~_{it-2}), 1502-pf-joint-gmm-net.RData,
##     so there is no longer a lag_m / lag_2_cal_W split (ins = "joint");
##   - the detection reference scale Mbar_{j,t-1}: industry mean (Mbar) and median (Mbar_med, sensitivity) of
##     reported materials M* over ALL firms (corporations included) in year t-1 (Paper/sections/9999-detection-q.qmd;
##     check in 1500-detection-reference-scale-check.R). Missing for each industry's first year.
## h(e) uses sales_tax_rate_purchases (tau_P) only; year intercepts for ln(rho_t) are added in the estimator.
## 1200-stage2-data.RData (the current headline's input) is left untouched.
library(tidyverse)
load("Code/Products/1501-fs-net.RData")          # df_net (raw panel + net cal_V, cal_W, epsilon), fs_net_ls
load("Code/Products/1502-pf-joint-gmm-net.RData") # res: alpha_K_eff, alpha_L_eff, beta by sic_3

alpha_df <- res %>% transmute(sic_3 = as.character(sic_3), alpha_K = alpha_K_eff, alpha_L = alpha_L_eff, beta)

ref <- df_net %>%
    mutate(sic_3 = as.character(sic_3), year = as.numeric(as.character(year))) %>%
    filter(is.finite(materials), materials > 0) %>%
    group_by(sic_3, year) %>%
    summarise(Mbar = mean(materials), Mbar_med = median(materials), n_ref = n(), .groups = "drop") %>%
    mutate(year = year + 1)                       # the value from t-1 is the reference in t

stage2_data <- df_net %>%
    mutate(sic_3 = as.character(sic_3), year = as.numeric(as.character(year))) %>%
    filter(is.finite(cal_V), is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m), is.finite(y)) %>%
    left_join(alpha_df, by = "sic_3") %>%
    filter(!is.na(alpha_K)) %>%
    group_by(plant) %>% arrange(year, .by_group = TRUE) %>%
    mutate(tilde_cal_W = cal_W - alpha_K * k - alpha_L * l,
           lag_2_cal_W = dplyr::lag(cal_W, 2, order_by = year)) %>%
    ungroup() %>%
    left_join(ref, by = c("sic_3", "year")) %>%
    mutate(M_star = materials,
           corp   = juridical_organization == 3,
           ins    = "joint",
           eff_tax_rate_c = sales_tax_rate_sales - sales_tax_rate_purchases * beta,
           tau_rho_proxy  = eff_tax_rate_c) %>%
    select(sic_3, year, plant, corp,
           cal_V, cal_W, tilde_cal_W, m, M_star, k, l, y, epsilon,
           lag_k, lag_l, lag_m, lag_2_cal_W,
           beta, alpha_K, alpha_L,
           sales_tax_rate_purchases, sales_tax_rate_sales, effective_sales_tax_rate,
           eff_tax_rate_c, tau_rho_proxy,
           Mbar, Mbar_med, n_ref, ins)

cat(sprintf("stage2_data (two-tax, joint PF): %d rows | corporations %d | unincorporated %d | industries %d | years %d-%d\n",
            nrow(stage2_data), sum(stage2_data$corp), sum(!stage2_data$corp), n_distinct(stage2_data$sic_3),
            min(stage2_data$year), max(stage2_data$year)))
u <- stage2_data %>% filter(!corp, sales_tax_rate_purchases > 0)
cat(sprintf("Unincorporated with tau_P > 0: %d | with a lagged reference (not the industry's first year): %d | with lag_m: %d\n",
            nrow(u), sum(is.finite(u$Mbar)), sum(is.finite(u$Mbar) & is.finite(u$lag_m))))
cat(sprintf("M*/Mbar (mean reference), unincorporated tau_P>0: median %.2f, p90 %.2f, p99 %.2f; share > 1: %.1f%%\n",
            median(u$M_star / u$Mbar, na.rm = TRUE), quantile(u$M_star / u$Mbar, .9, na.rm = TRUE),
            quantile(u$M_star / u$Mbar, .99, na.rm = TRUE), 100 * mean(u$M_star / u$Mbar > 1, na.rm = TRUE)))
save(stage2_data, file = "Code/Products/1503-stage2-data-twotax.RData")
cat("Saved: Code/Products/1503-stage2-data-twotax.RData\n")
