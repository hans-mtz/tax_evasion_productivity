## Stage-2 ELVIS data prep -----------------------------------------------
## Joins the per-firm-period first-stage residuals (cal_V, cal_W, epsilon,
## from 930.1-fs-se-het.R) with the industry-level production-function
## elasticities (alpha_K, alpha_L, from 1100-MSL-opt-tax.R), the tax-rate
## variables and the corp/unincorp flag (both already in the raw wrangled
## panel) into one table ready for the stage-2 evasion-FOC moment system.
##
## Base is `wdf` from 1100-MSL-opt-tax.R generalized in two ways:
##   1) `df` (the full raw panel + cal_V/cal_W/epsilon) is used instead of
##      fs_all_ls[[x]]$data, since the latter drops juridical_organization
##      and the tax-rate columns.
##   2) alpha_K/alpha_L is built for BOTH instrument choices (pf_list:
##      lag_m; pf_list_l2w: lag_2_cal_W) instead of hardcoding lag_m.

library(tidyverse)

load("Code/Products/931.1-fs-se-het.RData")   # df: full raw panel + cal_V, cal_W, epsilon
load("Code/Products/1100-MSL-opttax.RData")   # pf_list (lag_m), pf_list_l2w (lag_2_cal_W)

## %% Build stage2_data for one (alpha_K, alpha_L) instrument choice ------

build_stage2_data <- function(pf_list_choice, ins_label, base_df) {
    alpha_df <- lapply(
        names(pf_list_choice),
        \(x) data.frame(
            sic_3   = x,
            alpha_K = pf_list_choice[[x]]$coeffs[["k"]],
            alpha_L = pf_list_choice[[x]]$coeffs[["l"]],
            beta    = pf_list_choice[[x]]$coeffs[["m"]]
        )
    ) |> bind_rows()

    base_df %>%
        mutate(
            sic_3 = as.character(sic_3),
            year  = as.numeric(as.character(year))
        ) %>%
        dplyr::filter(
            is.finite(cal_V), is.finite(cal_W),
            is.finite(k), is.finite(l), is.finite(m), is.finite(y)
        ) %>%
        left_join(alpha_df, by = "sic_3") %>%
        dplyr::filter(!is.na(alpha_K)) %>%           # drop industries pf_list_choice doesn't cover
        group_by(plant) %>%
        arrange(year, .by_group = TRUE) %>%
        mutate(
            tilde_cal_W = cal_W - alpha_K * k - alpha_L * l,
            lag_2_cal_W = dplyr::lag(cal_W, 2, order_by = year)
        ) %>%
        ungroup() %>%
        mutate(
            M_star = materials,                       # level; m = log(materials) already
            corp   = juridical_organization == 3,
            ins    = ins_label,
            # corrected effective sales tax rate, tau_tilde_C,it = tau_S,it - tau_P,it * beta
            # (600-opt-tax.qmd, "What about tau?"; matches eff_tax_rate_c in 1000-opt-tax.R)
            eff_tax_rate_c = sales_tax_rate_sales - sales_tax_rate_purchases * beta,
            tau_rho_proxy  = eff_tax_rate_c           # designated tau*rho_t proxy -- see note below
        ) %>%
        dplyr::select(
            sic_3, year, plant, corp,
            cal_V, cal_W, tilde_cal_W, m, M_star, k, l, y, epsilon,
            lag_k, lag_l, lag_m, lag_2_cal_W,
            beta, alpha_K, alpha_L,
            sales_tax_rate_purchases, sales_tax_rate_sales, effective_sales_tax_rate,
            eff_tax_rate_c, tau_rho_proxy,
            ins
        )
}

stage2_data_lag_m <- build_stage2_data(pf_list,     "lag_m",       df)
stage2_data_lag2W <- build_stage2_data(pf_list_l2w, "lag_2_cal_W", df)

stage2_data <- bind_rows(stage2_data_lag_m, stage2_data_lag2W)

## %% NOTE -- tau*rho_t proxy decision (2026-08-25, SUPERSEDED 2026-08-27) --
## `tau_rho_proxy` = eff_tax_rate_c = sales_tax_rate_sales - sales_tax_rate_purchases*beta
## was originally chosen over the raw sales_tax_rate_purchases from a sign
## argument in 1000-opt-tax.R's bivariate regressions of cal_V on each
## candidate tau. Per the two-tax-rate re-derivation in
## Paper/sections/9999-tax-wedge.qmd (and CLAUDE.md's "Follow-up sign checks"
## entry), that sign argument is unreliable in both directions (mechanical
## M*_it contamination on both the regressor and cal_V, plus a possible
## enforcement-intensity confound) -- so it no longer justifies the choice.
## The theoretically correct h(e) input is `sales_tax_rate_purchases` (tau_P)
## alone: the evasion FOC only ever contains the purchases-side rate, never
## tau_S. 1205-stage2-warmstart.R and 1210-stage2-elvis-driver.R now use
## `sales_tax_rate_purchases` directly for h(e); `tau_rho_proxy`/`eff_tax_rate_c`
## are kept here only as columns for robustness comparisons, not as the
## estimation input. First-stage estimates (beta, cal_V, cal_W, epsilon,
## from 931.1-fs-se-het.RData) are left untouched -- the log_mats_share_net
## fix to `first_stage_panel`/`first_stage_panel_me` (9999-tax-wedge.qmd's
## item (a)) is deliberately not applied yet.
##
## Note this is a RATE (dimensionless share), not a price level -- h(e) only
## ever uses ln(tau_P*rho_t), so as long as tau_P*rho_t is ~constant within a
## sector-time cell this enters psi purely as an additive sector-time shifter,
## collinear with delta_0 if delta_0 is allowed to vary by sector-time. For
## the first stage-2 build, delta_0 stays a plain scalar intercept (no FE);
## whether it needs to be split into industry/time (or industry x time) FE is
## revisited once we see whether lambda is identified/well-behaved as-is.

cat("stage2_data rows by ins:\n")
print(table(stage2_data$ins))
cat("stage2_data rows by corp:\n")
print(table(stage2_data$corp, stage2_data$ins))

save(stage2_data, file = "Code/Products/1200-stage2-data.RData")
