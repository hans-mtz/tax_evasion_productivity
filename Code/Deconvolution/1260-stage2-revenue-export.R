## Stage-2 counterfactual -- revenue-input data export (2026-09-10) ---------
## Sibling of 1225-stage2-grid-export.R: SAME interior/corner split + trim
## (duplicated inline for the same reason 1225 does), but additionally joins
## the two raw panel columns the revenue formula needs that never made it
## into stage2_data: `sales_tax_sales` (CLAUDE.md's "t1", raw sales-tax-paid-
## on-sales) and `p_gdp` (CLAUDE.md's "p_gdp_new" deflator) from
## Code/Products/colombia_data.RData's colombia_data_frame, by (plant,year).
## Confirmed 2026-09-10: 100% match rate on the lag_m/!corp sample (32378/
## 32378), no NAs introduced. NOTE: sales_tax_rate_purchases in this pipeline
## is fed directly into h_of_e() as "tau_rho" (see grid_estimator.cpp's
## FirmData.tau_rho and CLAUDE.md's revised understanding) -- since M/M* are
## nominal COP already, tau_P*rho_t collapses to the bare rate tau_P with no
## separate price index needed, so the SAME column serves as both the FOC's
## tau_rho AND the revenue formula's tau_P; no separate rho_t column exists
## or is needed.

library(tidyverse)
source("Code/Deconvolution/utils-cli.R")

DEFAULTS <- list(ins = "lag_m", trim_top_pct = 0.005)
opt <- parse_cli_args(DEFAULTS)
opt$trim_top_pct <- as.numeric(opt$trim_top_pct)
log_run_header("1260-stage2-revenue-export.R", opt)

load("Code/Products/1200-stage2-data.RData")   # stage2_data
load("Code/Products/colombia_data.RData")       # colombia_data_frame

tax_panel <- colombia_data_frame %>%
    transmute(plant = as.character(plant), year = year,
              t1 = sales_tax_sales, pgdp = p_gdp)

base <- stage2_data %>% dplyr::filter(ins == opt$ins, !corp) %>%
    mutate(plant = as.character(plant))

interior <- base %>%
    dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0) %>%
    mutate(corner = 0L)

if (opt$trim_top_pct > 0) {
    Mstar_cutoff <- quantile(interior$M_star, 1 - opt$trim_top_pct, na.rm = TRUE)
    n_before <- nrow(interior)
    interior <- interior %>% dplyr::filter(M_star <= Mstar_cutoff)
    cat(sprintf("  [trim_top_pct=%.4g] dropped %d/%d interior firms with M_star > %.4g (top %.2g%%)\n",
                opt$trim_top_pct, n_before - nrow(interior), n_before, Mstar_cutoff, 100 * opt$trim_top_pct))
}

corner_obs <- base %>%
    dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases == 0) %>%
    mutate(corner = 1L)

run_sample <- bind_rows(interior, corner_obs) %>%
    left_join(tax_panel, by = c("plant", "year")) %>%
    mutate(row_id = dplyr::row_number())

n_na <- sum(!is.finite(run_sample$t1) | !is.finite(run_sample$pgdp))
cat(sprintf("Revenue-column join: %d/%d rows missing t1/pgdp after join\n", n_na, nrow(run_sample)))
stopifnot(n_na == 0)

run_sample <- run_sample %>%
    transmute(
        M_star = M_star,
        cal_V = cal_V,
        tilde_cal_W = tilde_cal_W,
        sales_tax_rate_purchases = sales_tax_rate_purchases,
        beta = beta,
        corner = corner,
        row_id = row_id,
        t1 = t1,
        pgdp = pgdp
    )

stopifnot(all(is.finite(as.matrix(run_sample %>% select(-row_id)))))

cat(sprintf("Sample: n=%d (%d corner, %d interior)\n",
            nrow(run_sample), sum(run_sample$corner == 1), sum(run_sample$corner == 0)))

out_file <- sprintf("Code/Products/1260-stage2-revenue-input-%s-trim%g.csv", opt$ins, opt$trim_top_pct)
write.csv(run_sample, out_file, row.names = FALSE, quote = FALSE)
cat(sprintf("Saved: %s\n", out_file))
