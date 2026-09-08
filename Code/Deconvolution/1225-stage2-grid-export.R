## Stage-2 grid estimator -- data export (2026-09-07) ------------------------
## R's ONLY job in the new (lambda,delta1,delta2) grid pipeline: load
## stage2_data, apply the SAME interior/corner split + trim as
## build_run_sample() in 1211-stage2-elvis-driver-AB.R (duplicated inline
## below, not sourced -- 1211 has top-level CLI/execution code that would run
## immediately on source(), and this ~15-line filter is short enough that
## duplicating it is lower-risk than refactoring validated code to share it),
## then write only the columns moment set C actually needs to CSV. The
## standalone Code/C-estimator/grid_estimator reads that CSV, runs the whole
## grid + inner optimization in C++/NLopt/Accelerate, writes a results CSV;
## 1226-stage2-grid-plot.R reads that back. See CLAUDE.md's Estimation
## section and Research-log/log.md for the full design rationale.
##
## Moment set C never uses ln(M) or any per-industry target -- no
## industry_idx, mu_m, mu_omega, sigma_omega columns needed here at all
## (a real simplification vs. what 1211's data prep carries for sets A/B).

library(tidyverse)
source("Code/Deconvolution/utils-cli.R")

DEFAULTS <- list(ins = "lag_m", trim_top_pct = 0.005)
opt <- parse_cli_args(DEFAULTS)
opt$trim_top_pct <- as.numeric(opt$trim_top_pct)
log_run_header("1225-stage2-grid-export.R", opt)

load("Code/Products/1200-stage2-data.RData")   # stage2_data

base <- stage2_data %>% dplyr::filter(ins == opt$ins, !corp)

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
    mutate(row_id = dplyr::row_number()) %>%
    transmute(
        M_star = M_star,
        cal_V = cal_V,
        tilde_cal_W = tilde_cal_W,
        sales_tax_rate_purchases = sales_tax_rate_purchases,
        beta = beta,
        corner = corner,
        row_id = row_id
    )

stopifnot(all(is.finite(as.matrix(run_sample %>% select(-row_id)))))

cat(sprintf("Sample: n=%d (%d corner, %d interior)\n",
            nrow(run_sample), sum(run_sample$corner == 1), sum(run_sample$corner == 0)))

out_file <- sprintf("Code/Products/1225-stage2-grid-input-%s-trim%g.csv", opt$ins, opt$trim_top_pct)
write.csv(run_sample, out_file, row.names = FALSE, quote = FALSE)   # all columns numeric -- no quoting needed,
                                                                      # and the grid_estimator's CSV reader expects unquoted headers
cat(sprintf("Saved: %s\n", out_file))
