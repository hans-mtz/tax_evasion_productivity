## Stage-2 ELVIS -- omega and e percentile diagnostics (2026-09-01) ---------
## Requested: (i) where does omega_star=delta1/(2*delta2) sit in the ACTUAL
## tilted omega distribution (median, p75, p90, p95)? (ii) detection
## probability lambda_hat*e at e's p80/p90/p95/p99 (extends the existing
## mean/median-only check in 1213 to the upper tail). Both via the same
## post-hoc-at-fitted-(theta_hat,gamma_hat) auxiliary-parameter machinery
## already used for E[e]/Med[e] -- new exports tilted_e_omega_diag_A_cpp/
## _B_cpp (1200-stage2-elvis.cpp / -B.cpp) pool BOTH e and omega in one pass.
##
## Exploratory ("just show them to me, I might want to refine the
## percentiles") -- prints only, no plot, not yet in the slides.

library(tidyverse)
library(Rcpp)
library(RcppParallel)

sourceCpp("Code/Rcpp/1200-stage2-elvis.cpp")
Sys.setenv(PKG_LIBS = "-framework Accelerate")
sourceCpp("Code/Rcpp/1200-stage2-elvis-B.cpp")
load("Code/Products/1200-stage2-data.RData")
load("Code/Products/1207-stage2-omega-targets.RData")
RcppParallel::setThreadOptions(numThreads = 6)

## %% Shared sample-building helpers (mirrors 1211's own, kept in sync by hand) --
build_run_sample <- function(ins_choice, data = stage2_data, corner_mode = c("drop", "include_zero")) {
    corner_mode <- match.arg(corner_mode)
    base <- data %>% dplyr::filter(ins == ins_choice, !corp)
    interior <- base %>%
        dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0) %>%
        mutate(corner = 0L)
    out <- if (corner_mode == "drop") {
        interior
    } else {
        corner_obs <- base %>%
            dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases == 0) %>%
            mutate(corner = 1L)
        bind_rows(interior, corner_obs)
    }
    out %>%
        dplyr::select(plant, year, sic_3, M_star, cal_V, tilde_cal_W, sales_tax_rate_purchases, beta, corner) %>%
        dplyr::left_join(
            dplyr::filter(omega_targets, ins == ins_choice) %>% dplyr::select(sic_3, mu_omega, sigma_omega),
            by = "sic_3"
        )
}
add_industry_idx <- function(run_sample) {
    sic_levels <- sort(unique(run_sample$sic_3))
    run_sample$industry_idx <- match(run_sample$sic_3, sic_levels) - 1L
    attr(run_sample, "sic_levels") <- sic_levels
    run_sample
}

best_fit <- function(path) {
    e <- new.env(); load(path, envir = e); fits <- e$fits
    df <- do.call(rbind, lapply(fits, function(f) data.frame(
        lambda = f$lambda, Lhat = f$value,
        delta0 = f$par[1], delta1 = f$par[2], delta2 = f$par[3], eta = f$par[4],
        par = I(list(f$par))
    )))
    df[which.min(df$Lhat), ]
}

RESULT_FILES <- list(
    A = c(lag_m       = "Code/Products/1211-stage2-elvis-AB-lag_m-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData",
          lag_2_cal_W = "Code/Products/1211-stage2-elvis-AB-lag_2_cal_W-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData"),
    B = c(lag_m       = "Code/Products/1211-stage2-elvis-AB-lag_m-B-include_zero-nburn500-nkeep1000-maxevalb5000.RData",
          lag_2_cal_W = "Code/Products/1211-stage2-elvis-AB-lag_2_cal_W-B-include_zero-nburn500-nkeep1000-maxevalb5000.RData")
)

OMEGA_PROBS <- c(0.50, 0.75, 0.95)   # refined per user request (2026-09-01): drop p90
E_PROBS     <- c(0.90, 0.95, 0.99)   # refined per user request (2026-09-01): drop p80

run_one <- function(moment_set, ins_label) {
    b <- best_fit(RESULT_FILES[[moment_set]][[ins_label]])
    run_sample <- build_run_sample(ins_label, corner_mode = "include_zero") %>%
        mutate(.row_id = dplyr::row_number())

    if (moment_set == "A") {
        res <- tilted_e_omega_diag_A_cpp(
            run_sample$M_star, run_sample$cal_V, run_sample$tilde_cal_W, run_sample$sales_tax_rate_purchases,
            run_sample$.row_id, run_sample$beta,
            lambda = b$lambda, delta0 = b$delta0, delta1 = b$delta1, delta2 = b$delta2, eta = b$eta,
            gamma = b$par[[1]][-(1:4)], corner = run_sample$corner,
            n_burn = 500, n_keep = 1000, n_pool = 20
        )
    } else {
        run_sample <- add_industry_idx(run_sample)
        J <- length(attr(run_sample, "sic_levels"))
        par <- b$par[[1]]
        mu_m  <- par[5:(4 + J)]
        gamma <- par[-(1:(4 + J))]
        res <- tilted_e_omega_diag_B_cpp(
            run_sample$M_star, run_sample$cal_V, run_sample$tilde_cal_W, run_sample$sales_tax_rate_purchases,
            run_sample$beta, run_sample$mu_omega, run_sample$sigma_omega,
            run_sample$.row_id, run_sample$corner, run_sample$industry_idx,
            lambda = b$lambda, delta0 = b$delta0, delta1 = b$delta1, delta2 = b$delta2, eta = b$eta,
            gamma = gamma, mu_m = mu_m,
            n_burn = 500, n_keep = 1000, n_pool = 20
        )
    }

    e_pooled     <- as.vector(res$e_pool)
    omega_pooled <- as.vector(res$omega_pool)
    omega_star   <- b$delta1 / (2 * b$delta2)

    omega_q <- quantile(omega_pooled, probs = OMEGA_PROBS)
    e_q     <- quantile(e_pooled, probs = E_PROBS)
    p_detect_q <- b$lambda * e_q

    ## where does omega_star fall in the pooled omega distribution?
    omega_star_pctile <- mean(omega_pooled <= omega_star) * 100

    list(
        moment_set = moment_set, ins = ins_label, lambda = b$lambda,
        omega_star = omega_star, omega_star_pctile = omega_star_pctile,
        omega_q = omega_q, e_q = e_q, p_detect_q = p_detect_q
    )
}

configs <- expand.grid(moment_set = c("A", "B"), ins = c("lag_m", "lag_2_cal_W"), stringsAsFactors = FALSE)
results <- Map(run_one, configs$moment_set, configs$ins)

cat("=====================================================================\n")
cat("omega* vs. tilted omega distribution (median, p75, p90, p95)\n")
cat("=====================================================================\n")
for (r in results) {
    cat(sprintf("\n[%s / %s]  lambda_hat=%.4g\n", r$moment_set, r$ins, r$lambda))
    cat(sprintf("  omega* = delta1/(2*delta2) = %.4f  -->  sits at the %.1f-th percentile of pooled omega\n",
                r$omega_star, r$omega_star_pctile))
    cat("  omega percentiles: "); print(round(r$omega_q, 4))
}

cat("\n=====================================================================\n")
cat("Detection probability at e's p80/p90/p95/p99 (lambda_hat * e_quantile)\n")
cat("=====================================================================\n")
for (r in results) {
    cat(sprintf("\n[%s / %s]\n", r$moment_set, r$ins))
    cat("  e percentiles:        "); print(round(r$e_q, 2))
    cat("  P(detect) at each:    "); print(scales::percent(r$p_detect_q, accuracy = 0.001))
}

saveRDS(results, "Code/Products/1216-stage2-omega-e-percentiles.rds")
cat("\nSaved: Code/Products/1216-stage2-omega-e-percentiles.rds\n")
