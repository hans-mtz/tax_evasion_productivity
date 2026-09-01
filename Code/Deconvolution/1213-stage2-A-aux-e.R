## Stage-2 ELVIS, moment set A -- auxiliary parameters E[e], Med[e] ---------
## (2026-08-31) Post-hoc auxiliary-parameter evaluation at the ALREADY-FITTED
## (theta_hat, gamma_hat) from 1211's saved results -- no re-optimization.
## Corrects the earlier plot/table's crude "E[V]=E[u]-E[eps], plug in at the
## mean" check (1212-stage2-A-prelim-plot.R): u=ln((M+e)/M) is nonlinear in
## e, so that decomposition never gave a clean e at all. This is Schennach's
## own E[U] device (her auxiliary-parameter worked example) applied to e(M)
## directly, via a new diagnostic export tilted_e_diag_A_cpp (1200-stage2-
## elvis.cpp) that reuses the EXACT SAME per-firm Metropolis chain as the
## live estimator (mh_tilted_average_A_cpp), just also tracking e per draw.
##
## Two auxiliary parameters, NOT interchangeable (see the .cpp file's header
## comment and CLAUDE.md/session record for the full derivation):
##   mu_e:    E[e]-mu_e=0            -- linear, a running per-firm mean, free
##   theta_e: E[1(e<=theta_e)-0.5]=0 -- indicator/quantile-GMM moment; the
##            literal "Med[e]-theta_e=0" is NOT itself a valid moment (no
##            expectation of a median) -- practically, this means pooling
##            thinned post-burn-in e draws ACROSS ALL FIRMS and taking the
##            empirical median of the pooled sample (an aggregate, population-
##            level median, matching build_lambda_grid's own "median firm"
##            framing -- a population quantile of M*, not a per-firm
##            conditional median).

library(tidyverse)
library(Rcpp)
library(RcppParallel)

sourceCpp("Code/Rcpp/1200-stage2-elvis.cpp")
load("Code/Products/1200-stage2-data.RData")   # stage2_data

RcppParallel::setThreadOptions(numThreads = 6)

RESULT_FILES <- c(
    lag_m       = "Code/Products/1211-stage2-elvis-AB-lag_m-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData",
    lag_2_cal_W = "Code/Products/1211-stage2-elvis-AB-lag_2_cal_W-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData"
)

best_fit <- function(path) {
    e <- new.env(); load(path, envir = e); fits <- e$fits
    df <- do.call(rbind, lapply(fits, function(f) data.frame(
        lambda = f$lambda, Lhat = f$value,
        delta0 = f$par[1], delta1 = f$par[2], delta2 = f$par[3], eta = f$par[4],
        gamma = I(list(f$par[-(1:4)]))
    )))
    df[which.min(df$Lhat), ]
}

aux_e_row <- function(ins_label) {
    b <- best_fit(RESULT_FILES[[ins_label]])
    run_sample <- stage2_data %>%
        dplyr::filter(ins == ins_label, !corp) %>%
        mutate(.row_id = dplyr::row_number())
    interior <- dplyr::filter(run_sample, is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0) %>%
        mutate(corner = 0L)
    corner_obs <- dplyr::filter(run_sample, is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases == 0) %>%
        mutate(corner = 1L)
    dat <- bind_rows(interior, corner_obs)

    res <- tilted_e_diag_A_cpp(
        dat$M_star, dat$cal_V, dat$tilde_cal_W, dat$sales_tax_rate_purchases,
        dat$.row_id, dat$beta,
        lambda = b$lambda, delta0 = b$delta0, delta1 = b$delta1, delta2 = b$delta2, eta = b$eta,
        gamma = b$gamma[[1]], corner = dat$corner,
        n_burn = 500, n_keep = 1000, n_pool = 20
    )

    mu_e_hat    <- mean(res$e_mean)
    pooled      <- as.vector(res$e_pool)
    theta_e_hat <- median(pooled)

    data.frame(
        ins = ins_label, lambda = b$lambda, mu_e_hat = mu_e_hat, theta_e_hat = theta_e_hat,
        p_at_mean_e   = b$lambda * mu_e_hat,
        p_at_median_e = b$lambda * theta_e_hat,
        n = nrow(dat), n_corner = sum(dat$corner)
    )
}

out <- do.call(rbind, lapply(names(RESULT_FILES), aux_e_row))
cat("---- Auxiliary parameters: E[e] and Med[e] (moment set A, at fitted theta_hat/gamma_hat) ----\n")
print(out)

saveRDS(out, "Code/Products/1213-stage2-A-aux-e.rds")
cat("\nSaved: Code/Products/1213-stage2-A-aux-e.rds\n")
