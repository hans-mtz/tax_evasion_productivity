## Stage-2 ELVIS -- finished lambda test-inversion CI (moment set A) ---------
## (2026-09-05) Finishes what 1215-stage2-profile-CI-explore.R started: that
## script's own note was "the true lower bound isn't resolved by this grid --
## almost certainly extends further left." This session extended the grid
## (coarse scan down to ~9e-9/1.8e-8, then a fine 11-point refinement, then a
## low-side check down to 5e-10) for BOTH ins choices and found a genuine,
## cross-instrument-confirmed interior minimum near lambda=1.84e-08 -- ~2+
## orders of magnitude better Lhat than anything on the original grid
## (lag_m: 2.99e-05 vs the old best 0.0108; lag_2_cal_W: 8.65e-05 vs 0.0073).
##
## This script merges every fit collected today (original grid+refine,
## coarse-extend, fine-refine2, lowcheck) with the CURRENT saved product files
## (which hold whichever run happened to save last), dedupes by lambda
## (within floating tolerance, keeping the LOWER Lhat when a lambda was fit
## more than once -- warm-start path dependence means a later, better-warm-
## started fit at the same lambda can beat an earlier one, per the lag_m
## 3.67e-08 case this session), then applies Theorem F.1's chi^2 shortcut
## exactly as 1215 set up: Q_n(lambda) = Lhat(lambda) - min(Lhat), TS =
## 2*n*Q_n, compared to chi^2_{d_g,0.95} with d_g=8 (moment set A).

library(tidyverse)

extract_fits <- function(path, ins_label) {
    if (!file.exists(path)) return(NULL)
    e <- new.env(); load(path, envir = e); fits <- e$fits
    do.call(rbind, lapply(fits, function(f) data.frame(
        ins = ins_label, phase = f$phase,
        lambda = f$lambda, Lhat = f$value, n_used = f$n, conv = f$convergence
    )))
}

## Every RData snapshot saved today for moment set A, include_zero, n_burn500/n_keep1000,
## across both ins choices -- .bak-* files preserve intermediate runs that got overwritten.
snapshot_suffixes <- c("bak-preextend", "bak-coarseextend", "bak-refine2", "bak-lowcheck", "bak-continuous", "")  # "" = current (ultralow) file
ins_choices <- c("lag_m", "lag_2_cal_W")

df <- bind_rows(lapply(ins_choices, function(ins) {
    bind_rows(lapply(snapshot_suffixes, function(suf) {
        base <- sprintf("Code/Products/1211-stage2-elvis-AB-%s-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData", ins)
        path <- if (suf == "") base else paste0(base, ".", suf)
        extract_fits(path, ins)
    }))
}))

## Dedupe by (ins, lambda) within floating tolerance, keep lowest Lhat.
df <- df %>%
    mutate(lambda_key = signif(lambda, 6)) %>%
    group_by(ins, lambda_key) %>%
    slice_min(Lhat, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-lambda_key) %>%
    arrange(ins, lambda)

d_g   <- 8
alpha <- 0.05
crit  <- qchisq(1 - alpha, df = d_g)

summary_tbl <- df %>%
    group_by(ins) %>%
    summarise(Lhat_min = min(Lhat), n = n_used[which.min(Lhat)], lambda_hat = lambda[which.min(Lhat)],
              n_points = n(), lambda_range = sprintf("%.3g - %.3g", min(lambda), max(lambda)), .groups = "drop") %>%
    mutate(dg = d_g, crit = crit, threshold = Lhat_min + crit / (2 * n))

cat("---- Consolidated grid: point estimate + 95% test-inversion CI for lambda (moment set A) ----\n")
print(as.data.frame(summary_tbl), digits = 6)

df2 <- df %>%
    left_join(summary_tbl %>% select(ins, threshold, Lhat_min, lambda_hat), by = "ins") %>%
    mutate(in_CI = Lhat <= threshold)

ci_bounds <- df2 %>%
    filter(in_CI) %>%
    group_by(ins) %>%
    summarise(lambda_lo = min(lambda), lambda_hi = max(lambda), n_in_band = n(), .groups = "drop") %>%
    left_join(summary_tbl %>% select(ins, lambda_hat), by = "ins")

cat("\n---- 95% CI for lambda (min/max of tested grid points inside the band) ----\n")
cat("NOTE: these are grid-resolution bounds (literal min/max of tested points\n")
cat("passing the test), not a continuous solve -- tighter with a finer grid\n")
cat("near the band edges, per the same logic as F_figure1.R's lbound/ubound.\n\n")
print(as.data.frame(ci_bounds), digits = 5)

## Flag whether either bound is STILL at the tested grid's own edge (i.e. the true
## bound may extend even further -- the same edge-of-grid caveat as before, now
## checked programmatically rather than eyeballed).
edge_check <- df2 %>%
    group_by(ins) %>%
    summarise(
        min_tested = min(lambda), max_tested = max(lambda),
        lo_at_edge = ci_bounds$lambda_lo[ci_bounds$ins == ins[1]] == min(lambda),
        hi_at_edge = ci_bounds$lambda_hi[ci_bounds$ins == ins[1]] == max(lambda),
        .groups = "drop"
    )
cat("\n---- Edge check: is either CI bound sitting at the tested grid's own edge? ----\n")
print(as.data.frame(edge_check))

save(df, df2, summary_tbl, ci_bounds, file = "Code/Products/1218-stage2-lambda-ci.RData")
cat("\nSaved: Code/Products/1218-stage2-lambda-ci.RData\n")
