## Stage-2 ELVIS epsilon targets, per (sic_3, ins) -------------------------
## Builds the fixed external targets (mu1, var, mu3 -- central moments) used
## by the new stage-2 moment rows that check whether unincorp firms'
## simulated eps(M) matches what's already known about eps from CORP firms
## (moments 8-10, 11, 15 in 1200-stage2-elvis.cpp -- see CLAUDE.md's
## 2026-08-28 "Stage-2 moment system, finalized" entry).
##
## Why per-industry, not pooled: eps's mean is EXACTLY zero within every
## sic_3 (to floating-point precision) because first_stage_panel/
## first_stage_panel_me (021-deconv-funs.R) already fit beta separately per
## industry, so the CORP residual is forced to zero mean by that regression's
## own normal equations -- but variance is NOT homogeneous across industries:
## per-industry sd ranges 0.17-0.63 (pooled sd 0.42), Bartlett's test on the
## 27 industries with n>=30 gives K^2=920, p<2.2e-16. Pooling would misspecify
## the variance/skewness rows for most industries, not just lose precision.
##
## Reuses the SAME per-industry computation already active in
## first_stage_panel/first_stage_panel_me (021-deconv-funs.R, feeding
## Code/Products/931.1-fs-se-het.RData -> stage2_data$epsilon) -- spot-checked
## against a leftover fs_list snapshot (Code/Products/np-deconv-funs.RData,
## sic_3=331: epsilon_mu=6.41e-18, epsilon_sigma=0.416) and it matches exactly.
## Does NOT use 030-np-deconv-funs.R's np_pdf()/get_stats() kernel-density
## machinery -- that's for recovering f_e/f_u by deconvolution from the
## POOLED sample; CORP epsilon is directly observed (e=0 exactly for CORP, so
## eps=-cal_V exactly, no deconvolution error to smooth over), so plain
## sample moments are the right (and simplest) target. Also does NOT reuse
## the ecdf-based per-industry code in 235-tims-test.R/_600-CD-GRN-eps.R --
## those are exploratory/abandoned, not part of the active pipeline (checked
## before writing this, per the project's "check existing code first" habit).
##
## Thin-industry fallback: sic_3 with fewer than MIN_N CORP obs (for a given
## ins) fall back to the ins-level pooled target instead of their own
## (unreliable) sample moments -- only 2 of 29 industries hit this (sic 354,
## n=22; sic 353, n=2), everything else has n>=54.

library(tidyverse)

source("Code/Deconvolution/utils-cli.R")
log_run_header("1206-stage2-eps-targets.R", list(note = "no CLI args -- cheap, deterministic, both ins choices every run"))

load("Code/Products/1200-stage2-data.RData")   # stage2_data

MIN_N <- 30

build_eps_targets <- function(ins_choice, data = stage2_data) {
    corp_eps <- data %>%
        dplyr::filter(ins == ins_choice, corp, is.finite(epsilon)) %>%
        dplyr::select(sic_3, epsilon)

    pooled <- corp_eps %>%
        summarise(
            n_pool   = n(),
            mu1_pool = mean(epsilon),
            var_pool = mean((epsilon - mean(epsilon))^2),
            mu3_pool = mean((epsilon - mean(epsilon))^3)
        )

    by_ind <- corp_eps %>%
        group_by(sic_3) %>%
        summarise(
            n   = n(),
            mu1 = mean(epsilon),
            var = mean((epsilon - mean(epsilon))^2),
            mu3 = mean((epsilon - mean(epsilon))^3),
            .groups = "drop"
        ) %>%
        mutate(
            pooled_fallback = n < MIN_N,
            mu1 = if_else(pooled_fallback, pooled$mu1_pool, mu1),
            var = if_else(pooled_fallback, pooled$var_pool, var),
            mu3 = if_else(pooled_fallback, pooled$mu3_pool, mu3),
            ins = ins_choice
        ) %>%
        dplyr::select(ins, sic_3, n, mu1, var, mu3, pooled_fallback)

    cat(sprintf("  [%s] %d industries, %d fell back to pooled (n<%d): %s\n",
                ins_choice, nrow(by_ind), sum(by_ind$pooled_fallback), MIN_N,
                paste(by_ind$sic_3[by_ind$pooled_fallback], collapse = ", ")))

    by_ind
}

eps_targets <- bind_rows(
    build_eps_targets("lag_m"),
    build_eps_targets("lag_2_cal_W")
)

cat(sprintf("Total eps_targets rows: %d\n", nrow(eps_targets)))
save(eps_targets, file = "Code/Products/1206-stage2-eps-targets.RData")
cat("Saved: Code/Products/1206-stage2-eps-targets.RData\n")
