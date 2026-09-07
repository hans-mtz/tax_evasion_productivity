## Stage-2 ELVIS omega targets, per (sic_3, ins) -----------------------------
## Cheap, EXTERNALLY-COMPUTED per-industry mu_omega/sigma_omega targets for
## the new moment-set-B rows (om-mu_omega, om^2-sigma_omega, and their psi-
## interactions -- see CLAUDE.md's 2026-08-29 "confirmed gamma-counting +
## moment-set A/B" entry). These are NOT jointly-estimated nuisance
## parameters -- unlike mu_m,j (industry mean of latent ln M, which genuinely
## needs its own per-industry moment inside the BOBYQA search), omega has an
## external anchor because tilde_cal_W = omega + (1-beta)*eps is DIRECTLY
## OBSERVED for every unincorp firm too (already a column in stage2_data),
## so:
##   mu_omega_j    = E[tilde_cal_W | j] - (1-beta_j)*mu1_j
##   sigma_omega_j = Var[tilde_cal_W | j] - (1-beta_j)^2*var_j
## where mu1_j/var_j are the ALREADY-BUILT CORP eps targets
## (1206-stage2-eps-targets.RData), reused here as the "eps is
## firm-type-invariant" assumption already relied on throughout (CLAUDE.md,
## 2026-08-27 "$\tau_P=0$ corner firms" entry and others) -- and the
## sigma_omega formula additionally leans on Cov(eps,omega|j)=0 (eps perp
## omega within industry), which is one of the moment rows being tested/
## imposed elsewhere in the system, used here only to build a construction
## target, not asserted as an estimation result.
##
## Computed from the FULL unincorp sample for each ins (corp==FALSE,
## regardless of corner status -- tilde_cal_W is well-defined and observed
## regardless of tau_P), not just the "interior" (tau_P>0) subset, for
## maximum precision -- corner-vs-interior only matters for how the stage-2
## moment system CONSUMES omega, not for computing this external target.
##
## Same thin-industry pooled-fallback pattern as 1206-stage2-eps-targets.R
## (MIN_N=30), though thin industries should be rarer here since the
## unincorp sample is much larger per industry than the CORP-only sample.

library(tidyverse)

source("Code/Deconvolution/utils-cli.R")
log_run_header("1207-stage2-omega-targets.R", list(note = "no CLI args -- cheap, deterministic, both ins choices every run"))

load("Code/Products/1200-stage2-data.RData")            # stage2_data
load("Code/Products/1206-stage2-eps-targets.RData")     # eps_targets (mu1, var per sic_3, ins)

MIN_N <- 30

build_omega_targets <- function(ins_choice, data = stage2_data, eps_tgt = eps_targets) {
    eps_j <- eps_tgt %>%
        dplyr::filter(ins == ins_choice) %>%
        dplyr::select(sic_3, eps_mu1 = mu1, eps_var = var)

    unincorp <- data %>%
        dplyr::filter(ins == ins_choice, !corp, is.finite(tilde_cal_W)) %>%
        dplyr::select(sic_3, beta, tilde_cal_W)

    pooled_beta <- unincorp %>% summarise(beta_pool = mean(beta))
    pooled_eps  <- eps_j %>% summarise(mu1_pool = mean(eps_mu1), var_pool = mean(eps_var))

    pooled <- unincorp %>%
        summarise(n_pool = n(), Wbar_pool = mean(tilde_cal_W), Wvar_pool = mean((tilde_cal_W - mean(tilde_cal_W))^2)) %>%
        mutate(
            mu_omega_pool    = Wbar_pool - (1 - pooled_beta$beta_pool) * pooled_eps$mu1_pool,
            sigma_omega_pool = Wvar_pool - (1 - pooled_beta$beta_pool)^2 * pooled_eps$var_pool
        )

    by_ind <- unincorp %>%
        group_by(sic_3) %>%
        summarise(
            n     = n(),
            beta  = mean(beta),      # constant within (sic_3, ins) by construction
            Wbar  = mean(tilde_cal_W),
            Wvar  = mean((tilde_cal_W - mean(tilde_cal_W))^2),
            .groups = "drop"
        ) %>%
        dplyr::left_join(eps_j, by = "sic_3") %>%
        mutate(
            mu_omega    = Wbar - (1 - beta) * eps_mu1,
            sigma_omega = Wvar - (1 - beta)^2 * eps_var,
            pooled_fallback = n < MIN_N,
            mu_omega    = if_else(pooled_fallback, pooled$mu_omega_pool, mu_omega),
            sigma_omega = if_else(pooled_fallback, pooled$sigma_omega_pool, sigma_omega),
            ins = ins_choice
        ) %>%
        dplyr::select(ins, sic_3, n, mu_omega, sigma_omega, pooled_fallback)

    cat(sprintf("  [%s] %d industries, %d fell back to pooled (n<%d): %s\n",
                ins_choice, nrow(by_ind), sum(by_ind$pooled_fallback), MIN_N,
                paste(by_ind$sic_3[by_ind$pooled_fallback], collapse = ", ")))
    cat(sprintf("  [%s] sigma_omega range: [%.3f, %.3f] (n_neg=%d -- negative would signal Cov(eps,omega|j) too far from 0 for the construction to make sense)\n",
                ins_choice, min(by_ind$sigma_omega), max(by_ind$sigma_omega), sum(by_ind$sigma_omega <= 0)))

    by_ind
}

omega_targets <- bind_rows(
    build_omega_targets("lag_m"),
    build_omega_targets("lag_2_cal_W")
)

cat(sprintf("Total omega_targets rows: %d\n", nrow(omega_targets)))
save(omega_targets, file = "Code/Products/1207-stage2-omega-targets.RData")
cat("Saved: Code/Products/1207-stage2-omega-targets.RData\n")
