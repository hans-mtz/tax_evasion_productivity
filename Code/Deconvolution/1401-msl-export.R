## MSL real-data export (2026-09-18) ------------------------------------------
## Builds the CSV read by Code/C-estimator/msl_estimator (columns: V, Wt, Mst,
## taur, beta, mu_eps, sd_eps, mu_om, sd_om, + sic_3, year, row_id for merging).
## Sample = SAME interior firms as the ELVIS moment-set-A fit (stage2_data,
## unincorporated, tau_P>0, top trim_top_pct by M_star dropped -- filter
## duplicated from 1225-stage2-grid-export.R, not sourced). Corner (tau_P=0)
## firms are theta-free constants in the MSL likelihood (see
## Paper/sections/9999-msl-implementation.qmd) and are not exported.
##
## f_eps: normal(mu1, sqrt(var)) per industry from 1206's eps_targets (CORP
## residuals), including its pooled_fallback rule for thin industries.
## f_omega (Version A, first pass): normal, moment-deconvolved from W_tilde on
## ALL firms in the industry (corp and unincorp -- E[eta|omega_lag]=0 holds for
## both, so one f_omega): mu_om = mean(Wt) - (1-beta)*mu_eps,
## var_om = var(Wt) - (1-beta)^2 * var_eps  (floored). Thin industries use the
## instrument-level pooled values. The tabulated logspline f_omega replaces
## this in the next iteration.
##
## Usage: Rscript Code/Deconvolution/1401-msl-export.R ins=lag_m trim_top_pct=0.005

library(tidyverse)
source("Code/Deconvolution/utils-cli.R")
DEFAULTS <- list(ins = "lag_m", trim_top_pct = 0.005, fomega = "A")   # fomega: A | Cf (k) | CfKL (k and l)
opt <- parse_cli_args(DEFAULTS)
opt$trim_top_pct <- as.numeric(opt$trim_top_pct)
log_run_header("1401-msl-export.R", opt)

load("Code/Products/1200-stage2-data.RData")          # stage2_data
load("Code/Products/1206-stage2-eps-targets.RData")   # eps_targets

et <- eps_targets %>% dplyr::filter(ins == opt$ins) %>%
    transmute(sic_3, n_corp = n, mu_eps = mu1, sd_eps = sqrt(var), pooled_fallback)

base <- stage2_data %>% dplyr::filter(ins == opt$ins)

## f_omega moments from ALL firms per industry (need finite Wt)
allw <- base %>% dplyr::filter(is.finite(tilde_cal_W), is.finite(beta)) %>%
    left_join(et, by = "sic_3")
om_ind <- allw %>% group_by(sic_3) %>%
    summarise(n_all = n(), m_w = mean(tilde_cal_W), v_w = var(tilde_cal_W),
              bo = 1 - mean(beta), mu_eps = first(mu_eps), sd_eps = first(sd_eps),
              pooled_fallback = first(pooled_fallback), .groups = "drop") %>%
    mutate(mu_om = m_w - bo * mu_eps,
           var_om = pmax(v_w - bo^2 * sd_eps^2, 0.05 * v_w))
pool_row <- allw %>% summarise(m_w = mean(tilde_cal_W), v_w = var(tilde_cal_W), bo = 1 - mean(beta),
                               mu_eps = mean(et$mu_eps), sd_eps = sqrt(mean(et$sd_eps^2))) %>%
    mutate(mu_om = m_w - bo * mu_eps, var_om = pmax(v_w - bo^2 * sd_eps^2, 0.05 * v_w))
om_ind <- om_ind %>%
    mutate(mu_om = ifelse(pooled_fallback, pool_row$mu_om, mu_om),
           sd_om = sqrt(ifelse(pooled_fallback, pool_row$var_om, var_om)))

interior <- base %>%
    dplyr::filter(!corp, is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0)
if (opt$trim_top_pct > 0) {
    cut <- quantile(interior$M_star, 1 - opt$trim_top_pct, na.rm = TRUE)
    nb <- nrow(interior); interior <- interior %>% dplyr::filter(M_star <= cut)
    cat(sprintf("[trim %.4g] dropped %d/%d interior firms with M_star > %.4g\n", opt$trim_top_pct, nb - nrow(interior), nb, cut))
}

## Version Cf (2026-09-19): f_omega(omega_t | W_{t-1}, k_{t-1}, k_t [, l_{t-1}, l_t]) Gaussian, FEASIBLE. eps_t is independent of the
## conditioning variables, so regress W_t on them (industry fixed effects, pooled slopes, ALL firms) -> omega_t's projection;
## var(omega|X) by industry = var(resid) - bo^2 var_eps (floored). Rows without a consecutive-year lag are dropped (they have no W_{t-1}).
if (opt$fomega != "A") {
    lagd <- base %>% dplyr::filter(is.finite(tilde_cal_W)) %>% transmute(plant, year = year + 1, Wl = tilde_cal_W, kl = k, ll = l)
    aw <- base %>% dplyr::filter(is.finite(tilde_cal_W), is.finite(beta)) %>% left_join(lagd, by = c("plant", "year")) %>%
        dplyr::filter(is.finite(Wl), is.finite(kl), is.finite(k), is.finite(l), is.finite(ll)) %>% left_join(et, by = "sic_3")
    fm <- if (opt$fomega == "CfKL") tilde_cal_W ~ Wl + kl + k + ll + l + factor(sic_3) else tilde_cal_W ~ Wl + kl + k + factor(sic_3)
    rg <- lm(fm, aw); aw$res <- resid(rg)
    cat(sprintf("[%s] regression on %d firm-years, R2 = %.3f; slopes:\n", opt$fomega, nrow(aw), summary(rg)$r.squared)); print(round(coef(rg)[c("Wl", "kl", "k", "ll", "l")], 4))
    vr <- aw %>% group_by(sic_3) %>% summarise(v_res = var(res), n = n(), bo = 1 - mean(beta), sd_eps_j = first(sd_eps), pf = first(pooled_fallback), .groups = "drop")
    vpool <- with(aw, var(res)); bo_p <- 1 - mean(aw$beta); sde_p <- sqrt(mean(et$sd_eps^2))
    vr <- vr %>% mutate(sd_om_c = sqrt(pmax(ifelse(pf | n < 30, vpool - bo_p^2 * sde_p^2, v_res - bo^2 * sd_eps_j^2), 0.05 * ifelse(pf | n < 30, vpool, v_res))))
    interior <- interior %>% left_join(lagd, by = c("plant", "year")) %>% dplyr::filter(is.finite(Wl), is.finite(kl), is.finite(ll))
    interior$fit_cf <- predict(rg, newdata = interior %>% mutate(sic_3 = ifelse(as.character(sic_3) %in% as.character(aw$sic_3), sic_3, aw$sic_3[1])))
}
out <- interior %>%
    left_join(et %>% select(sic_3, n_corp, pooled_fallback, mu_eps_i = mu_eps, sd_eps_i = sd_eps), by = "sic_3") %>%
    left_join(om_ind %>% select(sic_3, mu_om, sd_om), by = "sic_3") %>%
    { if (opt$fomega != "A") left_join(., vr %>% select(sic_3, sd_om_c), by = "sic_3") else . } %>%
    mutate(mu_eps = ifelse(pooled_fallback, pool_row$mu_eps, mu_eps_i),
           sd_eps = ifelse(pooled_fallback, pool_row$sd_eps, sd_eps_i)) %>%
    { if (opt$fomega != "A") mutate(., mu_om = fit_cf - (1 - beta) * ifelse(pooled_fallback, pool_row$mu_eps, mu_eps_i), sd_om = sd_om_c) else . } %>%
    transmute(V = cal_V, Wt = tilde_cal_W, Mst = M_star, taur = sales_tax_rate_purchases, beta = beta,
              mu_eps, sd_eps, mu_om, sd_om, sic_3, year, row_id = row_number())
stopifnot(all(is.finite(as.matrix(out %>% select(V:sd_om)))), all(out$sd_om > 0), all(out$sd_eps > 0))

cat(sprintf("Sample: n=%d interior firms, %d industries; pooled-fallback firms: %d\n",
            nrow(out), n_distinct(out$sic_3), sum(out$sic_3 %in% et$sic_3[et$pooled_fallback])))
cat(sprintf("sd_eps range %.3f-%.3f, sd_om range %.3f-%.3f, taur range %.2g-%.2g\n",
            min(out$sd_eps), max(out$sd_eps), min(out$sd_om), max(out$sd_om), min(out$taur), max(out$taur)))
f <- sprintf("Code/Products/msl/1401-msl-input-%s-trim%g%s.csv", opt$ins, opt$trim_top_pct, if (opt$fomega == "A") "" else paste0("-", opt$fomega))
write.csv(out, f, row.names = FALSE, quote = FALSE)
cat("Saved:", f, "\n")
