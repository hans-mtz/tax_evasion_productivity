## Full R=250 bootstrap for the joint efficient-GMM PF estimates (2026-09-21), following on from 1475's timing
## test (0.7 sec/replicate -> R=250 trivially cheap; point estimates there reproduced 1474 exactly). Design and CI
## convention match 490-boot-pf-prod.R exactly (see 1475's header comment for the full rationale): resample_by_group()
## block-bootstraps plants WITH replacement, stratified by sic_3 x Corp/Other, whole plant history kept; re-run
## first_stage_panel_me PER REPLICATE (propagates stage-1 beta sampling variance); refit the joint (m*_{it-1} +
## W~_{it-2}) efficient-GMM estimator of 1474 on the resampled fs_list, warm-started from the point estimate.
## Self-contained (doesn't source/depend on 1474 or 1475 at runtime) per the "reproducible products" convention.
library(tidyverse); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata")
load("Code/Products/931.1-fs-se-het.RData")   # df (raw panel + current cal_V/cal_W/epsilon; NOT resampled -- base for the bootstrap), fs_all_ls (point-estimate fits)
five <- c("331", "322", "369", "313", "321")
base_df <- df %>% select(-any_of(c("cal_V", "cal_W", "epsilon")))
R <- 250; set.seed(66636)   # matches the project-standard bootstrap seed/R (240-fs-boot.R, 490-boot-pf-prod.R)

## %% Joint efficient-GMM fit, refactored to take an explicit fs_list (was hardcoded in 1474) -----------------
prep <- function(fs_list, x) {
    d <- fs_list[[x]]$data %>% ungroup() %>% filter(is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m)) %>% arrange(plant, year)
    n <- nrow(d); pl <- d$plant
    lagi <- function(j) { i <- seq_len(n) - j; ok <- i >= 1; i[!ok] <- NA; i[ok][pl[pmax(i[ok], 1)] != pl[ok]] <- NA
                          if (j == 2) { i1 <- seq_len(n) - 1; bad <- is.na(i) | i1 < 1 | pl[pmax(i1, 1)] != pl; i[bad] <- NA }; i }
    list(d = d, p1 = lagi(1), p2 = lagi(2))
}
fit_joint <- function(fs_list, x, starts_extra = list()) {
    P <- tryCatch(prep(fs_list, x), error = function(e) NULL); if (is.null(P) || is.null(P$d) || nrow(P$d) < 30) return(NULL)
    d <- P$d; S <- which(!is.na(P$p1) & !is.na(P$p2)); if (length(S) < 30) return(NULL)
    p1 <- P$p1[S]; p2 <- P$p2[S]; pl <- d$plant[S]; n <- length(S)
    build <- function(aK, aL) { w <- d$cal_W - aK * d$k - aL * d$l; y <- w[S]; xlag <- w[p1]
        list(M = cbind(1, d$m[p1], w[p2], d$k[S], d$l[S]), y = y, xlag = xlag) }
    gamma_given <- function(aK, aL, W) { mm <- build(aK, aL); M <- mm$M; y <- mm$y; x <- mm$xlag
        a <- colMeans(M * y); B <- crossprod(M, cbind(1, x)) / n
        gam <- tryCatch(solve(t(B) %*% W %*% B, t(B) %*% W %*% a), error = function(e) c(NA, NA))
        if (anyNA(gam)) return(NULL); eta <- y - gam[1] - gam[2] * x; list(gam = gam, G = M * eta) }
    crit <- function(par, W) { if (any(par < 0 | par > 1)) return(1e10 * sum(pmax(0 - par, par - 1, 0)^2) + 1e6)
        r <- gamma_given(par[1], par[2], W); if (is.null(r)) return(1e12); gbar <- colMeans(r$G); n * drop(t(gbar) %*% W %*% gbar) }
    search <- function(W, starts) { best <- NULL
        for (s0 in starts) { o1 <- tryCatch(optim(s0, crit, W = W, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control = list(maxit = 200)), error = function(e) NULL)
            if (is.null(o1)) next
            o2 <- tryCatch(optim(o1$par, crit, W = W, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control = list(maxit = 200)), error = function(e) o1)
            if (is.null(best) || o2$value < best$value) best <- o2 }
        best }
    clustered_Omega <- function(G) { rs <- rowsum(G, pl); np <- as.vector(table(pl))[match(rownames(rs), names(table(pl)))]
        Gc <- rs - outer(np, colMeans(G)); crossprod(Gc) / n }
    ols0 <- tryCatch(pmin(pmax(coef(lm(cal_W ~ k + l, d))[-1], 0.01), 0.99), error = function(e) c(0.3, 0.3))
    starts <- unique(c(list(as.numeric(ols0), c(0.3, 0.3)), starts_extra))
    r0 <- gamma_given(ols0[1], ols0[2], diag(5)); if (is.null(r0)) return(NULL)
    v0 <- apply(r0$G, 2, var); if (any(!is.finite(v0)) || any(v0 <= 0)) return(NULL); W_id <- diag(1 / v0)
    fit1 <- search(W_id, starts); if (is.null(fit1)) return(NULL); a1 <- fit1$par
    r1 <- gamma_given(a1[1], a1[2], W_id); if (is.null(r1)) return(NULL)
    Om <- clustered_Omega(r1$G); W_eff <- tryCatch(solve(Om), error = function(e) tryCatch(solve(Om + 1e-6 * diag(5)), error = function(e2) NULL))
    if (is.null(W_eff)) return(NULL)
    fit2 <- search(W_eff, unique(c(starts, list(a1)))); if (is.null(fit2)) return(NULL)
    tibble(alpha_K = fit2$par[1], alpha_L = fit2$par[2], beta = fs_list[[x]]$beta, n = n)
}


t0 <- bind_rows(lapply(five, \(x) fit_joint(fs_all_ls, x) %>% mutate(sic_3 = x)))

one_rep <- function(b) {
    set.seed(66636 + b)
    resampled <- resample_by_group(base_df, sic_3)
    fs_b <- setNames(lapply(five, \(x) tryCatch(first_stage_panel_me(x, "log_mats_share", "materials", resampled), error = function(e) NULL)), five)
    bind_rows(lapply(five, \(x) {
        if (is.null(fs_b[[x]]) || is.null(fs_b[[x]]$data)) return(NULL)
        r <- fit_joint(fs_b, x, starts_extra = list(as.numeric(t0[t0$sic_3 == x, c("alpha_K", "alpha_L")])))
        if (is.null(r)) return(NULL); r %>% mutate(sic_3 = x, rep = b)
    }))
}
boot <- bind_rows(mclapply(seq_len(R), one_rep, mc.cores = max(1, detectCores() - 2)))
save(t0, boot, file = "Code/Products/1476-pf-joint-gmm-bootstrap.RData")
cat(sprintf("Replicates returned: %d / %d expected (%d industries x %d reps)\n", nrow(boot), 5 * R, 5, R))
cat("Replicates per industry:\n"); print(table(boot$sic_3))
cat("Share of bootstrap draws at a bound (alpha<0.001 or >0.999):\n")
print(boot %>% group_by(sic_3) %>% summarise(at_bound_K = mean(alpha_K < .001 | alpha_K > .999), at_bound_L = mean(alpha_L < .001 | alpha_L > .999)))

## %% Bias-corrected percentile CI, SAME convention as 490-boot-pf-prod.R: CI = t0 - quantile(boot - t0, c(.975,.025))
ci <- boot %>% left_join(t0 %>% select(sic_3, alpha_K0 = alpha_K, alpha_L0 = alpha_L, beta0 = beta), by = "sic_3") %>%
    mutate(bc_K = alpha_K - alpha_K0, bc_L = alpha_L - alpha_L0) %>%
    group_by(sic_3) %>%
    summarise(alpha_K = first(alpha_K0), K_lo = alpha_K0 - quantile(bc_K, .975), K_hi = alpha_K0 - quantile(bc_K, .025),
              alpha_L = first(alpha_L0), L_lo = alpha_L0 - quantile(bc_L, .975), L_hi = alpha_L0 - quantile(bc_L, .025),
              beta = first(beta0), se_K = sd(alpha_K), se_L = sd(alpha_L), n_reps = n(), .groups = "drop")
write.csv(ci, "Code/Products/1476-pf-joint-gmm-ci.csv", row.names = FALSE)
cat("\n== Bias-corrected percentile 95% CI (bootstrap SE also reported) ==\n")
write.table(ci %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)
