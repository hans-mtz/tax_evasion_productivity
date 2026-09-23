## Joint efficient-GMM refit of the PF step (2026-09-21): instruments m*_{it-1} AND W~_{it-2} used TOGETHER (not one-at-a-time as in 1472/1473),
## plus the outer (k,l) moments -- 5 moments (1, Z1, Z2, k, l)*eta, 4 parameters (alpha_K, alpha_L, gamma_0, gamma_1), 1 overidentifying restriction.
## "Embedded 2SLS": for FIXED (alpha_K,alpha_L), w_t=cal_W_t-aK*k_t-aL*l_t is data, so eta=w_t-g0-g1*w_{t-1} is LINEAR in (g0,g1) -- closed-form
## weighted-2SLS/GMM-IV, gamma_hat(alpha,W)=solve(B'WB, B'Wa), for ANY W. Nested numeric search only ever needs to be over (alpha_K,alpha_L).
##   Step 1 (identity-like): W = diag(1/var(moment_j)) at a reference alpha (OLS start) -- fixed before the search, so it doesn't depend on theta.
##   Step 2 (efficient): Omega_hat = plant-clustered covariance of the moments at the step-1 optimum; W = Omega_hat^{-1}; re-optimize.
## Paper's own sample (fs_all_ls, 931.1); requires BOTH lags (t-1 and t-2) -- a further common-sample restriction vs the single-instrument fits.
## Outputs: Code/Products/1474-pf-joint-gmm.csv, printed comparison table.
library(tidyverse); library(parallel)
load("Code/Products/931.1-fs-se-het.RData")   # fs_all_ls
five <- c("331", "322", "369", "313", "321")

prep <- function(x) {
    d <- fs_all_ls[[x]]$data %>% ungroup() %>% filter(is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m)) %>% arrange(plant, year)
    n <- nrow(d); pl <- d$plant
    lagi <- function(j) { i <- seq_len(n) - j; ok <- i >= 1; i[!ok] <- NA; i[ok][pl[pmax(i[ok],1)] != pl[ok]] <- NA
                          if (j == 2) { i1 <- seq_len(n) - 1; bad <- is.na(i) | i1 < 1 | pl[pmax(i1, 1)] != pl; i[bad] <- NA }; i }
    list(d = d, p1 = lagi(1), p2 = lagi(2))
}

fit_joint <- function(x) {
    P <- prep(x); d <- P$d
    S <- which(!is.na(P$p1) & !is.na(P$p2))   # needs BOTH lags (Z2 = w_{it-2})
    p1 <- P$p1[S]; p2 <- P$p2[S]; pl <- d$plant[S]; n <- length(S)

    build <- function(aK, aL) {
        w <- d$cal_W - aK * d$k - aL * d$l
        y <- w[S]; xlag <- w[p1]
        M <- cbind(1, d$m[p1], w[p2], d$k[S], d$l[S])           # (1, m*_{it-1}, W~_{it-2}, k, l)
        list(M = M, y = y, xlag = xlag)
    }
    gamma_given <- function(aK, aL, W) {
        mm <- build(aK, aL); M <- mm$M; y <- mm$y; x <- mm$xlag
        a <- colMeans(M * y); B <- crossprod(M, cbind(1, x)) / n
        gam <- tryCatch(solve(t(B) %*% W %*% B, t(B) %*% W %*% a), error = function(e) c(NA, NA))
        if (anyNA(gam)) return(NULL)
        eta <- y - gam[1] - gam[2] * x
        list(gam = gam, G = M * eta)
    }
    crit <- function(par, W) {
        if (any(par < 0 | par > 1)) return(1e10 * sum(pmax(0 - par, par - 1, 0)^2) + 1e6)
        r <- gamma_given(par[1], par[2], W); if (is.null(r)) return(1e12)
        gbar <- colMeans(r$G); n * drop(t(gbar) %*% W %*% gbar)
    }
    search <- function(W, starts) {
        best <- NULL
        for (s0 in starts) {
            o1 <- optim(s0, crit, W = W, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control = list(maxit = 300))
            o2 <- optim(o1$par, crit, W = W, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control = list(maxit = 300))  # 2-pass refine
            if (is.null(best) || o2$value < best$value) best <- o2
        }
        best
    }
    clustered_Omega <- function(G) {
        Gc <- rowsum(G, pl) - outer(as.vector(table(pl)[as.character(unique(pl))])[match(rownames(rowsum(G, pl)), as.character(unique(pl)))], colMeans(G))
        crossprod(Gc) / n
    }

    ols0 <- coef(lm(cal_W ~ k + l, d))[-1]; ols0 <- pmin(pmax(ols0, 0.01), 0.99)
    m_start <- fs_all_ls[[x]]$beta  # placeholder unused; real multi-starts below use existing 2SLS point estimates if available
    starts <- unique(list(as.numeric(ols0), c(0.3, 0.3), c(0.1, 0.5)))

    ## Step 1: diagonal "identity-like" weight, fixed at the OLS-start moments (not re-estimated -> not a function of theta)
    r0 <- gamma_given(ols0[1], ols0[2], diag(5))
    v0 <- apply(r0$G, 2, var); W_id <- diag(1 / v0)
    fit1 <- search(W_id, starts)
    a1 <- fit1$par

    ## Step 2: efficient, Omega at the step-1 optimum
    r1 <- gamma_given(a1[1], a1[2], W_id)
    Om <- clustered_Omega(r1$G)
    W_eff <- tryCatch(solve(Om), error = function(e) solve(Om + 1e-8 * diag(5)))
    fit2 <- search(W_eff, unique(c(starts, list(a1))))
    a2 <- fit2$par
    gam2 <- gamma_given(a2[1], a2[2], W_eff)$gam
    J_eff <- fit2$value   # n * gbar' W_eff gbar at the efficient optimum -- chi2_1 (5 moments - 4 params) if correctly specified

    tibble(sic_3 = x, n = n, alpha_K_id = a1[1], alpha_L_id = a1[2], crit_id = fit1$value,
           alpha_K_eff = a2[1], alpha_L_eff = a2[2], gamma0_eff = gam2[1], gamma1_eff = gam2[2],
           J_eff = J_eff, crit95_1df = qchisq(.95, 1), pass = J_eff <= qchisq(.95, 1))
}

res <- bind_rows(mclapply(five, fit_joint, mc.cores = 5))
write.csv(res, "Code/Products/1474-pf-joint-gmm.csv", row.names = FALSE)

single <- read.csv("Code/Products/1472-pf-instrument-comparison.csv") %>% mutate(sic_3 = as.character(sic_3)) %>%
    filter(sic_3 %in% five, ins %in% c("lag_m", "lag_2_w_eps")) %>% select(sic_3, ins, alpha_K, alpha_L) %>%
    pivot_wider(names_from = ins, values_from = c(alpha_K, alpha_L))

cmp <- res %>% left_join(single, by = "sic_3") %>%
    transmute(sic_3, n,
              K_m_single = alpha_K_lag_m, K_W_single = alpha_K_lag_2_w_eps, K_id = alpha_K_id, K_eff = alpha_K_eff,
              L_m_single = alpha_L_lag_m, L_W_single = alpha_L_lag_2_w_eps, L_id = alpha_L_id, L_eff = alpha_L_eff,
              d_K_id_eff = alpha_K_eff - alpha_K_id, d_L_id_eff = alpha_L_eff - alpha_L_id, J_eff, pass)
cat("== Joint-GMM (both instruments) vs each single-instrument 2SLS, 5 paper industries ==\n")
write.table(cmp %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)
cat(sprintf("\nmean |alpha_K: efficient - identity-step|: %.3f, mean |alpha_L: efficient - identity-step|: %.3f\n",
            mean(abs(cmp$d_K_id_eff)), mean(abs(cmp$d_L_id_eff))))
cat("Overid test (J_eff vs chi2_1,.95=3.84): ", sum(res$pass), "/", nrow(res), " industries pass (don't reject joint validity)\n", sep = "")
