## PRODUCT: Code/Products/1502-pf-joint-gmm-net.{csv,RData} := joint efficient-GMM PF step (instruments m*_{it-1} AND
## W~_{it-2} together; 5 moments (1, m*_{it-1}, W~_{it-2}, k, l)*eta; 4 parameters alpha_K, alpha_L, gamma_0, gamma_1; beta
## fixed at its first-stage value) for ALL industries, on the two-tax (net-of-tax) first stage (1501-fs-net.RData).
## Plan step A2 (Research-log/log.md, 2026-09-26). Estimator copied unchanged from 1474-pf-joint-efficient-gmm.R (which ran
## the 5 headline industries on the gross first stage); only the input and the industry list differ. Point estimates only:
## stage 2 needs (alpha_K, alpha_L); the test-inversion regions (1477) are for the thesis tables.
library(tidyverse); library(parallel)
load("Code/Products/1501-fs-net.RData")   # fs_net_ls

prep <- function(x) {
    d <- fs_net_ls[[x]]$data %>% ungroup() %>% filter(is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m)) %>% arrange(plant, year)
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
    m_start <- fs_net_ls[[x]]$beta  # placeholder unused; real multi-starts below use existing 2SLS point estimates if available
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


safe_fit <- function(x) tryCatch(fit_joint(x), error = function(e) tibble(sic_3 = x, error = conditionMessage(e)))
res <- bind_rows(mclapply(names(fs_net_ls), safe_fit, mc.cores = max(1, detectCores() - 2)))
res$beta <- vapply(res$sic_3, \(s) fs_net_ls[[s]]$beta, numeric(1))
write.csv(res, "Code/Products/1502-pf-joint-gmm-net.csv", row.names = FALSE)
save(res, file = "Code/Products/1502-pf-joint-gmm-net.RData")
cat("Saved: Code/Products/1502-pf-joint-gmm-net.{csv,RData}\n")

## Compare with the PF estimates stage 2 used until now (gross first stage, single instrument m*_{it-1}, 1100-MSL-opttax.RData)
load("Code/Products/1100-MSL-opttax.RData")   # pf_list
old <- map_dfr(names(pf_list), \(s) tibble(sic_3 = s, aK_old = unname(pf_list[[s]]$coeffs["k"]), aL_old = unname(pf_list[[s]]$coeffs["l"])))
cmp <- res %>% left_join(old, by = "sic_3") %>%
    transmute(sic_3, n, beta = round(beta, 3), aK_old = round(aK_old, 3), aK_new = round(alpha_K_eff, 3),
              aL_old = round(aL_old, 3), aL_new = round(alpha_L_eff, 3), rts_new = round(beta + alpha_K_eff + alpha_L_eff, 3),
              J = round(J_eff, 2), pass,
              at_bound_old = (aK_old %in% c(0, 1)) | (aL_old %in% c(0, 1)) | aK_old < 1e-3 | aK_old > 0.999 | aL_old < 1e-3 | aL_old > 0.999,
              at_bound_new = alpha_K_eff < 1e-3 | alpha_K_eff > 0.999 | alpha_L_eff < 1e-3 | alpha_L_eff > 0.999)
options(width = 220); print(cmp, n = 40)
cat(sprintf("\nIndustries: %d | at a bound (0 or 1): old %d, new %d | RTS in [0.8,1.2]: new %d | overid test passes (chi2_1, 5%%): %d\n",
            nrow(cmp), sum(cmp$at_bound_old, na.rm = TRUE), sum(cmp$at_bound_new, na.rm = TRUE),
            sum(cmp$rts_new >= 0.8 & cmp$rts_new <= 1.2, na.rm = TRUE), sum(cmp$pass, na.rm = TRUE)))
if ("error" %in% names(res)) print(res %>% filter(!is.na(error)) %>% select(sic_3, error))
