## Data-calibrated single-industry DGP (industry 321, lag_m sample), 2026-09-19.
## MODEL AS IS: same eps distribution for every firm (bootstrap of the corp ecdf), u = ln(1+e/M), FOC with linear q and linear-in-e cost,
## materials FOC (M deterministic in omega,k,l), AR(1) omega. Everything below is set from the industry-321 data EXCEPT the evasion block
## (lambda, delta0,1,2, psi), which 1451-calibrate-321.R matches to the observed distribution of V.
## Data-set parameters: beta,aK,aL (industry constants), corp share, tau_P (empirical resampling, exempt share), eps pool (corp ecdf, mean 0),
## (omega,k,l) stationary means/covariances (omega's variance = var(W) - bo^2 var(eps); cov with k,l = cov(W,k/l) since eps is indep. of k,l),
## k_t,l_t laws of motion = projections on (omega_{t-1},k_{t-1},l_{t-1}) using the same covariance algebra (errors-in-variables handled by using var(omega)).
suppressMessages(library(dplyr))
make_spec_321 <- function(sic = 321, ins = "lag_m", gamma1 = 0.97) {
    load("Code/Products/1200-stage2-data.RData"); d <- subset(stage2_data, ins == ins & sic_3 == sic)
    bo <- 1 - mean(d$beta); ce <- d$epsilon[d$corp & is.finite(d$epsilon)]
    it <- subset(d, !corp & sales_tax_rate_purchases > 0); nc <- subset(d, !corp)
    ## (omega,k,l) stationary law and k,l laws of motion, ESTIMATED SEPARATELY for corps and unincorporated (corps are much larger: mean k 11.7 vs 8.9).
    grp <- function(g) {
        vom <- var(g$tilde_cal_W) - bo^2 * var(ce)
        mu <- c(om = mean(g$tilde_cal_W) - bo * mean(ce), k = mean(g$k), l = mean(g$l))
        S <- cov(cbind(om = g$tilde_cal_W, k = g$k, l = g$l)); S[1, 1] <- vom            # cov(omega,k)=cov(W,k): eps indep. of k,l; var(omega) noise-corrected
        p <- g %>% arrange(plant, year) %>% group_by(plant) %>% mutate(Wl = dplyr::lag(tilde_cal_W), kl = dplyr::lag(k), ll = dplyr::lag(l), yl = dplyr::lag(year)) %>% ungroup() %>% filter(!is.na(Wl), year == yl + 1)
        SXX <- cov(cbind(om = p$Wl, k = p$kl, l = p$ll)); SXX[1, 1] <- vom
        SXY <- cov(cbind(om = p$Wl, k = p$kl, l = p$ll), cbind(k = p$k, l = p$l))
        B <- solve(SXX, SXY); mX <- c(mean(p$Wl) - bo * mean(ce), mean(p$kl), mean(p$ll)); mY <- c(mean(p$k), mean(p$l))
        list(mu = mu, S = S, B = B, A0 = mY - drop(t(B) %*% mX), Snu = cov(cbind(k = p$k, l = p$l)) - t(B) %*% SXX %*% B, n_pairs = nrow(p))
    }
    list(sic = sic, beta = mean(d$beta), aK = mean(d$alpha_K), aL = mean(d$alpha_L), share_corp = mean(d$corp), share_exempt = mean(nc$sales_tax_rate_purchases == 0),
         eps_pool = ce, tau_pool = it$sales_tax_rate_purchases, nc = grp(nc), co = grp(subset(d, corp)), gamma1 = gamma1,
         lnM_target = mean(log(it$M_star)), T = 11, sd_eps = sd(ce))
}
## Shocks independent of the evasion parameters (drawn once, common random numbers across calibration evaluations)
draw_shocks_321 <- function(spec, N, seed) {
    set.seed(seed); T <- spec$T; corp_i <- runif(N) < spec$share_corp
    om <- matrix(NA, N, T); k <- l <- om
    for (grp_ in c("nc", "co")) {
        g <- spec[[grp_]]; idx <- which(if (grp_ == "co") corp_i else !corp_i); n <- length(idx)
        z0 <- sweep(matrix(rnorm(n * 3), n, 3) %*% chol(g$S), 2, g$mu, "+")
        o <- kk <- ll <- matrix(NA, n, T); o[, 1] <- z0[, 1]; kk[, 1] <- z0[, 2]; ll[, 1] <- z0[, 3]
        g1 <- spec$gamma1; sd_eta <- sqrt(g$S[1, 1] * (1 - g1^2)); g0 <- g$mu["om"] * (1 - g1); Ln <- chol(g$Snu)
        for (t in 2:T) {
            o[, t] <- g0 + g1 * o[, t - 1] + rnorm(n, 0, sd_eta)
            Y <- sweep(cbind(o[, t - 1], kk[, t - 1], ll[, t - 1]) %*% g$B, 2, g$A0, "+") + matrix(rnorm(n * 2), n, 2) %*% Ln
            kk[, t] <- Y[, 1]; ll[, t] <- Y[, 2]
        }
        om[idx, ] <- o; k[idx, ] <- kk; l[idx, ] <- ll
    }
    list(N = N, T = T, om = om, k = k, l = l, eps = matrix(sample(spec$eps_pool, N * T, TRUE), N, T), z_psi = matrix(rnorm(N * T), N, T),
         tau = matrix(sample(spec$tau_pool, N * T, TRUE), N, T), exempt = rep(!corp_i & runif(N) < spec$share_exempt / (1 - spec$share_corp), T), corp = rep(corp_i, T))
}
## Build the panel (same column names as 1410-dgp-model.R so 1411/1412/1422 work unchanged). ev = c(lambda, delta0, delta1, delta2, theta_psi); psi_k shape.
build_321 <- function(spec, sh, ev, psi_k = 1, pc = NULL) {   # pc = ln P - beta*ln(rho): nominal price constant; cancels in V, sets the M* level
    lam <- ev[1]; d0 <- ev[2]; d1 <- ev[3]; d2 <- ev[4]; th <- ev[5]; b <- spec$beta
    if (is.null(pc)) pc <- (1 - b) * spec$lnM_target - (log(b) + spec$aK * spec$nc$mu["k"] + spec$aL * spec$nc$mu["l"] + spec$nc$mu["om"])
    v <- function(m) as.vector(m)
    om <- v(sh$om); k <- v(sh$k); l <- v(sh$l); eps <- v(sh$eps)
    psi <- psi_k * th - qgamma(pmin(pmax(pnorm(v(sh$z_psi)), 1e-12), 1 - 1e-12), shape = psi_k, scale = th)
    tau <- ifelse(sh$exempt, 0, v(sh$tau)); corp <- sh$corp
    lnx <- (log(b) + spec$aK * k + spec$aL * l + om + pc) / (1 - b); x <- exp(lnx)
    C <- d0 - d1 * om + d2 * om^2
    e <- ifelse(corp | tau <= 0, 0, pmax(0, (1 - exp(C + psi) / pmax(tau, 1e-12)) / (2 * lam)))
    xs <- x + e; y <- spec$aK * k + spec$aL * l + b * lnx + pc + om + eps      # y = ln Y; -beta*ln(rho) = pc
    V <- log(xs) - y - log(b); ms <- log(xs) + pc / b; Wt <- y - b * (ms - V) - spec$aK * k - spec$aL * l
    d <- tibble(i = rep(seq_len(sh$N), sh$T), t = rep(seq_len(sh$T), each = sh$N), sector = 1L, corp = corp, omega = om, eps = eps, psi = psi, k = k, l = l,
                tauP = tau, x = x, e = e, xstar = xs, y = y, V = V, mstar = ms, Wt = Wt, pc = pc)
    d %>% arrange(i, t) %>% group_by(i) %>% mutate(lag_mstar = dplyr::lag(mstar)) %>% ungroup()
}
