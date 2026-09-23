## Model-faithful DGP for the tax-evasion model (2026-09-18) --------------------
## Purpose: a single simulated panel on which BOTH stage-2 estimators (MSL, ELVIS)
## can be tested against known truth. Adapted from the structure of GNR's Monte
## Carlo (Gandhi-Navarro-Rivers, Table 1 CD: AR(1) productivity with eta, Cobb-
## Douglas, materials from the static FOC, capital as a state) with the tax /
## evasion block, corporations and exempt sectors added -- GNR has no evasion.
##
## EVERY endogenous variable is a function of the exogenous shocks and states:
##   shocks:   eta_it (AR(1) innovation), eps_it (ex-post output shock, E[e^eps]=1),
##             psi_it (evasion-cost shock), nu_it (capital adjustment shock),
##             tau_P (sector-year purchases tax rate + firm noise; exempt sectors 0),
##             corp_i (constrained non-evader), initial omega_i0, k_i0
##   states:   omega_it = g0 + g1*omega_i,t-1 + eta_it
##             k_it = kap0 + kap1*k_i,t-1 + kap2*omega_i,t-1 + nu_it   (predetermined at t-1:
##             a reduced-form policy function of the lagged state, not solved dynamically)
##   choices:  x_it (true nominal materials) from the static materials FOC
##                 x^(1-beta) = beta*P*rho^(-beta)*exp(aK*k + omega)   [E[e^eps]=1]
##             e_it from the evasion FOC (linear q, linear-in-e cost):
##                 tau_P (1 - 2 lambda e) = exp(C(omega)+psi),  C = d0 - d1*omega + d2*omega^2
##                 e = max(0, (1 - exp(C+psi)/tau_P)/(2 lambda));  corp or tau_P=0 => e=0
##   outcomes: x* = x + e ;  Y = exp(aK*k) * (x/rho)^beta * exp(omega+eps)
##   observables (as in the real pipeline):
##             V  = ln(x*/(P*Y)) - ln(beta)  (= ln(x*/x) - eps)      [nominal ratio only; x is nominal spending]
##             Wt = y - beta*(m* - V) - aK*k     (= omega + (1-beta) eps)  with y=ln Y, m*=ln(x*/rho)
## Not simulated (documented): tau_S wedge in the materials FOC (a constant, absorbed
## in rho), labor, entry/exit (panel is balanced), industries with different beta.

library(tidyverse)

default_spec <- function() list(
    N = 3000, T = 11, burn = 25, S = 20,
    share_exempt = 0.10, share_corp = 0.20,
    beta = 0.55, aK = 0.20, P = 1, rho = 1,
    g1 = 0.90, mu_om = 2.94, sd_om = 0.85,           # stationary mean/sd of omega
    sd_eps = 0.42,
    k_mean = 8.5, kap1 = 0.80, kap2 = 0.40, sd_nu = 0.40,
    tau_mean = 0.088, tau_sd = 0.04, tau_year_sd = 0.15, tau_firm_sd = 0.20,
    lambda = 3e-5, d0 = 3.7, d1 = 3.5, d2 = 0.5, sigma_psi = 1.0,
    ## psi distribution (2026-09-19): "normal" (sd sigma_psi) or "gamma": psi = k*theta - Gamma(shape k, scale theta), mean 0, support (-Inf, k*theta),
    ## left-skewed => e = (1-r)/(2 lambda) right-skewed. Built from the SAME normal shocks z_psi (common random numbers) via psi = k*theta - qgamma(pnorm(z)).
    psi_dist = "normal", psi_k = 1, psi_theta = 1
)

## Exogenous shocks, drawn ONCE (common random numbers -> calibration is smooth)
draw_shocks <- function(spec, seed) {
    set.seed(seed)
    with(spec, {
        sd_eta <- sd_om * sqrt(1 - g1^2)
        n_all <- N; Tt <- burn + T
        sector <- sample.int(S, N, replace = TRUE)
        exempt_s <- seq_len(S) <= round(share_exempt * S)          # first sectors exempt
        tau_s <- pmin(pmax(rnorm(S, tau_mean, tau_sd), 0.01), 0.25); tau_s[exempt_s] <- 0
        tau_st <- outer(tau_s, rep(1, Tt)) * exp(matrix(rnorm(S * Tt, 0, tau_year_sd), S, Tt))
        list(sector = sector, corp = runif(N) < share_corp, tau_st = tau_st,
             eta = matrix(rnorm(N * Tt, 0, sd_eta), N, Tt),
             eps = matrix(rnorm(N * Tt, -0.5 * sd_eps^2, sd_eps), N, Tt),
             z_psi = matrix(rnorm(N * Tt), N, Tt), nu = matrix(rnorm(N * Tt, 0, sd_nu), N, Tt),
             z_tau = matrix(rnorm(N * Tt), N, Tt), om0 = rnorm(N, mu_om, sd_om),
             k0 = rnorm(N, k_mean, 1.5), Tt = Tt)
    })
}

## Build the panel from shocks + parameters (deterministic given the shocks)
build_panel <- function(spec, sh) {
    with(c(spec, sh), {
        g0 <- mu_om * (1 - g1)
        kap0 <- k_mean * (1 - kap1) - kap2 * mu_om
        om <- k <- matrix(NA_real_, N, Tt)
        om[, 1] <- om0; k[, 1] <- k0
        for (t in 2:Tt) {
            om[, t] <- g0 + g1 * om[, t - 1] + eta[, t]
            k[, t]  <- kap0 + kap1 * k[, t - 1] + kap2 * om[, t - 1] + nu[, t]
        }
        keep <- (burn + 1):Tt
        idx <- expand.grid(i = seq_len(N), t = seq_along(keep))
        col <- keep[idx$t]
        pick <- function(m) m[cbind(idx$i, col)]
        d <- tibble(i = idx$i, t = idx$t, sector = sector[idx$i], corp = corp[idx$i],
                    omega = pick(om), eps = pick(eps),
                    psi = if (psi_dist == "gamma") psi_k * psi_theta - qgamma(pmin(pmax(pnorm(pick(z_psi)), 1e-12), 1 - 1e-12), shape = psi_k, scale = psi_theta) else sigma_psi * pick(z_psi),
                    k = pick(k),
                    eta = pick(eta), nu = pick(nu))
        d$tauP <- tau_st[cbind(d$sector, col)] * ifelse(d$sector <= round(share_exempt * S), 1, exp(tau_firm_sd * pick(z_tau)))
        d$lnx <- (log(beta) + log(P) - beta * log(rho) + aK * d$k + d$omega) / (1 - beta)
        d$x <- exp(d$lnx)
        C <- d0 - d1 * d$omega + d2 * d$omega^2
        d$e <- ifelse(d$corp | d$tauP <= 0, 0, pmax(0, (1 - exp(C + d$psi) / d$tauP) / (2 * lambda)))
        d$xstar <- d$x + d$e
        d$y <- aK * d$k + beta * (d$lnx - log(rho)) + d$omega + d$eps        # ln Y
        d$V <- log(d$xstar / (P * exp(d$y))) - log(beta)          # x is NOMINAL spending: no extra rho
        d$mstar <- log(d$xstar / rho)
        d$Wt <- d$y - beta * (d$mstar - d$V) - aK * d$k
        d <- d %>% arrange(i, t) %>% group_by(i) %>%
            mutate(lag_mstar = dplyr::lag(mstar)) %>% ungroup()
        d
    })
}

simulate_panel <- function(spec = default_spec(), seed = 1) {
    sh <- draw_shocks(spec, seed)
    list(data = build_panel(spec, sh), spec = spec, shocks = sh)
}

## Internal consistency checks (must all be ~0 / ~1) --------------------------------
check_panel <- function(sim) {
    d <- sim$data; s <- sim$spec
    C <- s$d0 - s$d1 * d$omega + s$d2 * d$omega^2
    ev <- d$e > 0
    list(
        foc_resid_max      = max(abs(d$tauP[ev] * (1 - 2 * s$lambda * d$e[ev]) - exp(C[ev] + d$psi[ev]))),
        V_identity_max     = max(abs(d$V - (log(d$xstar / d$x) - d$eps))),
        Wt_identity_max    = max(abs(d$Wt - (d$omega + (1 - s$beta) * d$eps))),
        corp_V_plus_eps    = max(abs((d$V + d$eps)[d$corp])),
        mean_exp_eps       = mean(exp(d$eps)),
        e_nonneg           = all(d$e >= 0),
        ar1_gamma1_hat     = { z <- d %>% group_by(i) %>% mutate(omega_l = dplyr::lag(omega)) %>% ungroup() %>% filter(!is.na(omega_l)); unname(coef(lm(omega ~ omega_l, data = z))[2]) },
        eta_indep_of_lag_k = abs(cor(d$eta, d$k))       # k_t is predetermined: uncorrelated with eta_t
    )
}

## Summary vs. real-data ballparks ----------------------------------------------------
summarize_panel <- function(sim) {
    d <- sim$data; u <- d %>% filter(!corp, tauP > 0)
    q <- function(x) round(quantile(x, c(.01, .1, .25, .5, .75, .9, .99)), 3)
    cat("\nnon-corp, tau_P>0 firm-periods:", nrow(u), " share e*=0:", round(mean(u$e == 0), 3),
        " mean ln(1+e/M):", round(mean(log1p(u$e / u$x)), 4), "  (real: 0.056)\n")
    cat("median e/M among evaders:", round(median(u$e[u$e > 0] / u$x[u$e > 0]), 3),
        " share e>M:", round(mean(u$e > u$x), 4), " share 2*lam*M*>1 (ceiling):", round(mean(2 * sim$spec$lambda * u$xstar > 1), 3), "\n")
    cat("M* quantiles (real: 326 1360 3208 8508 26446 75560 485189):\n"); print(round(quantile(u$xstar, c(.01, .1, .25, .5, .75, .9, .99))))
    cat("V quantiles (real: -1.32 -.47 -.15 .12 .34 .51 .80):\n"); print(q(u$V))
    cat("Wt quantiles (real median 2.94, IQR 2.35-3.51):\n"); print(q(u$Wt))
    cat("tau_P quantiles (real: 0 .01 .054 .088 .108 .132 .247):\n"); print(q(u$tauP))
    cat("k median/sd (real 8.5 / ~1.5):", round(median(u$k), 2), round(sd(u$k), 2), "\n")
    invisible(NULL)
}

## Calibrate (d0, sigma_psi) to two targets at a given lambda -----------------------------
##   target 1: E ln(1+e/M) over non-corp tau_P>0 firm-periods = mean_target (real: 0.056)
##   target 2: share of e*=0 in that group = share0_target
calibrate <- function(spec, sh, mean_target = 0.056, share0_target = 0.5) {
    stats <- function(d0, sp) {
        s2 <- spec; s2$d0 <- d0; s2$sigma_psi <- sp
        d <- build_panel(s2, sh); u <- d[!d$corp & d$tauP > 0, ]
        c(mean = mean(log1p(u$e / u$x)), share0 = mean(u$e == 0))
    }
    solve_d0 <- function(sp) uniroot(function(d0) stats(d0, sp)["share0"] - share0_target, c(-2, 12), tol = 1e-4)$root
    f <- function(sp) stats(solve_d0(sp), sp)["mean"] - mean_target
    lo <- 0.05; hi <- 4
    flo <- f(lo); fhi <- f(hi)
    if (sign(flo) == sign(fhi)) return(c(d0 = NA_real_, sigma_psi = NA_real_, mean = NA_real_, share0 = NA_real_, f_lo = unname(flo), f_hi = unname(fhi)))
    sp <- uniroot(f, c(lo, hi), tol = 1e-3)$root
    d0 <- solve_d0(sp)
    c(d0 = d0, sigma_psi = sp, stats(d0, sp))
}

if (sys.nframe() == 0) {
    spec <- default_spec()
    sh <- draw_shocks(spec, seed = 1)
    for (s0 in c(0.3, 0.5, 0.7)) {
        cat(sprintf("\nlambda=%.1e  target share0=%.1f -> ", spec$lambda, s0)); print(round(calibrate(spec, sh, 0.056, s0), 4))
    }
}
