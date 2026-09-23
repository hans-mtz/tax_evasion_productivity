## MSL simulation harness (Version A, parametric f_psi) ----------------------
## Spec: Paper/sections/9999-msl-implementation.qmd. Standalone eps-space code,
## deliberately independent of Code/Rcpp/1200-stage2-elvis-common.h (M-space).
##
## 1. Simulate from the model at a known theta (with the e*=0 mass in the DGP).
## 2. Evaluate the MSL log-likelihood by Gauss-Legendre over eps.
## 3. Check recovery of (lambda, delta0, delta1, delta2, sigma_psi), for the
##    full likelihood (interior + e*=0 branch) and the interior-only one.
##
## Usage: Rscript Code/Deconvolution/1400-msl-sim-harness.R n=3000 seed=1

source("Code/Deconvolution/utils-cli.R")
library(statmod)

defaults <- list(
    n       = 3000,
    seed    = 1,
    K       = 150,    # quadrature nodes per firm
    kwid    = 6,      # eps window: mu_eps +- kwid * sd_eps
    maxit   = 600,
    sd_om   = 0.5,    # sd of omega in the DGP (spread drives delta1/delta2 identification)
    lam0    = 0.02,   # true lambda
    d0      = 1.0,    # true delta0 (lower => cheaper evasion => smaller e=0 share)
    mdgp    = "indep", # "indep": true M independent lognormal (NOT the model); "fn_omega": M deterministic in omega (model-faithful)
    interior = TRUE,  # also fit the interior-only likelihood (slow)
    full    = TRUE,   # fit the full likelihood (set FALSE to run only the interior-only fit)
    export  = "",     # if non-empty: write simulated data + R reference per-firm log-lik (truth, full & interior-only) here, then quit
    mode    = "fit"   # "fit" or "quadcheck" (-logL at truth vs. number of quadrature nodes)
)
opt <- parse_cli_args(defaults)
log_run_header("1400-msl-sim-harness.R", opt)
set.seed(opt$seed)

## %% True parameters (scaled so that 2*lambda*e is not negligible) -----------
truth <- list(
    lambda = opt$lam0, delta0 = opt$d0, delta1 = 1.0, delta2 = 0.25, sigma_psi = 0.6,
    beta = 0.6,
    mu_eps = -0.5 * 0.3^2, sd_eps = 0.3,   # E[exp(eps)] = 1
    mu_om = 1.0, sd_om = opt$sd_om
)

## %% DGP -----------------------------------------------------------------------
simulate_msl <- function(n, p) {
    om   <- rnorm(n, p$mu_om, p$sd_om)
    psi  <- rnorm(n, 0, p$sigma_psi)
    eps  <- rnorm(n, p$mu_eps, p$sd_eps)
    M    <- if (opt$mdgp == "fn_omega") {
                exp(3 + (om - p$mu_om) / (1 - p$beta))   # materials FOC: M is a function of omega (and K,L, held const.)
            } else exp(rnorm(n, 3, 0.8))                 # independent lognormal: an EXTRA random draw the likelihood does not model
    taur <- exp(runif(n, log(0.5), log(2)))       # benefit shifter tau_P * rho_t
    Cw   <- p$delta0 - p$delta1 * om + p$delta2 * om^2
    e    <- pmax(0, (1 - exp(Cw + psi) / taur) / (2 * p$lambda))
    Mst  <- M + e
    V    <- log(Mst / M) - eps
    Wt   <- om + (1 - p$beta) * eps
    data.frame(V = V, Wt = Wt, Mst = Mst, taur = taur, e = e, om = om, psi = psi, eps = eps)
}

## %% Log-likelihood ------------------------------------------------------------
## par = c(lambda, delta0, delta1, delta2, log_sigma_psi)
make_msl_ll <- function(d, p, K = 150, kwid = 6, corner = TRUE) {
    gl <- gauss.quad(K, "legendre")
    n  <- nrow(d)
    V <- d$V; Wt <- d$Wt; Mst <- d$Mst; ltr <- log(d$taur)
    bo <- 1 - p$beta

    ## theta-free pieces of the eps grid are computed once per lambda below
    ll_i <- function(par) {
        lam <- par[1]; d0 <- par[2]; d1 <- par[3]; d2 <- par[4]; sp <- exp(par[5])
        if (lam <= 0) return(rep(-1e10, n))

        lo <- pmax(-V, p$mu_eps - kwid * p$sd_eps)
        emax <- ifelse(2 * lam * Mst > 1, -V - log(pmax(1 - 1 / (2 * lam * Mst), 1e-300)), Inf)
        hi <- pmin(emax, p$mu_eps + kwid * p$sd_eps)
        ok <- hi > lo
        lo[!ok] <- 0; hi[!ok] <- 1          # dummy window, contribution zeroed below

        half <- (hi - lo) / 2
        eps  <- lo + outer(half, gl$nodes + 1)              # n x K
        wts  <- outer(half, gl$weights)                     # n x K
        e    <- Mst * (1 - exp(-V - eps))
        den  <- 1 - 2 * lam * e
        den[den <= 1e-12] <- NA_real_
        om   <- Wt - bo * eps
        psi  <- ltr + log(den) - d0 + d1 * om - d2 * om^2
        integrand <- dnorm(psi, 0, sp) * (2 * lam * Mst / den) *
                     dnorm(om, p$mu_om, p$sd_om) * dnorm(eps, p$mu_eps, p$sd_eps)
        integrand[is.na(integrand)] <- 0
        Lint <- rowSums(integrand * wts)
        Lint[!ok] <- 0

        Lcor <- 0
        if (corner) {
            om0  <- Wt + bo * V
            psi0 <- ltr - (d0 - d1 * om0 + d2 * om0^2)
            Lcor <- dnorm(-V, p$mu_eps, p$sd_eps) * dnorm(om0, p$mu_om, p$sd_om) *
                    (1 - pnorm(psi0 / sp))
        }
        log(pmax(Lint + Lcor, 1e-300))
    }
    f <- function(par) -sum(ll_i(par))
    attr(f, "ll_i") <- ll_i
    f
}

## %% Run -----------------------------------------------------------------------
d <- simulate_msl(opt$n, truth)
cat(sprintf("Simulated n=%d: share e=0: %.3f, mean e (e>0): %.3f, max 2*lambda*e: %.3f\n",
            nrow(d), mean(d$e == 0), mean(d$e[d$e > 0]), max(2 * truth$lambda * d$e)))

if (nzchar(opt$export)) {
    tv0 <- c(truth$lambda, truth$delta0, truth$delta1, truth$delta2, log(truth$sigma_psi))
    tv1 <- tv0 * c(1.3, 1, 1, 1, 1) + c(0, 0.2, -0.1, 0.05, 0.1)     # a second, off-truth test point
    ex <- data.frame(V = d$V, Wt = d$Wt, Mst = d$Mst, taur = d$taur, beta = truth$beta,
                     mu_eps = truth$mu_eps, sd_eps = truth$sd_eps, mu_om = truth$mu_om, sd_om = truth$sd_om)
    fF <- attr(make_msl_ll(d, truth, K = opt$K, kwid = opt$kwid, corner = TRUE), "ll_i")
    fI <- attr(make_msl_ll(d, truth, K = opt$K, kwid = opt$kwid, corner = FALSE), "ll_i")
    ex$ll_full_p0 <- fF(tv0); ex$ll_full_p1 <- fF(tv1)
    ex$ll_int_p0  <- fI(tv0); ex$ll_int_p1  <- fI(tv1)
    write.csv(ex, opt$export, row.names = FALSE)
    cat("test points (lambda,delta0,delta1,delta2,log_sigma):\n p0 =", tv0, "\n p1 =", tv1, "\n")
    cat(sprintf("R -logL full p0=%.6f p1=%.6f | interior p0=%.6f p1=%.6f\n",
                -sum(ex$ll_full_p0), -sum(ex$ll_full_p1), -sum(ex$ll_int_p0), -sum(ex$ll_int_p1)))
    quit(save = "no")
}

if (opt$mode == "quadcheck") {
    tv0 <- c(truth$lambda, truth$delta0, truth$delta1, truth$delta2, log(truth$sigma_psi))
    for (K in c(150, 300, 600, 1200, 2400)) {
        f <- make_msl_ll(d, truth, K = K, kwid = opt$kwid, corner = TRUE)
        cat(sprintf("K=%5d  -logL at truth = %.4f\n", K, f(tv0)))
    }
    quit(save = "no")
}

start <- c(truth$lambda * 1.5, truth$delta0 + 0.3, truth$delta1 * 0.8,
           truth$delta2 * 1.3, log(truth$sigma_psi * 1.2))
tv    <- c(truth$lambda, truth$delta0, truth$delta1, truth$delta2, log(truth$sigma_psi))

fit_one <- function(corner) {
    f <- make_msl_ll(d, truth, K = opt$K, kwid = opt$kwid, corner = corner)
    cat(sprintf("\n[%s] -logL at truth: %.3f, at start: %.3f\n",
                if (corner) "full" else "interior-only", f(tv), f(start)))
    t0 <- Sys.time()
    o <- optim(start, f, method = "Nelder-Mead",
               control = list(maxit = opt$maxit, reltol = 1e-10))
    o <- optim(o$par, f, method = "Nelder-Mead",
               control = list(maxit = opt$maxit, reltol = 1e-12))
    cat(sprintf("  conv=%d, -logL=%.3f, time %.1fs\n", o$convergence, o$value,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    est <- c(o$par[1:4], exp(o$par[5]))
    names(est) <- c("lambda", "delta0", "delta1", "delta2", "sigma_psi")
    est
}

res_full <- if (opt$full) fit_one(TRUE) else rep(NA_real_, 5)
res_int  <- if (opt$interior) fit_one(FALSE) else rep(NA_real_, 5)
tab <- rbind(truth = c(truth$lambda, truth$delta0, truth$delta1, truth$delta2, truth$sigma_psi),
             full = res_full, interior_only = res_int)
colnames(tab) <- c("lambda", "delta0", "delta1", "delta2", "sigma_psi")
print(round(tab, 4))
