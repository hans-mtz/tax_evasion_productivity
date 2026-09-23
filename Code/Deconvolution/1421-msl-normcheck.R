## MSL normalization check (2026-09-19): full density (interior + e*=0 branch) integrated over (V, Wt) at fixed TRUE M (the
## conditioning the Jacobian was derived under; NOT fixed M*), s=ln(1-2 lam e) rule, Version A (uncond f_omega) vs B (f_omega | W_{t-1}).
## Result: 1.0000 in 43/45 configs (truth, off-truth, lam 1e-6..1e-3, degenerate point); 2 tiny-lam,M=20 cases 0.98 = V-grid edge truncation.
## Usage: Rscript Code/Deconvolution/1421-msl-normcheck.R
## Normalization check: full MSL density (interior + e*=0 branch) integrated over (V, Wt), for fixed
## (M*, tau_P*rho) and per-firm omega-density (mu_om_i, sd_om_i). Version A = unconditional, B = conditional on W_{t-1}.
suppressMessages(library(statmod)); library(parallel)
K <- 150; gl <- gauss.quad(K, "legendre")
bo <- 1 - 0.55; sd_eps <- 0.42; mu_eps <- -0.5*sd_eps^2; mu_om <- 2.94; sd_om <- 0.85; g1 <- 0.9
gS <- gauss.quad(300, "legendre")
ll_dens <- function(V, Wt, M, taur, mo, so, par, kwid = 6) {   # s = ln(1-2 lam e) quadrature (the C++ default rule); M fixed
    lam <- par[1]; d0 <- par[2]; d1 <- par[3]; d2 <- par[4]; sp <- par[5]; n <- length(V); ltr <- log(taur)
    ## window in s: e>=0 <=> s<=0 ; eps window => e in [M(exp(V+mu-kw*sd)-1)+, M(exp(V+mu+kw*sd)-1)]
    e_lo <- pmax(0, M*(exp(V + mu_eps - kwid*sd_eps) - 1)); e_hi <- pmax(M*(exp(V + mu_eps + kwid*sd_eps) - 1), 0)
    s_hi <- log(pmax(1 - 2*lam*e_lo, 1e-300)); s_lo <- log(pmax(1 - 2*lam*e_hi, 1e-300))
    ok <- e_hi > e_lo & (2*lam*e_hi < 1 | TRUE); s_lo <- pmax(s_lo, s_hi - 60)
    s_lo[!ok] <- -1; s_hi[!ok] <- 0
    half <- (s_hi - s_lo)/2; sg <- s_lo + outer(half, gS$nodes+1); w <- outer(half, gS$weights)
    e <- (1 - exp(sg))/(2*lam); Ms <- M + e; eps <- log(Ms/M) - V
    om <- Wt - bo*eps; psi <- ltr + sg - d0 + d1*om - d2*om^2
    integrand <- dnorm(psi, 0, sp) * dnorm(om, mo, so) * dnorm(eps, mu_eps, sd_eps)
    Lint <- rowSums(integrand*w); Lint[!ok] <- 0
    om0 <- Wt + bo*V; psi0 <- ltr - (d0 - d1*om0 + d2*om0^2)
    Lcor <- dnorm(-V, mu_eps, sd_eps)*dnorm(om0, mo, so)*(1 - pnorm(psi0/sp))
    Lint + Lcor
}
integ <- function(Mst, taur, mo, so, par, nV = 2600, nW = 700, Vr = c(-3, 10)) {
    Vg <- seq(Vr[1], Vr[2], length.out = nV); Wg <- seq(mo - 8*so, mo + 8*so, length.out = nW)
    dV <- Vg[2]-Vg[1]; dW <- Wg[2]-Wg[1]; tot <- 0; totI <- 0
    for (j in seq_len(nV)) {
        L <- ll_dens(rep(Vg[j], nW), Wg, Mst, taur, mo, so, par); tot <- tot + sum(L)*dV*dW
    }
    tot
}
## conditional omega_t | W_{t-1} (Version B), from 1412: k1 posterior weight, etc.
s2w <- sd_om^2; k1 <- s2w/(s2w + bo^2*sd_eps^2); v1 <- s2w - s2w*k1
condB <- function(Wl) { m1 <- mu_om + k1*(Wl - mu_om - bo*mu_eps); c(mu_om*(1-g1) + g1*m1, sqrt(s2w*(1-g1^2) + g1^2*v1)) }
pars <- list(truth = c(1e-4, 3.837, 3.5, 0.5, 0.66), tinyLam = c(1e-6, 3.837, 3.5, 0.5, 0.66),
             bigLam = c(1e-3, 3.837, 3.5, 0.5, 0.66), degen = c(0.5, -12, 0.01, 0.01, 0.66),
             offtruth = c(1.5e-4, 4.2, 3.0, 0.6, 0.8))
cfg <- expand.grid(par = names(pars), Mst = c(20, 150, 2000), Wl = c(1.5, 2.94, 4.5), taur = c(0.088), stringsAsFactors = FALSE)
res <- mclapply(seq_len(nrow(cfg)), function(i) {
    c_ <- cfg[i, ]; p <- pars[[c_$par]]; cb <- condB(c_$Wl)
    c(A = integ(c_$Mst, c_$taur, mu_om, sd_om, p), B = integ(c_$Mst, c_$taur, cb[1], cb[2], p))
}, mc.cores = 10)
out <- cbind(cfg, do.call(rbind, res)); print(out, digits = 5)
saveRDS(out, "Code/Products/msl/1421-normcheck.rds")
