## Calibrate the DGP with a right-skewed, non-normal psi (2026-09-19).
## psi = k*theta - Gamma(k, theta): mean 0, support (-Inf, k*theta), left-skewed -> e = (1-r)/(2 lambda) right-skewed.
## For a given shape k, solve (delta0, theta) so that, among non-corp tau_P>0 firm-periods:
##   (1) share with e>0  = share_target  (default 0.85)
##   (2) E[e/M] (level, pooled) = mean_target (default 0.059 = exp(mean V)-1 = 5.8% / ratio-of-means 5.9% in the real lag_m interior sample)
## lambda fixed (default 1e-4). Diagnostics reported, not targeted: E ln(1+e/M), exp(.)-1, top-1% share of e, skewness of e among evaders,
## median e/M among evaders, share e>M, share at the FOC ceiling. Usage: Rscript Code/Deconvolution/1415-calibrate-gamma-psi.R ks=0.3,0.5,1,2
suppressMessages({library(tidyverse); source("Code/Deconvolution/1410-dgp-model.R"); source("Code/Deconvolution/utils-cli.R")})
opt <- parse_cli_args(list(ks = "0.3,0.5,1,2", share = 0.85, mean = 0.059, lambda = 1e-4, seed = 1, save_k = ""))
opt$share <- as.numeric(opt$share); opt$mean <- as.numeric(opt$mean); opt$lambda <- as.numeric(opt$lambda)
spec0 <- default_spec(); spec0$lambda <- opt$lambda; spec0$psi_dist <- "gamma"
sh <- draw_shocks(spec0, as.numeric(opt$seed))
stats <- function(d0, theta, k) {
    s2 <- spec0; s2$d0 <- d0; s2$psi_k <- k; s2$psi_theta <- theta; s2$sigma_psi <- sqrt(k) * theta
    d <- build_panel(s2, sh); u <- d[!d$corp & d$tauP > 0, ]
    c(share = mean(u$e > 0), mean_eM = mean(u$e / u$x), mean_ln = mean(log1p(u$e / u$x)))
}
solve_d0 <- function(theta, k) uniroot(function(d0) stats(d0, theta, k)["share"] - opt$share, c(-8, 12), tol = 1e-4)$root
calib <- function(k) {
    f <- function(theta) stats(solve_d0(theta, k), theta, k)["mean_eM"] - opt$mean
    lo <- 0.05; hi <- 3; flo <- f(lo); fhi <- f(hi)
    if (sign(flo) == sign(fhi)) return(list(ok = FALSE, k = k, f_lo = unname(flo), f_hi = unname(fhi)))
    th <- uniroot(f, c(lo, hi), tol = 1e-4)$root
    list(ok = TRUE, k = k, theta = th, d0 = solve_d0(th, k))
}
diag <- function(cf) {
    s2 <- spec0; s2$d0 <- cf$d0; s2$psi_k <- cf$k; s2$psi_theta <- cf$theta; s2$sigma_psi <- sqrt(cf$k) * cf$theta
    sim <- list(data = build_panel(s2, sh), spec = s2, shocks = sh); u <- sim$data[!sim$data$corp & sim$data$tauP > 0, ]
    ev <- u[u$e > 0, ]; ee <- sort(u$e, decreasing = TRUE); m3 <- function(x) mean((x - mean(x))^3) / sd(x)^3
    tibble(k = cf$k, theta = cf$theta, d0 = cf$d0, sd_psi = sqrt(cf$k) * cf$theta, share_evade = mean(u$e > 0), mean_eM = mean(u$e / u$x),
           exp_mean_ln_minus1 = exp(mean(log1p(u$e / u$x))) - 1, top1pct_share_e = sum(ee[seq_len(ceiling(0.01 * length(ee)))]) / sum(ee),
           skew_e_evaders = m3(ev$e), median_eM_evaders = median(ev$e / ev$x), share_e_gt_M = mean(u$e > u$x), share_ceiling = mean(2 * opt$lambda * u$xstar > 1),
           max_2lam_e = max(2 * opt$lambda * u$e)) |> list(sim = sim, spec = s2)
}
res <- list()
for (k in as.numeric(strsplit(opt$ks, ",")[[1]])) {
    cf <- calib(k)
    if (!cf$ok) { cat(sprintf("k=%.2f: NOT feasible in theta in [0.05,3] (f_lo=%.4f, f_hi=%.4f)\n", k, cf$f_lo, cf$f_hi)); next }
    r <- diag(cf); res[[as.character(k)]] <- r
    cat(sprintf("k=%.2f -> theta=%.4f, delta0=%.4f\n", k, cf$theta, cf$d0))
}
tab <- bind_rows(lapply(res, `[[`, 1)); print(as.data.frame(tab), digits = 4)
if (nzchar(opt$save_k)) { r <- res[[as.character(as.numeric(opt$save_k))]]
    f <- sprintf("Code/Products/msl/1415-sim-gammapsi-k%s.rds", opt$save_k); saveRDS(r$sim, f); cat("saved", f, "\n") }
