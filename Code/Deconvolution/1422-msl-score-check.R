## Score-at-truth check of the MSL likelihood on the model-faithful DGP (2026-09-19).
## If the likelihood is correctly specified for the DGP, E[score(theta0)] = 0. Central-difference per-firm scores from
## the C++ estimator's per-firm log-lik (mode=eval out=), cluster-robust (by firm i) z-stats, and the implied
## first-order bias  I^{-1} * mean(score)  (a Newton step from the truth) for each f_omega variant.
## Usage: Rscript Code/Deconvolution/1422-msl-score-check.R design=withK N=20000 fomegas=A,B,Bo,C
suppressMessages({library(tidyverse); source("Code/Deconvolution/1410-dgp-model.R"); source("Code/Deconvolution/utils-cli.R")})
opt <- parse_cli_args(list(design = "withK", N = 20000, seed = 1, fomegas = "A,B,Bo,C", threads = 6, lambda = 1e-4, d0 = 3.837, sigma_psi = 0.66))
log_run_header("1422-msl-score-check.R", opt)
spec <- default_spec(); spec$lambda <- as.numeric(opt$lambda); spec$d0 <- as.numeric(opt$d0); spec$sigma_psi <- as.numeric(opt$sigma_psi); spec$N <- as.numeric(opt$N)
if (opt$design == "monly") { spec$aK <- 0; spec$rho <- exp(-3.1) }
sim <- simulate_panel(spec, as.numeric(opt$seed)); s <- sim$spec
d <- sim$data %>% filter(!is.na(lag_mstar))
sd_eps <- s$sd_eps; mu_eps <- -0.5 * sd_eps^2; bo <- 1 - s$beta
mu_om <- mean(d$Wt) - bo * mu_eps; sd_om <- sqrt(max(var(d$Wt) - bo^2 * sd_eps^2, 0.05 * var(d$Wt)))
inter0 <- d %>% filter(!corp, tauP > 0); inter0 <- inter0 %>% filter(xstar <= quantile(xstar, 0.995))
pr <- sim$data %>% group_by(i) %>% mutate(Wl = dplyr::lag(Wt), kl = dplyr::lag(k)) %>% ungroup() %>% filter(!is.na(Wl))
s2w <- var(pr$Wt) - bo^2 * sd_eps^2; g1 <- cov(pr$Wt, pr$Wl) / s2w; g0 <- mu_om * (1 - g1)
k1 <- s2w / (s2w + bo^2 * sd_eps^2); v1 <- s2w - s2w * k1
prev <- sim$data %>% transmute(i, t = t + 1, Wl = Wt, kl = k)
inter0 <- inter0 %>% left_join(prev, by = c("i", "t"))
cond <- function(v, inter) {
    if (v == "A") return(list(mu = rep(mu_om, nrow(inter)), sd = rep(sd_om, nrow(inter))))
    if (v == "B") { m1 <- mu_om + k1 * (inter$Wl - mu_om - bo * mu_eps); return(list(mu = g0 + g1 * m1, sd = rep(sqrt(s2w * (1 - g1^2) + g1^2 * v1), nrow(inter)))) }
    if (v == "Cf") { fit <- lm(Wt ~ Wl + kl + k, pr); return(list(mu = predict(fit, newdata = inter) - bo * mu_eps, sd = rep(sqrt(sigma(fit)^2 - bo^2 * sd_eps^2), nrow(inter)))) }
    fit <- lm(if (v == "C") omega ~ Wl + kl + k else omega ~ Wl, pr)      # oracle: true omega
    list(mu = predict(fit, newdata = inter), sd = rep(sigma(fit), nrow(inter)))
}
tv <- c(s$lambda, s$d0, s$d1, s$d2, log(s$sigma_psi)); hh <- c(s$lambda * 0.01, 1e-3, 1e-3, 1e-3, 1e-3)
eval_ll <- function(f, par) {
    o <- tempfile(fileext = ".csv")
    system2("Code/C-estimator/msl_estimator", c(paste0("data=", f), "mode=eval", paste0("par=", paste(sprintf("%.15g", par), collapse = ",")),
            paste0("threads=", opt$threads), paste0("out=", o)), stdout = FALSE)
    x <- read.csv(o); unlink(o); x[[ncol(x)]]
}
for (v in strsplit(opt$fomegas, ",")[[1]]) {
    cd <- cond(v, inter0)
    f <- tempfile(fileext = ".csv")
    write.csv(inter0 %>% transmute(V, Wt, Mst = xstar, taur = tauP, beta = s$beta, mu_eps, sd_eps, mu_om = cd$mu, sd_om = cd$sd), f, row.names = FALSE, quote = FALSE)
    S <- sapply(1:5, function(j) { pp <- pm <- tv; pp[j] <- pp[j] + hh[j]; pm[j] <- pm[j] - hh[j]; (eval_ll(f, pp) - eval_ll(f, pm)) / (2 * hh[j]) })
    unlink(f)
    ## cluster (by firm) sums
    Sf <- rowsum(S, inter0$i); nf <- nrow(Sf); m <- colMeans(S)
    Vs <- crossprod(scale(Sf, center = FALSE, scale = FALSE)) / nf         # cluster-robust variance of the per-firm score SUM (B-hat)
    se <- sqrt(diag(Vs) / nf) / (nrow(S) / nf)                              # SE of mean score, per-row scale
    Ihat <- crossprod(S) / nrow(S)                                          # outer-product information
    step <- solve(Ihat, m)                                                  # I^{-1} * mean score = first-order bias (Newton step from truth)
    cat(sprintf("\n== f_omega %s  (rows=%d, firms=%d)\n", v, nrow(S), nf))
    tab <- rbind(mean_score = m, cluster_z = m / se, newton_step = step, step_pct_of_truth = 100 * step / c(tv[1:4], 1))
    colnames(tab) <- c("lambda", "delta0", "delta1", "delta2", "log_sigma"); print(signif(tab, 3))
}
