## MSL Monte Carlo on the model-faithful DGP (2026-09-18) -----------------------
## For each seed: simulate (1410-dgp-model.R), build the MSL sample (same rules as 1411-sim-export.R),
## fit with the C++ MSL (Version A), record estimates. Truth is the DGP spec.
## Usage: Rscript Code/Deconvolution/1412-msl-mc.R design=monly|withK seeds=10
suppressMessages({library(tidyverse); source("Code/Deconvolution/1410-dgp-model.R"); source("Code/Deconvolution/utils-cli.R")})
opt <- parse_cli_args(list(design = "withK", fomega = "A", seeds = 10, lambda = 1e-4, d0 = 3.837, sigma_psi = 0.66, threads = 4))
log_run_header("1412-msl-mc.R", opt)
spec <- default_spec(); spec$lambda <- opt$lambda; spec$d0 <- opt$d0; spec$sigma_psi <- opt$sigma_psi
if (opt$design == "monly") { spec$aK <- 0; spec$rho <- exp(-3.1) }
fit_seed <- function(seed) {
    sim <- simulate_panel(spec, seed); d <- sim$data %>% filter(!is.na(lag_mstar)); s <- sim$spec
    sd_eps <- s$sd_eps; mu_eps <- -0.5 * sd_eps^2; bo <- 1 - s$beta
    mu_om <- mean(d$Wt) - bo * mu_eps; sd_om <- sqrt(max(var(d$Wt) - bo^2 * sd_eps^2, 0.05 * var(d$Wt)))
    inter <- d %>% filter(!corp, tauP > 0); inter <- inter %>% filter(xstar <= quantile(xstar, 0.995))
    if (opt$fomega %in% c("B", "Bprior")) {
        ## Version B: f(omega_t | W_{t-1}) normal. AR(1) by moments from ALL firms' (W_t, W_{t-1}) pairs.
        pr <- sim$data %>% group_by(i) %>% mutate(Wl = dplyr::lag(Wt)) %>% ungroup() %>% filter(!is.na(Wl))
        s2w <- var(pr$Wt) - bo^2 * sd_eps^2                      # var(omega)
        g1 <- cov(pr$Wt, pr$Wl) / s2w                            # gamma1 = Cov(W_t,W_{t-1})/var(omega)
        g0 <- mu_om * (1 - g1)
        k1 <- s2w / (s2w + bo^2 * sd_eps^2)                      # posterior weight on W_{t-1}
        v1 <- s2w - s2w * k1                                     # Var(omega_{t-1} | W_{t-1})
        inter <- inter %>% mutate(Wl = sim$data$Wt[match(paste(i, t - 1), paste(sim$data$i, sim$data$t))])
        m1 <- mu_om + k1 * (inter$Wl - mu_om - bo * mu_eps)      # E[omega_{t-1} | W_{t-1}]   (posterior; Version B)
        if (opt$fomega == "Bprior") {                            # the OLD derivation: eps_{t-1} integrated against its PRIOR
            m1 <- inter$Wl - bo * mu_eps                         #   omega_{t-1} = W_{t-1} - b*eps_{t-1}, eps ~ N(mu_eps, sd_eps^2) indep. of W
            v1 <- bo^2 * sd_eps^2
        }
        inter$mu_om_i <- g0 + g1 * m1
        inter$sd_om_i <- sqrt(s2w * (1 - g1^2) + g1^2 * v1)
    } else if (opt$fomega == "Cf") {
        ## FEASIBLE Gaussian conditional on (W_{t-1}, k_{t-1}, k_t): eps_t is independent of all three, so regressing W_t = omega_t + bo*eps_t
        ## on them gives omega_t's projection coefficients; var(omega|X) = resid var(W|X) - bo^2 sd_eps^2. No true omega used.
        pr <- sim$data %>% group_by(i) %>% mutate(Wl = dplyr::lag(Wt), kl = dplyr::lag(k)) %>% ungroup() %>% filter(!is.na(Wl))
        fit <- lm(Wt ~ Wl + kl + k, pr)
        prev <- sim$data %>% transmute(i, t = t + 1, Wl = Wt, kl = k)
        inter <- inter %>% left_join(prev, by = c("i", "t"))
        inter$mu_om_i <- predict(fit, newdata = inter) - bo * mu_eps; inter$sd_om_i <- sqrt(sigma(fit)^2 - bo^2 * sd_eps^2)
        cat(sprintf("  [Cf] cond. omega sd=%.3f\n", inter$sd_om_i[1]))
    } else if (opt$fomega %in% c("C", "Bo")) {
        ## ORACLE Gaussian conditional (uses the TRUE omega from the simulation; a test of WHERE the Version-B bias comes from,
        ## not a feasible estimator): linear projection of omega_t on the information set, residual sd constant.
        ##   "Bo": info = W_{t-1} only   (Version B without its moment-estimation error)
        ##   "C" : info = (W_{t-1}, k_{t-1}, k_t)   (adds the capital signal about omega_{t-1}; k_t = kap0+kap1 k_{t-1}+kap2 omega_{t-1}+nu_t)
        pr <- sim$data %>% group_by(i) %>% mutate(Wl = dplyr::lag(Wt), kl = dplyr::lag(k)) %>% ungroup() %>% filter(!is.na(Wl))
        fm <- if (opt$fomega == "C") omega ~ Wl + kl + k else omega ~ Wl
        fit <- lm(fm, pr)
        prev <- sim$data %>% transmute(i, t = t + 1, Wl = Wt, kl = k)
        inter <- inter %>% left_join(prev, by = c("i", "t"))
        inter$mu_om_i <- predict(fit, newdata = inter); inter$sd_om_i <- sigma(fit)
        cat(sprintf("  [%s] cond. omega: R2=%.3f resid sd=%.3f (unconditional sd_om=%.3f)\n", opt$fomega, summary(fit)$r.squared, sigma(fit), sd_om))
    } else { inter$mu_om_i <- mu_om; inter$sd_om_i <- sd_om }
    f <- tempfile(fileext = ".csv")
    write.csv(inter %>% transmute(V, Wt, Mst = xstar, taur = tauP, beta = s$beta, mu_eps, sd_eps, mu_om = mu_om_i, sd_om = sd_om_i), f, row.names = FALSE, quote = FALSE)
    run_fit <- function(st) {
        out <- system2("Code/C-estimator/msl_estimator", c(paste0("data=", f), "mode=fit", paste0("start=", st),
                       paste0("threads=", opt$threads), "maxeval=3000"), stdout = TRUE)
        line <- grep("^lambda=", out, value = TRUE)
        nll <- as.numeric(sub(".*-logL=([-0-9.eE+]+).*", "\\1", tail(grep("^pass", out, value = TRUE), 1)))
        list(v = as.numeric(str_match_all(line, "=([-0-9.eE+]+)")[[1]][, 2]), nll = nll)
    }
    starts <- c("3e-5,3,3,0.4,-0.5", "3e-4,4.5,4,0.6,0", "1e-5,2,2,0.2,-1")
    fits <- lapply(starts, run_fit)
    best <- fits[[which.min(sapply(fits, `[[`, "nll"))]]
    nll_first <- fits[[1]]$nll
    tv <- paste(c(s$lambda, s$d0, s$d1, s$d2, log(s$sigma_psi)), collapse = ",")
    nll_truth <- as.numeric(sub("-logL = ", "", grep("^-logL", system2("Code/C-estimator/msl_estimator",
                  c(paste0("data=", f), "mode=eval", paste0("par=", tv), paste0("threads=", opt$threads)), stdout = TRUE), value = TRUE)))
    unlink(f); v <- best$v
    tibble(seed = seed, n = nrow(inter), lambda = v[1], delta0 = v[2], delta1 = v[3], delta2 = v[4], sigma_psi = v[5],
           nll_best = best$nll, nll_truth = nll_truth, nll_start1 = nll_first, n_distinct_modes = length(unique(round(sapply(fits, `[[`, "nll"), 0))))
}
res <- map_dfr(seq_len(opt$seeds), fit_seed)
truth <- c(lambda = spec$lambda, delta0 = spec$d0, delta1 = spec$d1, delta2 = spec$d2, sigma_psi = spec$sigma_psi)
print(as.data.frame(res %>% mutate(across(-c(seed, n, n_distinct_modes), ~ signif(., 5)))))
cat("\ndesign:", opt$design, "  seeds:", opt$seeds, "\n")
cat("\nseeds where best fit has LOWER -logL than the truth by >0: ", sum(res$nll_best < res$nll_truth), " of ", nrow(res), "\n")
tab <- rbind(truth = truth, mean = colMeans(res[names(truth)]), sd = apply(res[names(truth)], 2, sd),
             bias_pct = 100 * (colMeans(res[names(truth)]) / truth - 1),
             t_stat = (colMeans(res[names(truth)]) - truth) / (apply(res[names(truth)], 2, sd) / sqrt(nrow(res))))
print(signif(tab, 4))
write.csv(res, sprintf("Code/Products/msl/1412-msl-mc-%s-%s.csv", opt$design, opt$fomega), row.names = FALSE)
