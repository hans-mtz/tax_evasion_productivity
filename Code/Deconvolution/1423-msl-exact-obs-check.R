## Exact-observables likelihood vs. the current eps-space likelihood (2026-09-19).
## Observables given (k_t, W_{t-1}, k_{t-1}, tau_P): (V, M*)  [W is redundant: W = bo*(ln M* - V) - ln(beta) - aK*k exactly].
## Latents (omega, psi, eps); M = g(omega,k) = exp((ln beta + aK k + omega)/bo). Given omega: e = M* - g, psi = ln tau + ln(1-2 lam e) - C(omega),
## eps = ln(M*/g) - V. Jacobian of (psi,eps) wrt (M*,V) at fixed omega = |h'(e)| = 2 lam/(1-2 lam e). So
##   L_exact(V,M*) = int f_omega(omega|I) f_psi(psi) f_eps(eps) 2 lam/(1-2 lam e) d omega  +  corner: f_eps(-V) f_omega(omega0|I) (bo/M*) [1-F_psi]
## Claim: L_exact = L_epsspace * (bo/M*), a THETA-FREE factor (d omega = bo d eps at fixed W; J_old = 2 lam M*/den). Check numerically.
suppressMessages({library(tidyverse); library(statmod); source("Code/Deconvolution/1410-dgp-model.R"); source("Code/Deconvolution/utils-cli.R")})
opt <- parse_cli_args(list(N = 600, seed = 1, K = 3000, threads = 6))
spec <- default_spec(); spec$N <- as.numeric(opt$N); sim <- simulate_panel(spec, as.numeric(opt$seed)); s <- sim$spec
bo <- 1 - s$beta; sd_eps <- s$sd_eps; mu_eps <- -0.5 * sd_eps^2
d <- sim$data %>% filter(!is.na(lag_mstar), !corp, tauP > 0)
pr <- sim$data %>% group_by(i) %>% mutate(Wl = dplyr::lag(Wt), kl = dplyr::lag(k)) %>% ungroup() %>% filter(!is.na(Wl))
fit <- lm(omega ~ Wl + kl + k, pr)                                    # oracle conditional (Gaussian) for this equivalence check
prev <- sim$data %>% transmute(i, t = t + 1, Wl = Wt, kl = k); d <- d %>% left_join(prev, by = c("i", "t")) %>% filter(!is.na(Wl))
d$mo <- predict(fit, newdata = d); so <- sigma(fit)
f <- tempfile(fileext = ".csv")
write.csv(d %>% transmute(V, Wt, Mst = xstar, taur = tauP, beta = s$beta, mu_eps, sd_eps, mu_om = mo, sd_om = so), f, row.names = FALSE, quote = FALSE)
cpp <- function(par) { o <- tempfile(fileext = ".csv"); system2("Code/C-estimator/msl_estimator", c(paste0("data=", f), "mode=eval", paste0("par=", paste(sprintf("%.15g", par), collapse = ",")), paste0("threads=", opt$threads), paste0("out=", o)), stdout = FALSE); x <- read.csv(o); unlink(o); x[[ncol(x)]] }
gl <- gauss.quad(as.numeric(opt$K), "legendre")
exact <- function(par, dd) {                                            # per-firm log L_exact by quadrature over omega
    lam <- par[1]; d0 <- par[2]; d1 <- par[3]; d2 <- par[4]; sp <- exp(par[5])
    sapply(seq_len(nrow(dd)), function(r) {
        x <- dd[r, ]; Ms <- x$xstar; lt <- log(x$tauP)
        om0 <- bo * log(Ms) - log(s$beta) - s$aK * x$k                  # omega at which g = M*  (e = 0)
        lo <- x$mo - 9 * so; hi <- om0; if (hi <= lo) return(NA_real_)
        half <- (hi - lo) / 2; om <- lo + half * (gl$nodes + 1); w <- half * gl$weights
        g <- exp((log(s$beta) + s$aK * x$k + om) / bo); e <- Ms - g; den <- 1 - 2 * lam * e
        ok <- den > 1e-12; eps <- log(Ms / g) - x$V
        psi <- lt + log(pmax(den, 1e-300)) - (d0 - d1 * om + d2 * om^2)
        Lint <- sum(ifelse(ok, dnorm(psi, 0, sp) * dnorm(eps, mu_eps, sd_eps) * dnorm(om, x$mo, so) * 2 * lam / pmax(den, 1e-300), 0) * w)
        psi0 <- lt - (d0 - d1 * om0 + d2 * om0^2)
        Lcor <- dnorm(-x$V, mu_eps, sd_eps) * dnorm(om0, x$mo, so) * (bo / Ms) * (1 - pnorm(psi0 / sp))
        log(Lint + Lcor)
    })
}
sub <- which(2 * s$lambda * d$xstar < 0.4)[1:min(300, sum(2 * s$lambda * d$xstar < 0.4))]     # ceiling-free subset for a clean quadrature comparison
ths <- list(truth = c(s$lambda, s$d0, s$d1, s$d2, log(s$sigma_psi)), off = c(1.4e-4, 4.2, 3.2, 0.55, log(0.8)))
for (nm in names(ths)) {
    ex <- exact(ths[[nm]], d[sub, ]); oldl <- cpp(ths[[nm]])[sub]
    dif <- ex - oldl - log(bo / d$xstar[sub])
    cat(sprintf("%-6s : ln L_exact - ln L_eps-space - ln(bo/M*)  ->  mean %.2e  max|.| %.2e   (n=%d)\n", nm, mean(dif, na.rm = TRUE), max(abs(dif), na.rm = TRUE), sum(!is.na(dif))))
}
