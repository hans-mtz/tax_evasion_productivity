## 2026-10-01: uncertainty for the ballpark elasticity of overreporting w.r.t. tau_P (point estimates: ch07-overreporting-elasticity.R).
## PRODUCT: Code/Products/ch07-overreporting-elasticity-ci.RData (+ console output).
## Elasticity E = [(r87 - r83)/r83] / [tau87/tau83 - 1],  r_t = exp(mu_t) - 1,  mu_t = ch. 7 level coefficient (liable unincorporated vs
## corporations, net share, industry FE). A firm has no elasticity of its own (ratio of means), so the bootstrap resamples PLANTS
## (all their years), refits the level model and recomputes both means on every draw (same spec as 1530's rg_lvl_crp; iid vcov for speed).
## CI: basic ("flipped") bootstrap interval [2E - q_{.975}, 2E - q_{.025}]; percentile interval for comparison.
## Analytic: delta method. V = u - eps, so Var(V) = Var(u) + Var(eps). Two versions of Var(mu_t-hat):
##   "as estimated" (plant-clustered; contains Var(eps)/n, the sampling variance of the mean of V -- what the bootstrap reproduces) and
##   "u only": scaled by |Var(V_t) - Var(eps)| / Var(V_t) (absolute value in case the difference is negative), the variance of the
##   mean of u if u were observed (finite-sample estimand: the average overreporting of these firms).
##   Var(eps) = residual variance of the corporations (they report truthfully, V = -eps). Covariance between mu-hat and tau-hat ignored.
library(tidyverse); library(fixest); library(parallel)
PRODUCTS_DIR <- "Code/Products"; B <- as.integer(Sys.getenv("B", 1000)); NCORE <- as.integer(Sys.getenv("NCORE", 8)); set.seed(20261001)
load(file.path(PRODUCTS_DIR, "921-DD2.RData"))   # wip_df
threshold_cut <- 0.05

d0 <- wip_df %>% filter(!juridical_organization %in% 6:9) %>%
    mutate(s = suppressWarnings(log((nom_mats * (1 - sales_tax_rate_purchases)) / (nom_gross_output - sales_tax_rate_sales * nom_sales))),
           corp = droplevels(corp),
           corp_exempt_year = factor(ifelse(corp == "Corp", "Base", paste(corp, exempt_ind, year, sep = ":"))),
           in_reg = is.finite(s) & s > log(threshold_cut),
           in_tau = corp == "Other" & exempt_ind == "Taxed" & is.finite(sales_tax_rate_purchases) &
                    sales_tax_rate_purchases > 0 & sales_tax_rate_purchases < 0.5,
           tau = sales_tax_rate_purchases, yr = as.character(year)) %>%
    filter(in_reg | in_tau) %>% select(plant, sic_3, yr, corp, corp_exempt_year, s, tau, in_reg, in_tau)
N83 <- "corp_exempt_year::Other:Taxed:83"; N87 <- "corp_exempt_year::Other:Taxed:87"

est <- function(d) {
    m <- feols(s ~ i(corp_exempt_year, "Base") | sic_3, data = d[d$in_reg, ], vcov = "iid")
    cf <- coef(m); mu83 <- cf[[N83]]; mu87 <- cf[[N87]]
    t <- d[d$in_tau, ]; tm <- tapply(t$tau, t$yr, mean); td <- tapply(t$tau, t$yr, median)
    A <- (exp(mu87) - 1) / (exp(mu83) - 1) - 1
    c(mu83 = mu83, mu87 = mu87, r83 = exp(mu83) - 1, r87 = exp(mu87) - 1, A = A,
      g_med = td[["87"]] / td[["83"]] - 1, g_mean = tm[["87"]] / tm[["83"]] - 1,
      E_med = A / (td[["87"]] / td[["83"]] - 1), E_mean = A / (tm[["87"]] / tm[["83"]] - 1))
}
point <- est(d0)

## --- bootstrap over plants
idx <- split(seq_len(nrow(d0)), d0$plant); pl <- names(idx)
one <- function(b) { r <- try(est(d0[unlist(idx[sample(pl, length(pl), TRUE)], use.names = FALSE), ]), silent = TRUE)
    if (inherits(r, "try-error")) rep(NA_real_, length(point)) else r }
cat("Bootstrap:", B, "draws,", length(pl), "plants,", NCORE, "cores\n")
bs <- do.call(rbind, mclapply(seq_len(B), one, mc.cores = NCORE, mc.set.seed = TRUE)); colnames(bs) <- names(point)
cat("failed draws:", sum(!complete.cases(bs)), "| draws with r83 <= 0:", sum(bs[, "r83"] <= 0, na.rm = TRUE), "\n")
flip <- function(x, e, a = 0.05) { q <- quantile(x, c(a / 2, 1 - a / 2), na.rm = TRUE); c(lo = 2 * e - q[[2]], hi = 2 * e - q[[1]]) }
pct  <- function(x, a = 0.05) { q <- quantile(x, c(a / 2, 1 - a / 2), na.rm = TRUE); c(lo = q[[1]], hi = q[[2]]) }
boot <- sapply(c("E_med", "E_mean", "A", "r83", "r87", "mu83", "mu87", "g_med", "g_mean"), function(v)
    c(point = point[[v]], se = sd(bs[, v], na.rm = TRUE), flip(bs[, v], point[[v]]), pct(bs[, v])) %>% setNames(c("point", "se", "flip_lo", "flip_hi", "pct_lo", "pct_hi")))

## --- analytic (delta method)
dm <- d0[d0$in_reg, ]; m <- feols(s ~ i(corp_exempt_year, "Base") | sic_3, data = dm, cluster = ~plant)
res <- resid(m); cell <- function(y) dm$corp_exempt_year == paste0("Other:Taxed:", y)
s2e <- var(res[dm$corp == "Corp"]); s2V <- c(`83` = var(res[cell("83")]), `87` = var(res[cell("87")]))
ratio_u <- abs(s2V - s2e) / s2V     # share of Var(V) that is Var(u); absolute value in case Var(V) < Var(eps)
Sig <- vcov(m)[c(N83, N87), c(N83, N87)]
Sig_u <- diag(sqrt(ratio_u)) %*% Sig %*% diag(sqrt(ratio_u))
mu83 <- point[["mu83"]]; mu87 <- point[["mu87"]]; r83 <- point[["r83"]]; r87 <- point[["r87"]]
grA <- c(-r87 * exp(mu83) / r83^2, exp(mu87) / r83)                       # dA/d(mu83, mu87)
dt <- d0[d0$in_tau & d0$yr %in% c("83", "87"), ]
mt <- feols(tau ~ 0 + i(yr), data = dt, cluster = ~plant); St <- vcov(mt); t83 <- coef(mt)[[1]]; t87 <- coef(mt)[[2]]
grg <- c(-t87 / t83^2, 1 / t83); g <- t87 / t83 - 1; A <- point[["A"]]; E <- A / g
varE <- function(S) as.numeric(t(grA) %*% S %*% grA) / g^2 + A^2 * as.numeric(t(grg) %*% St %*% grg) / g^4
ana <- tibble(version = c("as estimated (contains Var(eps))", "u only (Var(eps) removed)"),
              se = sqrt(c(varE(Sig), varE(Sig_u)))) %>% mutate(E_mean = E, lo = E - 1.96 * se, hi = E + 1.96 * se)

options(width = 200, digits = 4)
cat("\nVar(eps) (corps' residual variance):", s2e, "| Var(V) 1983, 1987:", s2V, "| Var(u)/Var(V):", ratio_u, "\n")
cat("Point estimates:\n"); print(round(point, 4))
cat("\nBootstrap (plants), elasticity and ingredients:\n"); print(round(boot, 4))
cat("\nAnalytic delta method, elasticity with the mean tau_P (", sprintf("point %.1f", E), "):\n", sep = ""); print(ana)
save(point, bs, boot, ana, s2e, s2V, file = file.path(PRODUCTS_DIR, "ch07-overreporting-elasticity-ci.RData"))
cat("Saved: Code/Products/ch07-overreporting-elasticity-ci.RData\n")
