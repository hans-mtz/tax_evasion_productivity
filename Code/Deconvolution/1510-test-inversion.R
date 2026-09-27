## PRODUCT: Code/Products/1510-test-inversion.{csv,RData} := test-inversion confidence sets for mean overreporting,
## mu = E[V | unincorporated] = E[u], by industry (top 20), single-tax (gross) and two-tax (net-of-tax) log materials share.
## The "§4.3 to-do" of Thesis/PLAN.md, and the first step of checking whether the two-tax model breaks the first stages.
##
## Headline (PLAN.md §4.3 spec): ln D-hat = mean log share of corporations, taken as the truth and held FIXED. Grid
## mu in [0, 1] (the model's parameter space: u >= 0). Moments per plant-year i:
##   g1 = 1{corp}  (s - lnD_hat)          g2 = 1{uninc} (s - lnD_hat - mu)
## Plant-clustered CENTRED covariance Omega(mu) at each candidate (the convention of the PF step 1512/1513 and of ELVIS;
## switched from uncentred 2026-09-26: same size under H0, more power; only verdict change 351, which now just rejects); TS(mu) = n gbar' Omega^{-1} gbar. Conservative:
## chi2_2 (nothing profiled). Sharp: chi2_1 (g1 holds exactly at lnD_hat, which is estimated from it, so only g2 is
## informative). The region is the set of passing grid points; if mu = 0 passes, no overreporting cannot be
## rejected and the lower end is left open; if no point in [0, 1] passes, the region is empty.
## Secondary (sensitivity only): ln D profiled at each mu (carries the corporations' sampling error), chi2_1, same grid.
##
## Sample: the test's own (206-boot-test.R / 001-data.R): finite y, k, l, m; share > threshold_cut (5%); no upper cut for
## 369 (the approved test has none). Two-tax share as in 1501: log((nom_mats - t2) / (nom_gross_output - t1)),
## t1 = tau_S * nom_sales, t2 = tau_P * nom_mats; the 5% rule applied to each share separately (log of a negative net
## share, where tau_P > 1, gives NaN and the row is dropped, as in 1501).
## Sample (2026-09-26, PLAN.md §9a option b): juridical organization codes 6-9 dropped (6 stock partnerships, taxed as
## corporations; 7-9 cooperatives, state enterprises and other entities). Corporations = 3; unincorporated = 0, 1, 2, 4, 5.
library(tidyverse); library(parallel)
load("Code/Products/colombia_data.RData"); load("Code/Products/global_vars.RData")
threshold_cut <- 0.05
mc_cores <- max(1, parallel::detectCores() - 2)

base <- colombia_data_frame %>% ungroup() %>%
    filter(is.finite(y), is.finite(k), is.finite(l), is.finite(m), sic_3 %in% top_20_inds$sic_3,
           !juridical_organization %in% 6:9) %>%
    mutate(corp = juridical_organization == 3,
           t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
           s_gross = log(nom_mats / nom_gross_output),
           s_net   = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1)))) %>%
    select(sic_3, plant, year, corp, s_gross, s_net)

## TS at (a, mu): n gbar' Omega^{-1} gbar, plant-clustered, centred
ts_at <- function(a, mu, s, corp, cl) {
    n <- length(s)
    g <- cbind(corp * (s - a), (!corp) * (s - a - mu))
    gb <- colMeans(g); G <- rowsum(g, cl); G <- G - outer(as.vector(table(cl)[rownames(G)]), gb)
    drop(n * t(gb) %*% solve(crossprod(G) / n, gb))
}

grid_mu <- seq(0, 1, by = 0.001)
region <- function(TS, crit) {
    p <- grid_mu[TS <= crit]
    if (!length(p)) return(c(lo = NA, hi = NA, open = NA))
    c(lo = min(p), hi = max(p), open = min(p) == 0)
}
run_one <- function(sic, share) {
    d <- base %>% filter(sic_3 == sic, is.finite(.data[[share]]), .data[[share]] > log(threshold_cut))
    s <- d[[share]]; corp <- d$corp; cl <- d$plant
    lnD <- mean(s[corp]); mu_hat <- mean(s[!corp]) - lnD
    TS_fix  <- sapply(grid_mu, \(mu) ts_at(lnD, mu, s, corp, cl))
    TS_prof <- sapply(grid_mu, \(mu) optimize(ts_at, lnD + c(-.5, .5), mu = mu, s = s, corp = corp, cl = cl,
                                              tol = 1e-10)$objective)
    f <- region(TS_fix, qchisq(.95, 2)); f1 <- region(TS_fix, qchisq(.95, 1)); p <- region(TS_prof, qchisq(.95, 1))
    tibble(sic_3 = as.character(sic), share = share, n_corp = sum(corp), n_uninc = sum(!corp),
           corp_plants = length(unique(cl[corp])), lnD = lnD, mu_hat = mu_hat,
           TS0 = TS_fix[1], p0 = 1 - pchisq(TS_fix[1], 2),
           lo = f[["lo"]], hi = f[["hi"]], open = as.logical(f[["open"]]),
           sh_lo = f1[["lo"]], sh_hi = f1[["hi"]], sh_open = as.logical(f1[["open"]]),
           prof_TS0 = TS_prof[1], prof_lo = p[["lo"]], prof_hi = p[["hi"]], prof_open = as.logical(p[["open"]]),
           hits_top = any(c(f[["hi"]], p[["hi"]]) %in% 1, na.rm = TRUE))
}

jobs <- expand.grid(sic = top_20_inds$sic_3, share = c("s_gross", "s_net"), stringsAsFactors = FALSE)
out <- mcmapply(run_one, jobs$sic, jobs$share, SIMPLIFY = FALSE, mc.cores = mc_cores) %>% bind_rows() %>%
    arrange(sic_3, share)
stopifnot(!any(out$hits_top))

write.csv(out, "Code/Products/1510-test-inversion.csv", row.names = FALSE)
save(out, file = "Code/Products/1510-test-inversion.RData")
fmt <- \(lo, hi, open) ifelse(is.na(lo), "empty", ifelse(open, sprintf("(, %.3f]", hi), sprintf("[%.3f, %.3f]", lo, hi)))
options(width = 220)
print(out %>% transmute(sic_3, share, corp_plants, mu_hat = round(mu_hat, 3), TS0 = round(TS0, 2), p0 = round(p0, 3),
                        sharp = fmt(sh_lo, sh_hi, sh_open), conservative = fmt(lo, hi, open), region_profiled = fmt(prof_lo, prof_hi, prof_open)), n = Inf)
cat("Saved: Code/Products/1510-test-inversion.{csv,RData}\n")
