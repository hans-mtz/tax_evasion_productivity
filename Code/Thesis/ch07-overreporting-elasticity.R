## 2026-10-01: ballpark elasticity of overreporting with respect to the purchases-side sales-tax rate tau_P, 1983 -> post-reform.
## PRODUCT: Code/Products/ch07-overreporting-elasticity.csv (console output for the abstract; no figure or table asset).
## Overreporting: ch. 7 event study (1530-fiscal-did.RData, NET share), unincorporated firms in ST-liable industries relative to
## corporations. The level coefficient mu_t = E[u_t] is the average log ratio M*/M; the ratio of overreporting to true materials is
## exp(mu_t) - 1 (never exp(Delta mu) - 1, which is the wrong conversion for a difference; log 2026-09-27).
## Tax rate: tau_P = sales tax paid on purchases / raw materials (the rate in the evasion FOC), liable unincorporated firms,
## 0 < tau_P < 50% as in ch03-sales-tax-by-year-table.R. Median is the headline (as in ch. 3); mean as a check.
## Caveat: this is a reduced-form arc elasticity of a response that also reflects the income-tax cut (opposite sign for
## proprietorships) and the 1984-87 phase-in; it is not the structural elasticity of the counterfactual.
library(tidyverse); library(fixest)
PRODUCTS_DIR <- "Code/Products"
load(file.path(PRODUCTS_DIR, "921-DD2.RData"))          # wip_df
load(file.path(PRODUCTS_DIR, "1530-fiscal-did.RData"))  # did$net
BASE <- "83"; YEARS <- c("84", "85", "86", "87", "88")

## --- overreporting: level (anchor) and difference to 1983, liable unincorporated, net share
lvl <- coeftable(did$net$rg_lvl_crp)
dif <- coeftable(did$net$rg_lvl_b83_crp)
mu   <- function(y) lvl[paste0("corp_exempt_year::Other:Taxed:", y), "Estimate"]
dmu  <- function(y) dif[paste0("corp_exempt_y83::Other:Taxed:", y), c("Estimate", "Std. Error")]
ratio <- function(y) exp(mu(y)) - 1   # overreporting / true materials

## --- tax rate
tau <- wip_df %>%
    filter(!juridical_organization %in% 6:9, corp == "Other", exempt_ind == "Taxed",
           is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0, sales_tax_rate_purchases < 0.5) %>%
    group_by(year) %>%
    summarise(n = n(), tau_med = median(sales_tax_rate_purchases), tau_mean = mean(sales_tax_rate_purchases), .groups = "drop") %>%
    mutate(year = as.character(year))
t83 <- filter(tau, year == BASE)

res <- map_dfr(YEARS, function(y) {
    ty <- filter(tau, year == y); d <- dmu(y)
    dr <- ratio(y) - ratio(BASE)                       # change in the ratio, in ratio units
    tibble(year = y,
           ratio83 = ratio(BASE), ratio_t = ratio(y),
           d_ratio_pp = 100 * dr, se_d_ratio_pp = 100 * exp(mu(y)) * d[["Std. Error"]],   # delta method, difference-model SE
           tau83_med = t83$tau_med, tau_t_med = ty$tau_med,
           pct_dtau_med = 100 * (ty$tau_med / t83$tau_med - 1),
           pct_dtau_mean = 100 * (ty$tau_mean / t83$tau_mean - 1),
           ## (0) point-to-point elasticity: % change in the ratio over % change in tau_P (the standard definition)
           pct_d_ratio = 100 * dr / ratio(BASE),
           eps_pct_med  = pct_d_ratio / pct_dtau_med,
           eps_pct_mean = pct_d_ratio / pct_dtau_mean,
           ## (1) semi-elasticity: percentage points of true materials per 1% change in tau_P
           semi_med  = d_ratio_pp / pct_dtau_med,
           semi_mean = d_ratio_pp / pct_dtau_mean,
           ## (2) log-log arc elasticity: d ln(ratio) / d ln(tau_P); base-sensitive because the 1983 ratio is small
           eps_loglog_med = log(ratio_t / ratio83) / log(ty$tau_med / t83$tau_med),
           ## (3) elasticity of the log ratio u itself: d mu / d ln tau_P  (mu = E[ln(M*/M)], base-free)
           eps_u_med = d[["Estimate"]] / log(ty$tau_med / t83$tau_med))
})
options(width = 220, digits = 3)
cat("Tax rate tau_P (liable unincorporated), by year:\n"); print(tau, n = Inf)
cat("\nOverreporting level mu_t (liable unincorporated, net share):", sprintf("1983 %.4f, 1987 %.4f", mu("83"), mu("87")), "\n")
cat("Ratio to true materials:", sprintf("1983 %.1f%%, 1987 %.1f%%", 100 * ratio("83"), 100 * ratio("87")), "\n\n")
print(res %>% mutate(across(where(is.numeric), ~ signif(.x, 3))), n = Inf, width = Inf)

## Headline: 1987, the first year the response reaches its plateau (ch. 7)
h <- filter(res, year == "87")
cat(sprintf(paste0("\nHEADLINE (1983 -> 1987): tau_P (median) %.1f%% -> %.1f%% (+%.0f%%; mean +%.0f%%); overreporting %.1f%% -> %.1f%% of true materials (+%.1f pp, SE %.1f).\n",
                   "  ELASTICITY (%% change in ratio / %% change in tau_P): ratio +%.0f%% -> %.1f (median rate), %.1f (mean rate)\n  semi-elasticity: %.2f pp of true materials per 1%% rise in tau_P (mean-rate version %.2f)\n",
                   "  elasticity of u (log ratio) w.r.t. ln tau_P: %.2f\n",
                   "  log-log arc elasticity of the ratio: %.1f (base-sensitive)\n"),
            100 * h$tau83_med, 100 * h$tau_t_med, h$pct_dtau_med, h$pct_dtau_mean, 100 * h$ratio83, 100 * h$ratio_t,
            h$d_ratio_pp, h$se_d_ratio_pp, h$pct_d_ratio, h$eps_pct_med, h$eps_pct_mean, h$semi_med, h$semi_mean, h$eps_u_med, h$eps_loglog_med))
write.csv(res, file.path(PRODUCTS_DIR, "ch07-overreporting-elasticity.csv"), row.names = FALSE)
cat("Saved: Code/Products/ch07-overreporting-elasticity.csv\n")
