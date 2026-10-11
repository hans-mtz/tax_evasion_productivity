## Audit 313 (2026-10-11): the model implies Var(V | unincorporated) >= Var(eps) (V = u - eps, u independent of eps),
## with Var(eps) = Var(V | corporation). Ratio Var(s | uninc) / Var(s | corp) by industry, current test definitions
## (net share GO - t1, codes 6-9 dropped, share > 5 percent, as 1510), 20 tested industries. Bootstrap: resample
## (plant, legal form) clusters with replacement, separately within corporations and unincorporated firms; B = 999.
## Reports the ratio, the percentile 95 percent interval and the share of replications with ratio >= 1.
suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity")
load("Code/Products/colombia_data.RData"); load("Code/Products/global_vars.RData")
set.seed(20261011); B <- 999; thr <- log(0.05)
d <- colombia_data_frame %>% ungroup() %>%
  filter(is.finite(y), is.finite(k), is.finite(l), is.finite(m), sic_3 %in% top_20_inds$sic_3, juridical_organization %in% 0:5) %>%
  mutate(corp = juridical_organization == 3, t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
         s = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1)))) %>%
  filter(is.finite(s), s > thr) %>% mutate(cl = paste(plant, corp))
boot_ratio <- function(x) {
  r0 <- var(x$s[!x$corp]) / var(x$s[x$corp])
  idx <- split(seq_len(nrow(x)), x$cl); grp <- tapply(x$corp, x$cl, first)
  cc <- names(idx)[grp[names(idx)]]; cu <- names(idx)[!grp[names(idx)]]
  rb <- replicate(B, {
    ic <- unlist(idx[sample(cc, length(cc), TRUE)], use.names = FALSE)
    iu <- unlist(idx[sample(cu, length(cu), TRUE)], use.names = FALSE)
    var(x$s[iu]) / var(x$s[ic])
  })
  tibble(n_corp = sum(x$corp), n_uninc = sum(!x$corp), ratio = r0, lo = quantile(rb, .025), hi = quantile(rb, .975),
         share_ge1 = mean(rb >= 1))
}
res <- d %>% group_by(sic_3) %>% group_modify(~ boot_ratio(.x)) %>% ungroup()
options(width = 200, pillar.sigfig = 3); print(res, n = Inf)
write.csv(res, "Code/Fact-Checks/audit-313/09-variance-ratio.csv", row.names = FALSE)
