## Product-mix (4-digit) diagnostic of the benchmark, current definitions (net share GO-t1, codes 6-9 out), 9 industries
suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity")
load("Code/Products/colombia_data.RData")
thr <- log(0.05); inds <- c(313, 321, 322, 324, 331, 342, 351, 352, 369)
d <- colombia_data_frame %>% ungroup() %>%
  filter(sic_3 %in% inds, is.finite(y), is.finite(k), is.finite(l), is.finite(m), !as.integer(as.character(juridical_organization)) %in% 6:9) %>%
  mutate(corp = juridical_organization == 3,
         t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
         s = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1)))) %>%
  filter(is.finite(s), s > thr)
options(width = 250, pillar.sigfig = 4)
cat("== 313 by group x sic4: mean, sd ==\n")
print(d %>% filter(sic_3 == 313) %>% group_by(corp, sic_4) %>% summarise(n = n(), plants = n_distinct(plant), mean = mean(s), sd = sd(s), .groups="drop"))
rw <- d %>% group_by(sic_3) %>% group_modify(function(x, key) {
  lnD <- mean(x$s[x$corp]); muhat <- mean(x$s[!x$corp]) - lnD
  bp <- x %>% group_by(sic_4) %>% summarise(nc = sum(corp), nu = sum(!corp), mc = mean(s[corp]), mu = mean(s[!corp]), .groups = "drop")
  ok <- bp %>% filter(nc >= 10, nu > 0)
  cov <- sum(ok$nu) / sum(bp$nu)
  ## (a) reweight corps to the uninc product mix (cells with >=10 corp obs), (b) fixed effects regression s ~ sic4 + uninc
  rwV <- sum(ok$nu * (ok$mu - ok$mc)) / sum(ok$nu)
  fe <- if (n_distinct(x$sic_4) > 1) coef(lm(s ~ factor(sic_4) + I(!corp), data = x))[["I(!corp)TRUE"]] else muhat
  tibble(n_c = sum(x$corp), n_u = sum(!x$corp), n_sic4 = n_distinct(x$sic_4), muhat = muhat, V_reweighted = rwV,
         uninc_cov = cov, V_sic4FE = fe, var_ratio = var(x$s[!x$corp]) / var(x$s[x$corp]))
}) %>% ungroup()
print(rw %>% mutate(across(where(is.double), \(z) round(z, 3))))
write.csv(rw, "Code/Fact-Checks/audit-313/04-productmix.csv", row.names = FALSE)
## 313: benchmark excluding beer corporations
x <- d %>% filter(sic_3 == 313)
cat(sprintf("313: lnD all corps %.3f | lnD excl. beer (3133) %.3f -> mean V %.3f | uninc soft drinks only vs corp soft drinks %.3f\n",
  mean(x$s[x$corp]), mean(x$s[x$corp & x$sic_4 != 3133]), mean(x$s[!x$corp]) - mean(x$s[x$corp & x$sic_4 != 3133]),
  mean(x$s[!x$corp & x$sic_4 == 3134]) - mean(x$s[x$corp & x$sic_4 == 3134])))
## pooled variance check excluding beer
cat(sprintf("313 var ratio V/eps: all corps %.3f ; corps excl beer %.3f\n", var(x$s[!x$corp]) / var(x$s[x$corp]), var(x$s[!x$corp]) / var(x$s[x$corp & x$sic_4 != 3133])))
