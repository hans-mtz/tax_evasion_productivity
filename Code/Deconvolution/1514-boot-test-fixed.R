## PRODUCT: Code/Products/1514-boot-test-fixed.{csv,RData} := the approved bootstrap test for overreporting (206-boot-test.R,
## "fix corps, others": plants resampled separately within corporations and within unincorporated firms, ln D re-estimated
## on the resampled corporations in every replicate, mean of V over unincorporated firms), with its resampling bug fixed.
##
## The bug (found 2026-09-26): resample_by_group (021-deconv-funs.R) draws plants within (industry, legal form) but joins the
## draws back to the data by (industry, plant) only, so a plant that switches legal form brings its rows from the OTHER group
## along. Replicates are then not centred on the estimate (e.g. 324: mean 0.035 vs 0.070), and the one-sided rule
## p = P(theta* - theta_hat >= theta_hat) rejects too often. Fix: the resampling unit is the (plant, legal form) cluster.
## Verified: with switching plants removed, 206's own code gives centred replicates.
##
## Same sample as 1510 (test_data rules, share > 5%, top 20); both shares (gross, net-of-tax). B = 2000 (206 used 250).
## Basic (pivotal) 95% CI; one-sided p-value as in 206's render_tbl.
## Sample (2026-09-26, PLAN.md §9a option b): juridical organization codes 6-9 dropped (6 stock partnerships, taxed as
## corporations; 7-9 cooperatives, state enterprises and other entities). Corporations = 3; unincorporated = 0, 1, 2, 4, 5.
library(tidyverse); library(parallel)
load("Code/Products/colombia_data.RData"); load("Code/Products/global_vars.RData")
threshold_cut <- 0.05; B <- 2000
mc_cores <- max(1, parallel::detectCores() - 2)

base <- colombia_data_frame %>% ungroup() %>%
    filter(is.finite(y), is.finite(k), is.finite(l), is.finite(m), sic_3 %in% top_20_inds$sic_3,
           !juridical_organization %in% 6:9) %>%
    mutate(corp = juridical_organization == 3,
           t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
           s_gross = log(nom_mats / nom_gross_output),
           s_net   = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1)))) %>%
    select(sic_3, plant, year, corp, s_gross, s_net)

run_one <- function(sic, share, seed) {
    set.seed(seed)
    d <- base %>% filter(sic_3 == sic, is.finite(.data[[share]]), .data[[share]] > log(threshold_cut))
    pc <- split(d[[share]][d$corp], d$plant[d$corp]); pu <- split(d[[share]][!d$corp], d$plant[!d$corp])
    th <- mean(d[[share]][!d$corp]) - mean(d[[share]][d$corp])
    bs <- replicate(B, mean(unlist(sample(pu, replace = TRUE))) - mean(unlist(sample(pc, replace = TRUE))))
    p <- mean(bs - th >= th)
    tibble(sic_3 = as.character(sic), share = share, mu_hat = th, boot_mean = mean(bs), boot_sd = sd(bs),
           lo = 2 * th - quantile(bs, .975, names = FALSE), hi = 2 * th - quantile(bs, .025, names = FALSE), p_one = p)
}
jobs <- expand.grid(sic = top_20_inds$sic_3, share = c("s_gross", "s_net"), stringsAsFactors = FALSE)
out_boot <- mcmapply(run_one, jobs$sic, jobs$share, seq_len(nrow(jobs)) + 66636, SIMPLIFY = FALSE,
                     mc.cores = mc_cores) %>% bind_rows() %>% arrange(sic_3, share)
write.csv(out_boot, "Code/Products/1514-boot-test-fixed.csv", row.names = FALSE)
save(out_boot, file = "Code/Products/1514-boot-test-fixed.RData")
options(width = 200)
print(out_boot %>% mutate(across(where(is.numeric), \(x) round(x, 3))), n = Inf)
cat("Saved: Code/Products/1514-boot-test-fixed.{csv,RData}\n")
