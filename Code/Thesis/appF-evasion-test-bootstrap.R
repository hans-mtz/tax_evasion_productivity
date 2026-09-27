## PRODUCT: Thesis/tables/appF-evasion-test-bootstrap.png := appendix table, bootstrap test for overreporting (resampling fixed)
## Reads: Code/Products/1514-boot-test-fixed.RData -> out_boot (Code/Deconvolution/1514-boot-test-fixed.R): two-tax share;
##        plants resampled within corporations and within unincorporated firms, (plant, legal form) as the unit; ln D
##        re-estimated in every replicate; B = 2000; basic CIs; one-sided p = P(theta* - theta_hat >= theta_hat).
source("Code/Thesis/001-setup.R"); source("Code/Thesis/ch04-test-helpers.R")
load(file.path(PRODUCTS_DIR, "1514-boot-test-fixed.RData"))   # out_boot

d <- out_boot %>% filter(share == "s_net") %>% left_join(test_names, by = "sic_3") %>% arrange(sic_3) %>%
    mutate(stars = case_when(p_one < .01 ~ "***", p_one < .05 ~ "**", p_one < .1 ~ "*", TRUE ~ ""))
stopifnot(nrow(d) == 20, !anyNA(d$name))
tbl <- d %>% transmute(Industry = paste(sic_3, name), `$\\hat\\mu$` = paste0(sprintf("$%.3f$", mu_hat), stars),
                       `95\\% CI` = sprintf("$[%.3f,\\ %.3f]$", lo, hi))
print(tbl, n = Inf)
tt_obj <- tt(tbl, align = "lcc", width = c(4, 1.2, 2),
             notes = "$\\hat\\mu$: mean of $\\mathcal V_{it}$ among unincorporated firms, log materials share net of sales taxes, $\\ln\\hat D$ the mean among corporations. Bootstrap with 2,000 replications: plants resampled separately within corporations and within unincorporated firms, with $\\ln\\hat D$ re-estimated in every replication, so the test carries the first-stage sampling error. Basic (pivotal) 95\\% confidence intervals. *, **, *** denote significance at the 10\\%, 5\\%, and 1\\% levels (one-sided). The intervals are two-sided, so a one-sided rejection at 5\\% can coexist with an interval that includes zero.") %>%
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "appF-evasion-test-bootstrap")
