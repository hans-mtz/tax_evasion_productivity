## PRODUCT: Thesis/tables/appF-evasion-test-conservative.png := appendix table, test for overreporting, conservative region
## Reads: Code/Products/1510-test-inversion.RData -> out: two-tax share, ln D fixed, grid mu in [0,1], chi2_2 (nothing profiled).
source("Code/Thesis/001-setup.R"); source("Code/Thesis/ch04-test-helpers.R")
load(file.path(PRODUCTS_DIR, "1510-test-inversion.RData"))   # out

d <- out %>% filter(share == "s_net") %>% left_join(test_names, by = "sic_3") %>% arrange(sic_3)
stopifnot(nrow(d) == 20, !anyNA(d$name))
tbl <- d %>% transmute(Industry = paste(sic_3, name), `$\\hat\\mu$` = sprintf("$%.3f$", mu_hat),
                       `95\\% region, conservative` = fmt_region(lo, hi))
print(tbl, n = Inf)
tt_obj <- tt(tbl, align = "lcc", width = c(4, 1.2, 2),
             notes = test_inv_note("conservative, $\\chi^2_{2,0.95}=5.99$", "no credit for the corporations' moment")) %>%
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "appF-evasion-test-conservative")
