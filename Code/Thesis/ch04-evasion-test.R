## PRODUCT: Thesis/tables/ch04-evasion-test.png := Table 4.x, test for overreporting by industry (headline, sharp test)
## Reads:   Code/Products/1510-test-inversion.RData -> out (Code/Deconvolution/1510-test-inversion.R): two-tax (net-of-tax)
##          log materials share, ln D from corporations held fixed, grid mu in [0,1], sharp region (chi2_1).
##          Conservative region and the (fixed) bootstrap test: appendix (appF-evasion-test-conservative.R, -bootstrap.R).
## Replaces (2026-09-26) the bootstrap table from 206-boot-test.R, whose resampling bug overstated significance (1514).
source("Code/Thesis/001-setup.R"); source("Code/Thesis/ch04-test-helpers.R")
load(file.path(PRODUCTS_DIR, "1510-test-inversion.RData"))   # out

d <- out %>% filter(share == "s_net") %>% left_join(test_names, by = "sic_3") %>% arrange(sic_3)
stopifnot(nrow(d) == 20, !anyNA(d$name))
tbl <- d %>% transmute(Industry = paste(sic_3, name), `$\\hat\\mu$` = sprintf("$%.3f$", mu_hat),
                       `95\\% region, sharp` = fmt_region(sh_lo, sh_hi))
print(tbl, n = Inf)
tt_obj <- tt(tbl, align = "lcc", width = c(4, 1.2, 2),
             notes = test_inv_note("sharp, $\\chi^2_{1,0.95}=3.84$", "the corporations' moment holds exactly at $\\ln\\hat D$")) %>%
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "ch04-evasion-test")
