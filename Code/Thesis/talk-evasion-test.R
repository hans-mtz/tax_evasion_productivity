## PRODUCT: Quarto-Slides/images/talk-evasion-test.png := slide version of Thesis/tables/ch04-evasion-test.png
##          (JMP talk). Same numbers, laid out in two side-by-side blocks of 10 industries for legibility;
##          industries where the test rejects (region excludes 0) in bold; short note.
##          Quarto-Slides/images/talk-evasion-test-food.png := the same table, exempt food industries (311, 312)
##          also highlighted (second results slide).
##          Quarto-Slides/images/talk-evasion-test-conservative.png := same layout, conservative region (chi2_2),
##          slide version of Thesis/tables/appF-evasion-test-conservative.png (appendix slide).
## Reads:   Code/Products/1510-test-inversion.RData -> out (as ch04-evasion-test.R).
source("Code/Thesis/001-setup.R"); source("Code/Thesis/ch04-test-helpers.R")
load(file.path(PRODUCTS_DIR, "1510-test-inversion.RData"))   # out

d <- out %>% filter(share == "s_net") %>% left_join(test_names, by = "sic_3") %>% arrange(sic_3)
stopifnot(nrow(d) == 20, !anyNA(d$name))
d <- d %>% mutate(Industry = paste(sic_3, name), mu = sprintf("$%.3f$", mu_hat),
                  region = fmt_region(sh_lo, sh_hi), reject = !is.na(sh_lo) & sh_lo > 0)
left <- d[1:10, ]; right <- d[11:20, ]
tbl <- data.frame(left$Industry, left$mu, left$region, right$Industry, right$mu, right$region)
names(tbl) <- c("Industry", "$\\hat\\mu$", "95\\% region", "Industry ", "$\\hat\\mu$ ", "95\\% region ")
print(tbl)
make_tt <- function(food = FALSE) {
    tt_obj <- tt(tbl, align = "lcclcc", width = c(3.3, 0.9, 1.75, 4.1, 0.9, 1.75),
                 notes = "$\\hat\\mu$: mean of $\\mathcal V_{it}$ among unincorporated firms. 95\\% test-inversion region (sharp test); bold: the region excludes 0 (overreporting detected). $(\\,\\cdot\\,,\\ b]$: 0 not rejected. $\\dagger$: empty region.") %>%
        style_tt(i = "notes", fontsize = 0.8)
    for (r in which(left$reject))  tt_obj <- style_tt(tt_obj, i = r, j = 1:3, bold = TRUE)
    for (r in which(right$reject)) tt_obj <- style_tt(tt_obj, i = r, j = 4:6, bold = TRUE)
    if (food) tt_obj <- style_tt(tt_obj, i = which(left$sic_3 %in% c(311, 312)), j = 1:3, background = "#FBD9C9")
    tt_obj
}
render_png_tt_tbl(make_tt(), "talk-evasion-test", out_dir = "Quarto-Slides/images", linewidth_pt = THESIS_TEXTWIDTH_PT)
render_png_tt_tbl(make_tt(food = TRUE), "talk-evasion-test-food", out_dir = "Quarto-Slides/images", linewidth_pt = THESIS_TEXTWIDTH_PT)

## Conservative region (appendix slide)
dc <- d %>% mutate(region = fmt_region(lo, hi), reject = !is.na(lo) & lo > 0)
lc <- dc[1:10, ]; rc <- dc[11:20, ]
tbl_c <- data.frame(lc$Industry, lc$mu, lc$region, rc$Industry, rc$mu, rc$region)
names(tbl_c) <- names(tbl)
tt_c <- tt(tbl_c, align = "lcclcc", width = c(3.3, 0.9, 1.75, 4.1, 0.9, 1.75),
           notes = "$\\hat\\mu$: mean of $\\mathcal V_{it}$ among unincorporated firms. 95\\% test-inversion region, conservative test ($\\chi^2_{2,0.95}=5.99$); bold: the region excludes 0. $(\\,\\cdot\\,,\\ b]$: 0 not rejected. $\\dagger$: empty region.") %>%
    style_tt(i = "notes", fontsize = 0.8)
for (r in which(lc$reject)) tt_c <- style_tt(tt_c, i = r, j = 1:3, bold = TRUE)
for (r in which(rc$reject)) tt_c <- style_tt(tt_c, i = r, j = 4:6, bold = TRUE)
render_png_tt_tbl(tt_c, "talk-evasion-test-conservative", out_dir = "Quarto-Slides/images", linewidth_pt = THESIS_TEXTWIDTH_PT)
