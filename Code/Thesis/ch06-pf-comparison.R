## PRODUCT: Thesis/tables/ch06-pf-comparison.png := Table 6.x, joint efficient-GMM (m*_{it-1}+W~_{it-2}) vs GNR vs OLS
## Reads: Code/Products/1478-pf-joint-testinv-summary-cons.csv (test-inversion regions, both tests),
##        Code/Products/1476-pf-joint-gmm-bootstrap.RData (t0: point beta per industry, fixed from stage 1),
##        Code/Products/deconv_prod_fun_trim.RData (PF_me_tbl: GNR + OLS columns, taken as-is, unchanged)
## Revised 2026-09-22: report BOTH test-inversion regions for every industry (sharp chi2_3, credit for
## profiling, AND conservative chi2_5, no credit), not just sharp-with-asterisk-fallback -- same "report
## both tests, don't silently pick one" convention the counterfactual chapter uses (hard/soft). Point
## estimate is always the sharp-region grid minimum; industry 313's sharp region is empty (Research-log
## 2026-09-21, "313 fails the sharp joint test... deferred"), shown as "empty" rather than omitted.
## Companion figure (ch06-pf-testinv-regions.R) shows the full 2D region under both tests for all 5 industries.
source("Code/Thesis/001-setup.R")

smry <- read.csv(file.path(PRODUCTS_DIR, "1478-pf-joint-testinv-summary-cons.csv")) %>% mutate(sic_3 = as.character(sic_3))
load(file.path(PRODUCTS_DIR, "1476-pf-joint-gmm-bootstrap.RData"))   # t0: sic_3, alpha_K, alpha_L, beta, n (point estimates)
load(file.path(PRODUCTS_DIR, "deconv_prod_fun_trim.RData"))           # PF_me_tbl

five <- c("331", "322", "369", "313", "321")
f3 <- \(x) sprintf("%.3f", x)
rng <- \(lo, hi) sprintf("[%.2f, %.2f]", lo, hi)

## GNR / OLS, as-is, wide by industry
gnr_ols <- as.data.frame(PF_me_tbl) %>% mutate(sic_3 = as.character(sic_3)) %>%
    select(sic_3, input, `CD-GNR`, OLS) %>%
    pivot_wider(names_from = input, values_from = c(`CD-GNR`, OLS))

tbl <- smry %>% left_join(t0 %>% select(sic_3, beta), by = "sic_3") %>%
    mutate(sic_3 = factor(sic_3, five)) %>% arrange(sic_3) %>%
    left_join(gnr_ols, by = "sic_3") %>%
    transmute(
        Industry = as.character(sic_3),
        m = f3(beta),
        k = paste0(f3(aK_min), "\n", K_sharp, " / ", K_cons),
        l = paste0(f3(aL_min), "\n", L_sharp, " / ", L_cons),
        gnr_m = f3(as.numeric(`CD-GNR_m`)), gnr_k = f3(as.numeric(`CD-GNR_k`)), gnr_l = f3(as.numeric(`CD-GNR_l`)),
        ols_m = f3(as.numeric(OLS_m)), ols_k = f3(as.numeric(OLS_k)), ols_l = f3(as.numeric(OLS_l))
    )
print(tbl)

## width=1: project default (full book text width, decided in chat
## 2026-09-22) -- added here for consistency; no caption= (never had one).
## k/l cells carry the point estimate plus both test-inversion brackets on one line each
## ("[lo,hi] / [lo,hi]") -- these need much more width than the plain single-value columns,
## or the bracket text wraps mid-expression (caught 2026-09-22: default equal-column-split
## put "/" alone on its own line). Per-column width vector, proportional (tinytable ?tt).
tt_obj <- tt(tbl, align = "lccccccccc", width = c(1, 0.9, 2.3, 2.3, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75),
             notes = "Joint efficient GMM: instruments $m^*_{it-1}$ and $\\tilde{\\mathcal W}_{it-2}$ used together, $\\beta$ fixed at its stage-1 value. $\\hat\\alpha_K,\\hat\\alpha_L$: point estimate (grid minimum, sharp region), followed by both 95\\% test-inversion regions as Sharp / Conservative -- Sharp ($\\chi^2_3$, credit given for profiling $\\gamma_0,\\gamma_1$) and Conservative ($\\chi^2_5$, no credit for profiling). Industry 313's sharp region is empty (min $J=9.74>7.81$; shown as `empty'); its conservative region is still reported -- see Figure 6.x for the full 2D region under both tests. GNR and OLS: uncorrected estimates, unchanged.") %>%
    group_tt(j = list("Joint efficient GMM ($m^*_{it-1}+\\tilde{\\mathcal W}_{it-2}$)" = 2:4, "GNR" = 5:7, "OLS" = 8:10)) %>%
    style_tt(i = "notes", fontsize = 0.65)
colnames(tt_obj) <- c("Industry", "$\\hat\\beta$ (m)", "$\\hat\\alpha_K$", "$\\hat\\alpha_L$", "m", "k", "l", "m", "k", "l")

render_thesis_table(tt_obj, "ch06-pf-comparison")
cat("Saved: Thesis/tables/ch06-pf-comparison.{png,pdf}\n")
