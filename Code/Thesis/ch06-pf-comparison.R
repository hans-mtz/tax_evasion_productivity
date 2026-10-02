## PRODUCT: Thesis/tables/ch06-pf-comparison.png := Table 6.x, production function estimates by method
## Rebuilt 2026-09-24 (Hans): report BOTH single-instrument fits used downstream -- m*_{it-1} (lag_m,
## feeds the ELVIS/counterfactual estimates) and W~_{it-2} (lag_2_w_eps = lag of w_eps = cal_W - aK k - aL l,
## the TILDED W) -- next to the joint efficient GMM (both instruments; the choice for the final version),
## GNR and OLS. HEADLINE (2026-09-26): W~_{it-2} alone, first estimate column (all-industry comparison, 1517: never
## empty, as precise as the joint system; m*_{it-1} alone is weak). Switched 2026-09-26 to the two-tax (net-of-tax) first stage, juridical organization codes 6-9 excluded
## (PLAN.md §9a): 1511/1512/1513 are the two-tax copies of 1472/1473/1477(+1478). GNR/OLS unchanged. Layout like the ch. 4 test table:
## one row with point estimates, then one row per test-inversion region (sharp, conservative) below it.
## Points (2026-09-26): Code/Products/1516-pf-points-updated-omega.csv -- for every system, the continuous minimum of the
##        same test-inversion statistic whose regions are shown (Omega re-estimated at each candidate).
## Reads: Code/Products/1517-pf-systems-all-industries.RData (points + sharp/conservative regions, all three systems)
##        Code/Products/1512-pf-testinv-grid.csv            (single-instrument J on the (aK,aL) grid;
##                                                           sharp chi2_2 = 5.99, conservative chi2_4 = 9.49)
##        Code/Products/1513-pf-joint-testinv-summary-cons.csv (joint: grid-min point, sharp chi2_3 /
##                                                           conservative chi2_5 regions)
##        Code/Products/1501-fs-net.RData                   (fs_net_ls: beta per industry, fixed from stage 1)
##        Code/Products/1520-gnr-ols.RData                  (GNR (R port of the Stata code, validated to 4 decimals) + OLS,
##                                                           same sample and net-of-tax share; replaced PF_me_tbl 2026-09-26)
source("Code/Thesis/001-setup.R")

five <- c("313", "321", "322", "342", "369")   # 2026-10-01: 5 largest industries (output share) with overreporting detected at 1% (same as ch. 5)
f3  <- \(x) sprintf("%.3f", x)
rg3 <- \(lo, hi) ifelse(is.na(lo), "empty",
    paste0("$", ifelse(lo <= 0, "(\\,\\cdot\\,", sprintf("[%.3f", lo)), ",\\,",
           ifelse(hi >= 1, "\\,\\cdot\\,)", sprintf("%.3f]", hi)), "$"))   # test convention: open end where 0 / 1 is not rejected

## Points and regions for all three systems from ONE source: 1517 (minimum of the test-inversion statistic, Omega at
## each candidate; sharp/conservative projections on a 0.005 grid). Single: chi2_2/chi2_4; joint: chi2_3/chi2_5.
load(file.path(PRODUCTS_DIR, "1517-pf-systems-all-industries.RData"))   # res
sys <- \(x, pre) { z <- res %>% filter(system == x, sic_3 %in% five) %>%
    transmute(sic_3, aK = alpha_K, aL = alpha_L, K_sharp = rg3(K_sh_lo, K_sh_hi), L_sharp = rg3(L_sh_lo, L_sh_hi),
              K_cons = rg3(K_co_lo, K_co_hi), L_cons = rg3(L_co_lo, L_co_hi), stat)
    names(z)[-1] <- paste0(pre, names(z)[-1]); z }
m1 <- sys("lag_m", "m_"); w2 <- sys("lag_2_w_eps", "w_"); joint <- sys("joint", "j_")
load(file.path(PRODUCTS_DIR, "1501-fs-net.RData"))                   # fs_net_ls (beta)
t0 <- tibble(sic_3 = five, beta = sapply(five, \(s) fs_net_ls[[s]]$beta))
load(file.path(PRODUCTS_DIR, "1522-beta-testinv.RData"))            # beta_ci: sharp chi2_1 / conservative chi2_5
t0 <- t0 %>% left_join(beta_ci %>% transmute(sic_3, b_sharp = rg3(b_sh_lo, b_sh_hi), b_cons = rg3(b_co_lo, b_co_hi)), by = "sic_3")
load(file.path(PRODUCTS_DIR, "1520-gnr-ols.RData"))                    # gnr_ols (uncorrected)
gnr_ols <- gnr_ols %>% transmute(sic_3 = as.character(sic_3), `CD-GNR_m` = gnr_beta, `CD-GNR_k` = gnr_aK, `CD-GNR_l` = gnr_aL,
                                 OLS_m = ols_beta, OLS_k = ols_aK, OLS_l = ols_aL)

w <- tibble(sic_3 = five) %>%
    left_join(t0 %>% mutate(sic_3 = as.character(sic_3)), by = "sic_3") %>%
    left_join(m1, by = "sic_3") %>% left_join(w2, by = "sic_3") %>%
    left_join(joint, by = "sic_3") %>% left_join(gnr_ols, by = "sic_3")

## Three rows per industry: estimates, sharp region, conservative region
tbl <- w %>% rowwise() %>% reframe(
    Industry = c(sic_3, "sharp", "cons."),
    beta   = c(f3(beta), b_sharp, b_cons),
    w_K = c(f3(w_aK), w_K_sharp, w_K_cons), w_L = c(f3(w_aL), w_L_sharp, w_L_cons),
    m_K = c(f3(m_aK), m_K_sharp, m_K_cons), m_L = c(f3(m_aL), m_L_sharp, m_L_cons),
    j_K = c(f3(j_aK), j_K_sharp, j_K_cons), j_L = c(f3(j_aL), j_L_sharp, j_L_cons),
    g_m = c(f3(as.numeric(`CD-GNR_m`)), "", ""), g_k = c(f3(as.numeric(`CD-GNR_k`)), "", ""),
    g_l = c(f3(as.numeric(`CD-GNR_l`)), "", ""),
    o_m = c(f3(as.numeric(OLS_m)), "", ""), o_k = c(f3(as.numeric(OLS_k)), "", ""),
    o_l = c(f3(as.numeric(OLS_l)), "", "")
)
print(tbl, n = Inf)

est_rows <- seq(1, nrow(tbl), by = 3)
tt_obj <- tt(tbl, align = "lccccccccccccc",
             width = c(1.0, 2.3, 2.3, 2.3, 2.3, 2.3, 2.3, 2.3, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55),
             notes = "Evasion-corrected columns use the first stage on the log materials share net of sales taxes; $\\hat\\beta$ is the corporations' first-stage estimate, common to the three evasion-corrected columns; below it, its 95\\% test-inversion regions from the corporations' share moment (the production-function moments are exactly identified given $\\beta$ and profiled): sharp ($\\chi^2_1$) and conservative ($\\chi^2_5$). Point estimates: the minimum of the test-inversion statistic, with the covariance of the moments re-estimated at each candidate $(\\alpha_K,\\alpha_L)$, the statistic whose regions are shown. $\\tilde{\\mathcal W}_{it-2}$ (the headline) and $m^*_{it-1}$: one instrument each (exactly identified, statistic zero at the estimate); below them, the projections of the 95\\% test-inversion regions for $(\\alpha_K,\\alpha_L)$ (0.005 grid over $[0,1]^2$): sharp ($\\chi^2_2$, credit for profiling $\\gamma_0,\\gamma_1$) and conservative ($\\chi^2_4$). Joint efficient GMM: both instruments together; sharp ($\\chi^2_3$) and conservative ($\\chi^2_5$) regions. Industry 313's joint sharp region is empty (statistic at the estimate 9.46 $>7.81$). GNR and OLS: uncorrected estimates on the same sample (GNR's share regression on the log materials share net of sales taxes). $(\\,\\cdot\\,$ or $\\,\\cdot\\,)$: the bound of the parameter space (0 or 1) is not rejected. Industries: the five largest, by share of output, where overreporting is detected at the 1\\% level (all nine in Appendix E).") %>%
    group_tt(j = list(" " = 1:2, "$\\tilde{\\mathcal W}_{it-2}$" = 3:4, "$m^*_{it-1}$" = 5:6,
                      "Joint efficient GMM" = 7:8, "GNR" = 9:11, "OLS" = 12:14)) %>%
    style_tt(fontsize = 0.66) %>%
    style_tt(i = "notes", fontsize = 0.8)
colnames(tt_obj) <- c("Industry", "$\\hat\\beta$", "$\\hat\\alpha_K$", "$\\hat\\alpha_L$", "$\\hat\\alpha_K$", "$\\hat\\alpha_L$",
                      "$\\hat\\alpha_K$", "$\\hat\\alpha_L$",
                      "$\\hat\\beta$", "$\\hat\\alpha_K$", "$\\hat\\alpha_L$", "$\\hat\\beta$", "$\\hat\\alpha_K$", "$\\hat\\alpha_L$")

render_thesis_table(tt_obj, "ch06-pf-comparison")
cat("Saved: Thesis/tables/ch06-pf-comparison.{png,pdf}\n")
