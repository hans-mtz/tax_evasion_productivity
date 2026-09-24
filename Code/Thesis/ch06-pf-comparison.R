## PRODUCT: Thesis/tables/ch06-pf-comparison.png := Table 6.x, production function estimates by method
## Rebuilt 2026-09-24 (Hans): report BOTH single-instrument fits used downstream -- m*_{it-1} (lag_m,
## feeds the ELVIS/counterfactual estimates) and W~_{it-2} (lag_2_w_eps = lag of w_eps = cal_W - aK k - aL l,
## the TILDED W) -- next to the joint efficient GMM (both instruments; the choice for the final version),
## GNR and OLS. All from the single-tax (nominal log share) first stage. Layout like the ch. 4 test table:
## one row with point estimates, then one row per test-inversion region (sharp, conservative) below it.
## Reads: Code/Products/1472-pf-instrument-comparison.csv   (single-instrument 2SLS/GMM point estimates)
##        Code/Products/1473-pf-testinv-grid.csv            (single-instrument J on the (aK,aL) grid;
##                                                           sharp chi2_2 = 5.99, conservative chi2_4 = 9.49)
##        Code/Products/1478-pf-joint-testinv-summary-cons.csv (joint: grid-min point, sharp chi2_3 /
##                                                           conservative chi2_5 regions)
##        Code/Products/1476-pf-joint-gmm-bootstrap.RData   (t0: beta per industry, fixed from stage 1)
##        Code/Products/deconv_prod_fun_trim.RData          (PF_me_tbl: GNR + OLS, taken as-is)
source("Code/Thesis/001-setup.R")

five <- c("331", "322", "369", "313", "321")
f3  <- \(x) sprintf("%.3f", x)
## compact interval: leading zero dropped ([.00,.20]) so regions fit one line
cmp <- \(x) sub("^0\\.", ".", sprintf("%.2f", x))
rng <- \(v) if (length(v) == 0 || all(is.na(v))) "empty" else sprintf("[%s,%s]", cmp(min(v)), cmp(max(v)))
recmp <- \(z) ifelse(z == "empty", z, gsub("0\\.", ".", gsub(" ", "", z)))   # same format for the joint CSV strings

## Single instruments: point estimates (2SLS/GMM, as used downstream) ...
single_pt <- read.csv(file.path(PRODUCTS_DIR, "1472-pf-instrument-comparison.csv")) %>%
    mutate(sic_3 = as.character(sic_3)) %>%
    filter(sic_3 %in% five, ins %in% c("lag_m", "lag_2_w_eps")) %>%
    select(sic_3, ins, aK = alpha_K, aL = alpha_L)
## ... and their test-inversion regions, projected on each axis
grid <- read.csv(file.path(PRODUCTS_DIR, "1473-pf-testinv-grid.csv")) %>%
    mutate(sic_3 = as.character(sic_3)) %>%
    filter(sic_3 %in% five, ins %in% c("lag_m", "lag_2_w_eps"))
crit_s <- qchisq(.95, 2); crit_c <- qchisq(.95, 4)
single_rg <- grid %>% group_by(sic_3, ins) %>% summarise(
    K_sharp = rng(aK[J <= crit_s]), L_sharp = rng(aL[J <= crit_s]),
    K_cons  = rng(aK[J <= crit_c]), L_cons  = rng(aL[J <= crit_c]), .groups = "drop")
single <- single_pt %>% left_join(single_rg, by = c("sic_3", "ins"))
S <- \(ins_) single %>% filter(ins == ins_) %>% select(-ins)
m1 <- S("lag_m");       names(m1)[-1] <- paste0("m_", names(m1)[-1])
w2 <- S("lag_2_w_eps"); names(w2)[-1] <- paste0("w_", names(w2)[-1])

## Joint efficient GMM
joint <- read.csv(file.path(PRODUCTS_DIR, "1478-pf-joint-testinv-summary-cons.csv")) %>%
    mutate(sic_3 = as.character(sic_3)) %>%
    transmute(sic_3, j_aK = aK_min, j_aL = aL_min, j_K_sharp = recmp(K_sharp), j_L_sharp = recmp(L_sharp),
              j_K_cons = recmp(K_cons), j_L_cons = recmp(L_cons))
load(file.path(PRODUCTS_DIR, "1476-pf-joint-gmm-bootstrap.RData"))   # t0 (beta)
load(file.path(PRODUCTS_DIR, "deconv_prod_fun_trim.RData"))           # PF_me_tbl
gnr_ols <- as.data.frame(PF_me_tbl) %>% mutate(sic_3 = as.character(sic_3)) %>%
    select(sic_3, input, `CD-GNR`, OLS) %>%
    pivot_wider(names_from = input, values_from = c(`CD-GNR`, OLS))

w <- tibble(sic_3 = five) %>%
    left_join(t0 %>% transmute(sic_3 = as.character(sic_3), beta), by = "sic_3") %>%
    left_join(m1, by = "sic_3") %>% left_join(w2, by = "sic_3") %>%
    left_join(joint, by = "sic_3") %>% left_join(gnr_ols, by = "sic_3")

## Three rows per industry: estimates, sharp region, conservative region
tbl <- w %>% rowwise() %>% reframe(
    Industry = c(sic_3, "sharp", "cons."),
    beta   = c(f3(beta), "", ""),
    m_K = c(f3(m_aK), m_K_sharp, m_K_cons), m_L = c(f3(m_aL), m_L_sharp, m_L_cons),
    w_K = c(f3(w_aK), w_K_sharp, w_K_cons), w_L = c(f3(w_aL), w_L_sharp, w_L_cons),
    j_K = c(f3(j_aK), j_K_sharp, j_K_cons), j_L = c(f3(j_aL), j_L_sharp, j_L_cons),
    g_m = c(f3(as.numeric(`CD-GNR_m`)), "", ""), g_k = c(f3(as.numeric(`CD-GNR_k`)), "", ""),
    g_l = c(f3(as.numeric(`CD-GNR_l`)), "", ""),
    o_m = c(f3(as.numeric(OLS_m)), "", ""), o_k = c(f3(as.numeric(OLS_k)), "", ""),
    o_l = c(f3(as.numeric(OLS_l)), "", "")
)
print(tbl, n = Inf)

est_rows <- seq(1, nrow(tbl), by = 3)
tt_obj <- tt(tbl, align = "lccccccccccccc",
             width = c(1.1, 0.8, 1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75),
             notes = "All columns use the single-tax (nominal log share) first stage; $\\hat\\beta$ is the corporations' first-stage estimate, common to the three evasion-corrected columns. $m^*_{it-1}$ and $\\tilde{\\mathcal W}_{it-2}$: one instrument each, point estimates by GMM; below them, the projections of the 95\\% test-inversion regions for $(\\alpha_K,\\alpha_L)$: sharp ($\\chi^2_2$, credit for profiling $\\gamma_0,\\gamma_1$) and conservative ($\\chi^2_4$). Joint efficient GMM: both instruments together; point estimate at the grid minimum; sharp ($\\chi^2_3$) and conservative ($\\chi^2_5$) regions. Industry 313's joint sharp region is empty (min $J=9.74>7.81$). In 313, the $\\tilde{\\mathcal W}_{it-2}$ estimate of $\\alpha_K$ is at the bound (0) and outside its own sharp region. GNR and OLS: uncorrected estimates.") %>%
    group_tt(j = list(" " = 1:2, "$m^*_{it-1}$" = 3:4, "$\\tilde{\\mathcal W}_{it-2}$" = 5:6,
                      "Joint efficient GMM" = 7:8, "GNR" = 9:11, "OLS" = 12:14)) %>%
    style_tt(fontsize = 0.8) %>%
    style_tt(i = "notes", fontsize = 0.65)
colnames(tt_obj) <- c("Industry", "$\\hat\\beta$", "$\\hat\\alpha_K$", "$\\hat\\alpha_L$", "$\\hat\\alpha_K$", "$\\hat\\alpha_L$",
                      "$\\hat\\alpha_K$", "$\\hat\\alpha_L$", "m", "k", "l", "m", "k", "l")

render_thesis_table(tt_obj, "ch06-pf-comparison")
cat("Saved: Thesis/tables/ch06-pf-comparison.{png,pdf}\n")
