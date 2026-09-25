## PRODUCT: Thesis/tables/ch06-productivity-comparison.png := Table 6.x, productivity: evasion-corrected vs GNR
## Compares omega with omega (never omega + eps):
##   - Corrected: deconvolved density of omega from W~ = omega + (1-beta) eps (293-omega-deconv-current-pf.R,
##     omega_deconv_current_pf.RData), one per instrument (m*_{it-1} = lag_m, W~_{it-2} = lag_2_w_eps).
##     Moments/quantiles are integrals against the fitted density on a fine grid.
##   - GNR (uncorrected): firm-level log omega from Code/Stata/020-loop-me.do + GNR_code_CD_me.do,
##     exported as Code/Products/stata-gnr-me-omg-<sic>.csv. There, vg = yg - eg - integ_G_I removes the
##     measurement error eg (with E[exp eg] set to 1, mexp_eg = 1) and logomega = vg - aL*l - aK*k,
##     i.e. omega alone. Same sample (e.g. 1,117 obs in 313 on both sides), real variables.
## Metrics on productivity in levels, exp(omega): mean, SD, median, percentile ratios 75/25, 80/20, 90/10, 95/5
## (GNR 2020, Table 3, use 75/25, 90/10, 95/5); skewness of omega; persistence gamma_1 (AR(1) coefficient,
## from 294-omega-persistence.R: corrected = IV at the PF point estimates, GNR = OLS on firm-level omega).
source("Code/Thesis/001-setup.R")
fenv <- new.env(); load(file.path(PRODUCTS_DIR, "np-deconv-funs.RData"), envir = fenv)   # never source() 030
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv
f_e.np <- fenv$f_e.np
load(file.path(PRODUCTS_DIR, "omega_deconv_current_pf.RData"))   # omega_cur_np_ls

five <- c("331", "322", "369", "313", "321")
ind_names <- c("331" = "Wood products", "322" = "Wearing apparel", "369" = "Non-metallic minerals",
               "313" = "Beverages", "321" = "Textiles")

## weighted-sample metrics: x = omega values, w = probability weights (sum to 1)
metrics <- function(x, w) {
    o <- order(x); x <- x[o]; w <- w[o] / sum(w); Fw <- cumsum(w)
    q <- \(p) x[which.max(Fw >= p)]
    phi <- exp(x); m1 <- sum(w * phi)
    mu <- sum(w * x); s <- sqrt(sum(w * (x - mu)^2))
    tibble(Mean = m1, SD = sqrt(sum(w * (phi - m1)^2)), Median = exp(q(.5)),
           r7525 = exp(q(.75) - q(.25)), r8020 = exp(q(.8) - q(.2)),
           r9010 = exp(q(.9) - q(.1)), r955 = exp(q(.95) - q(.05)),
           Skew = sum(w * (x - mu)^3) / s^3)
}

corrected <- imap_dfr(omega_cur_np_ls, function(d, nm) {
    p <- d$params
    x <- seq(p$a, p$b, length.out = 4001)
    w <- vapply(x, function(z) f_e.np(z, d$theta, p), numeric(1))
    key <- strsplit(nm, " ")[[1]]
    metrics(x, w) %>% mutate(sic_3 = key[1], Method = recode(key[2], lag_m = "m", lag_2_w_eps = "w"))
})
gnr <- map_dfr(five, function(s) {
    g <- read.csv(file.path(PRODUCTS_DIR, paste0("stata-gnr-me-omg-", s, ".csv")))
    x <- g$logomega[is.finite(g$logomega)]
    metrics(x, rep(1, length(x))) %>% mutate(sic_3 = s, Method = "gnr")
})

pers <- read.csv(file.path(PRODUCTS_DIR, "294-omega-persistence.csv")) %>%
    mutate(sic_3 = as.character(sic_3), Method = recode(method, lag_m = "m", lag_2_w_eps = "w", gnr = "gnr")) %>%
    select(sic_3, Method, gamma1)
f2 <- \(v) sprintf("%.2f", v); f1 <- \(v) formatC(v, format = "f", digits = 1, big.mark = ",")
tbl <- bind_rows(corrected, gnr) %>% left_join(pers, by = c("sic_3", "Method")) %>%
    mutate(sic_3 = factor(sic_3, five),
           Method = factor(Method, c("m", "w", "gnr"),
                           c("Corrected, $m^*_{it-1}$", "Corrected, $\\tilde{\\mathcal W}_{it-2}$", "GNR"))) %>%
    arrange(sic_3, Method) %>%
    group_by(sic_3) %>%
    mutate(Industry = ifelse(row_number() == 1, as.character(sic_3), "")) %>%
    ungroup() %>%
    transmute(Industry, Method = as.character(Method), Mean = f1(Mean), SD = f1(SD), Median = f1(Median),
              `75/25` = f2(r7525), `80/20` = f2(r8020), `90/10` = f2(r9010), `95/5` = f2(r955),
              Skewness = f2(Skew), `$\\hat\\gamma_1$` = f2(gamma1))
print(tbl, n = Inf)

tt_obj <- tt(tbl, align = "llccccccccc", width = c(0.8, 2.4, 0.9, 0.9, 0.9, 0.8, 0.8, 0.8, 0.8, 1, 0.8),
             notes = "Productivity in levels, $\\exp(\\omega_{it})$; percentile ratios as in Gandhi, Navarro and Rivers (2020, Table 3); skewness of $\\omega_{it}$; $\\hat\\gamma_1$: persistence, the AR(1) coefficient of $\\omega_{it}$ (corrected: IV estimate within the production function step; GNR: OLS on firm-level $\\omega_{it}$). Corrected: moments of the deconvolved density of $\\omega$ (penalized B-spline deconvolution of $\\widetilde{\\mathcal W}_{it}=\\omega_{it}+(1-\\beta)\\varepsilon_{it}$), with the production function estimates of the corresponding single instrument. GNR: firm-level $\\omega_{it}$ from the uncorrected GNR (2020) Cobb-Douglas estimation, with the measurement error removed. Both exclude $\\varepsilon$. Industries: 331 wood products, 322 wearing apparel, 369 non-metallic minerals, 313 beverages, 321 textiles. In 313, the $\\tilde{\\mathcal W}_{it-2}$ estimate of $\\alpha_K$ is 0 (at the bound), so its $\\omega$ absorbs capital and its level is not comparable.") %>%
    style_tt(fontsize = 0.85) %>%
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "ch06-productivity-comparison")
