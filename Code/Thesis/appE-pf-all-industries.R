## Production-function estimates for all industries used in stage 2 (@tbl-pf-all-industries, appendix E).
## HEADLINE (2026-09-26): single instrument W~_{it-2} (lag_2_w_eps: the tilded W = omega + (1-beta) eps, lagged twice),
## on the two-tax (net-of-tax) first stage, juridical organization codes 6-9 excluded (PLAN.md §9a). Point = minimum of
## the test-inversion statistic (Omega re-estimated at each candidate); 95% sharp regions (chi2_2) projected on each axis.
## Source: Code/Deconvolution/1517-pf-systems-all-industries.R -> Code/Products/1517-pf-systems-all-industries.RData.
## 353 has no estimate: 3 observations once codes 6-9 are excluded.
## (Earlier versions: joint two-step GMM, 1502; before that single instrument m*_{it-1}, single-tax, 1100.)

source("Code/Thesis/001-setup.R")
load(file.path(PRODUCTS_DIR, "1517-pf-systems-all-industries.RData"))   # res
nine <- c("313", "321", "322", "324", "331", "342", "351", "352", "369")   # 2026-10-01: the 9 industries that need the correction (interior firms in stage 2)
pf_raw <- res |> filter(system == "lag_2_w_eps") |> mutate(sic_3 = as.character(sic_3)) |> filter(sic_3 %in% nine)

## Short ISIC Rev. 2 industry names (same labels as ch04-evasion-test.R, plus the
## industries outside ch. 4's top 20).
names_df <- tribble(
    ~sic_3, ~name,
    "311", "Food products", "312", "Other food products", "313", "Beverages",
    "314", "Tobacco", "321", "Textiles", "322", "Wearing apparel",
    "323", "Leather products", "324", "Footwear", "331", "Wood products",
    "332", "Furniture", "341", "Paper products", "342", "Printing and publishing",
    "351", "Industrial chemicals", "352", "Other chemicals", "353", "Petroleum refineries",
    "354", "Petroleum and coal", "355", "Rubber products", "356", "Plastic products",
    "361", "Pottery and china", "362", "Glass products", "369", "Non-metallic minerals",
    "371", "Iron and steel", "372", "Non-ferrous metals", "381", "Metal products",
    "382", "Non-electrical machinery", "383", "Electrical machinery",
    "384", "Transport equipment", "385", "Professional equipment", "390", "Other manufacturing"
)

rg <- \(lo, hi) ifelse(is.na(lo), "empty",
    paste0("$", ifelse(lo <= 0, "(\\,\\cdot\\,", sprintf("[%.3f", lo)), ",\\,",
           ifelse(hi >= 1, "\\,\\cdot\\,)", sprintf("%.3f]", hi)), "$"))   # test convention: open end where 0 / 1 is not rejected
load(file.path(PRODUCTS_DIR, "1522-beta-testinv.RData"))   # beta_ci: sharp region for beta (corporations' share moment)
pf <- pf_raw |> left_join(names_df, by = "sic_3") |> left_join(beta_ci |> select(sic_3, b_sh_lo, b_sh_hi, corp_plants), by = "sic_3") |> arrange(sic_3)
stopifnot(!anyNA(pf$name), nrow(pf) == 9, !any(pf$at_bound))
print(as_tibble(pf), n = Inf)

tbl <- pf |> transmute(
    Industry = paste0(sic_3, " ", name, ifelse(corp_plants < 3, "$^{\\S}$", "")),
    `$\\hat\\beta$` = sprintf("%.3f", beta),
    `$\\beta$, sharp` = rg(b_sh_lo, b_sh_hi),
    `$\\hat\\alpha_K$` = sprintf("%.3f", alpha_K),
    `$\\alpha_K$, sharp` = rg(K_sh_lo, K_sh_hi),
    `$\\hat\\alpha_L$` = sprintf("%.3f", alpha_L),
    `$\\alpha_L$, sharp` = rg(L_sh_lo, L_sh_hi),
    `$n$` = format(n_obs, big.mark = ",")
)

tt_obj <- tt(tbl, align = "lccccccc", width = c(4.1, 0.75, 1.95, 0.75, 1.95, 0.75, 1.95, 0.75),
             notes = "Output elasticities of materials ($\\hat\\beta$, from the first stage on corporations, log materials share net of sales taxes), capital ($\\hat\\alpha_K$) and labour ($\\hat\\alpha_L$) for the nine industries where overreporting is detected, whose unincorporated firms enter the stage-2 estimation of the detection and evasion-cost parameters as interior firms. Instrument $\\tilde{\\mathcal W}_{it-2}$. Point estimates: the minimum of the test-inversion statistic, with the covariance of the moments re-estimated at each candidate $(\\alpha_K,\\alpha_L)$; $\\beta$, sharp: 95\\% test-inversion region for $\\beta$ from the corporations' share moment ($\\chi^2_{1,0.95}$; the production-function moments are exactly identified given $\\beta$ and profiled). $\\alpha$, sharp: projections of the 95\\% test-inversion region ($\\chi^2_{2,0.95}$, credit for profiling $\\gamma_0,\\gamma_1$) on a 0.005 grid over $[0,1]^2$. $(\\,\\cdot\\,$ or $\\,\\cdot\\,)$: the bound of the parameter space (0 or 1) is not rejected. $n$: observations in the first stage. Juridical organization codes 6--9 excluded. $^{\\S}$ Two corporate plants: the plant-clustered variance behind the region for $\\beta$ is unreliable.") |>
    style_tt(fontsize = 0.88) |>
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "appE-pf-all-industries")
