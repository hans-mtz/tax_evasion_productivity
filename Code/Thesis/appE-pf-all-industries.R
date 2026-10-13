## Production-function estimates for all industries used in stage 2 (appendix E): @tbl-pf-all-industries (sharp regions) and
## @tbl-pf-all-industries-cons (conservative regions; Hans 2026-10-12: one table per test).
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

## Hans 2026-10-13: no open ends in a table of estimates. The (alpha_K, alpha_L) regions come from 1525 (1517's regions with
## the grid extended beyond [0,1]^2 until each region closes); beta's regions are interior. A region still open at 1525's cap
## ([-1, 3]; Hans 2026-10-13: no large negative bounds) prints with an open end and the note says so (313's alpha_L, cons.).
rg <- \(lo, hi, olo = rep(FALSE, length(lo)), ohi = rep(FALSE, length(lo))) ifelse(is.na(lo), "empty",
    paste0("$", ifelse(olo, "(\\,\\cdot\\,", sprintf("[%.3f", lo)), ",\\,", ifelse(ohi, "\\,\\cdot\\,)", sprintf("%.3f]", hi)), "$"))
ext <- read.csv(file.path(PRODUCTS_DIR, "1525-pf-regions-extended.csv"), colClasses = c(sic_3 = "character")) %>%
    filter(system == "lag_2_w_eps", sic_3 %in% nine)
stopifnot(nrow(distinct(ext, sic_3, test)) == 18)   # 9 industries x 2 tests
load(file.path(PRODUCTS_DIR, "1522-beta-testinv.RData"))   # beta_ci: sharp (chi2_1) and conservative (chi2_5) regions for beta
pf <- pf_raw |> left_join(names_df, by = "sic_3") |>
    left_join(beta_ci |> select(sic_3, b_sh_lo, b_sh_hi, b_co_lo, b_co_hi, corp_plants), by = "sic_3") |> arrange(sic_3)
stopifnot(!anyNA(pf$name), nrow(pf) == 9, !any(pf$at_bound))
print(as_tibble(pf), n = Inf)

note_common <- "Output elasticities of materials ($\\hat\\beta$, from the first stage on corporations, log materials share net of sales taxes), capital ($\\hat\\alpha_K$) and labour ($\\hat\\alpha_L$) for the nine industries where overreporting is detected, whose unincorporated firms enter the stage-2 estimation of the detection and evasion-cost parameters as interior firms. Instrument $\\tilde{\\mathcal W}_{it-2}$. Point estimates: the minimum of the test-inversion statistic, with the covariance of the moments re-estimated at each candidate $(\\alpha_K,\\alpha_L)$. "
note_tail <- " $n$: observations in the first stage. Juridical organization codes 6--9 excluded. $^{\\S}$ Two corporate plants: the plant-clustered variance behind the region for $\\beta$ is unreliable."
make_tbl <- function(test, slug) {
    sh <- test == "sharp"; lab <- if (sh) "sharp" else "cons."
    b_lo <- if (sh) pf$b_sh_lo else pf$b_co_lo; b_hi <- if (sh) pf$b_sh_hi else pf$b_co_hi
    stopifnot(b_lo > 0, b_hi < 1)   # interior: closed as printed
    e <- ext %>% filter(test == !!test) %>% slice(match(pf$sic_3, sic_3))
    stopifnot(identical(e$sic_3, pf$sic_3))
    o <- with(e, cbind(Klo = !is.na(K_lo) & K_lo <= Klim_lo, Khi = !is.na(K_hi) & K_hi >= Klim_hi,
                       Llo = !is.na(L_lo) & L_lo <= Llim_lo, Lhi = !is.na(L_hi) & L_hi >= Llim_hi))
    tbl <- tibble(Industry = paste0(pf$sic_3, " ", pf$name, ifelse(pf$corp_plants < 3, "$^{\\S}$", "")),
                  b = sprintf("%.3f", pf$beta), b_r = rg(b_lo, b_hi), k = sprintf("%.3f", pf$alpha_K),
                  k_r = rg(e$K_lo, e$K_hi, o[, "Klo"], o[, "Khi"]),
                  l = sprintf("%.3f", pf$alpha_L), l_r = rg(e$L_lo, e$L_hi, o[, "Llo"], o[, "Lhi"]), n = format(pf$n_obs, big.mark = ","))
    names(tbl) <- c("Industry", "$\\hat\\beta$", paste0("$\\beta$, ", lab), "$\\hat\\alpha_K$", paste0("$\\alpha_K$, ", lab),
                    "$\\hat\\alpha_L$", paste0("$\\alpha_L$, ", lab), "$n$")
    reg <- if (sh) "$\\beta$, sharp: 95\\% test-inversion region for $\\beta$ from the corporations' share moment ($\\chi^2_{1,0.95}$; the production-function moments are exactly identified given $\\beta$ and profiled). $\\alpha$, sharp: projections of the 95\\% test-inversion region ($\\chi^2_{2,0.95}$, credit for profiling $\\gamma_0,\\gamma_1$) on a 0.005 grid over $[0,1]^2$, extended beyond it where the region reaches its edge."
           else "$\\beta$, cons.: 95\\% test-inversion region for $\\beta$, conservative test ($\\chi^2_{5,0.95}$, no credit for profiling). $\\alpha$, cons.: projections of the 95\\% conservative test-inversion region ($\\chi^2_{4,0.95}$) on a 0.005 grid over $[0,1]^2$, extended beyond it where the region reaches its edge."
    tt_obj <- tt(tbl, align = "lccccccc", width = c(4.1, 0.75, 1.95, 0.75, 1.95, 0.75, 1.95, 0.75),
                 notes = paste0(note_common, reg, if (any(o)) " $(\\,\\cdot\\,$ or $\\,\\cdot\\,)$: the region is not bounded on that side within $[-1,3]^2$." else "", if (any(pf$corp_plants < 3)) note_tail else sub(" \\$\\^\\{.*$", "", note_tail))) |>
        style_tt(fontsize = 0.88) |> style_tt(i = "notes", fontsize = 0.8)
    render_thesis_table(tt_obj, slug)
}
make_tbl("sharp", "appE-pf-all-industries")
make_tbl("cons", "appE-pf-all-industries-cons")
cat("Saved: Thesis/tables/appE-pf-all-industries{,-cons}.{png,pdf}\n")
