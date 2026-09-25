## Production-function estimates for all industries used in stage 2 (@tbl-pf-all-industries,
## appendix E). Stage 2 (ELVIS, ch. 8) uses the single-instrument (m*_{it-1}) estimates for
## every industry in pf_list, not only the five reported in ch. 6's headline table.
## Source: Code/Deconvolution/1100-MSL-opttax.R -> Code/Products/1100-MSL-opttax.RData.
## Point estimates only (no standard errors are stored in pf_list).

source("Code/Thesis/001-setup.R")
load(file.path(PRODUCTS_DIR, "1100-MSL-opttax.RData"))  # pf_list (instrument lag_m)

## Short ISIC Rev. 2 industry names (same labels as ch04-evasion-test.R, plus the
## industries outside ch. 4's top 20).
names_df <- tribble(
    ~sic_3, ~name,
    "311", "Food products", "312", "Other food products", "313", "Beverages",
    "314", "Tobacco", "321", "Textiles", "322", "Wearing apparel",
    "323", "Leather products", "324", "Footwear", "331", "Wood products",
    "332", "Furniture", "341", "Paper products", "342", "Printing and publishing",
    "351", "Industrial chemicals", "352", "Other chemicals", "353", "Petroleum refineries",
    "354", "Petroleum and coal products", "355", "Rubber products", "356", "Plastic products",
    "361", "Pottery and china", "362", "Glass products", "369", "Non-metallic minerals",
    "371", "Iron and steel", "372", "Non-ferrous metals", "381", "Metal products",
    "382", "Non-electrical machinery", "383", "Electrical machinery",
    "384", "Transport equipment", "385", "Professional equipment", "390", "Other manufacturing"
)

pf <- lapply(names(pf_list), \(x) tibble(
    sic_3 = x,
    beta  = pf_list[[x]]$coeffs[["m"]],
    alpha_K = pf_list[[x]]$coeffs[["k"]],
    alpha_L = pf_list[[x]]$coeffs[["l"]],
    conv  = pf_list[[x]]$convergence
)) |> bind_rows() |>
    left_join(names_df, by = "sic_3") |>
    arrange(sic_3)

stopifnot(!anyNA(pf$name), all(unique(sapply(pf_list, `[[`, "instrument")) == "lag_m"))
print(pf, n = Inf)

tbl <- pf |> transmute(
    Industry = paste0(sic_3, " ", name,
                      ifelse(conv != 0, "$^{\\dagger}$", ""),
                      ifelse(pmin(alpha_K, alpha_L) <= 0 | pmax(alpha_K, alpha_L) >= 1, "$^{\\ddagger}$", "")),
    `$\\hat\\beta$` = sprintf("%.3f", beta),
    `$\\hat\\alpha_K$` = sprintf("%.3f", alpha_K),
    `$\\hat\\alpha_L$` = sprintf("%.3f", alpha_L)
)

tt_obj <- tt(tbl, align = "lccc", width = c(4, 1, 1, 1),
             notes = "Output elasticities of materials ($\\hat\\beta$, from the first stage on corporations), capital ($\\hat\\alpha_K$) and labour ($\\hat\\alpha_L$), with instrument $m^*_{it-1}$, for every industry used in the stage-2 estimation of the detection and evasion-cost parameters. Point estimates. $^{\\dagger}$ The optimizer stopped with a warning (convergence code 52) for this industry. $^{\\ddagger}$ An estimate is at the bound of the $[0,1]$ parameter space.") |>
    style_tt(i = "notes", fontsize = 0.65)
render_thesis_table(tt_obj, "appE-pf-all-industries")
