## PRODUCT: Thesis/figures/ch07-fiscal-liable-vs-exempt.png := @fig-fiscal-liable. Same models as ch07-fiscal-liable-table.R.
source("Code/Thesis/ch07-fiscal-plot-helpers.R")
load(file.path(PRODUCTS_DIR, "921.1-DD.RData"))
fiscal_plot(list(
    `ST-exempt` = list(level = list(model = rg_lvl_crp, prefix = "corp_exempt_year::Other:Exempt:"),
                       diff  = list(model = rg_lvl_b83_crp, prefix = "corp_exempt_y83::Other:Exempt:")),
    `ST-liable` = list(level = list(model = rg_lvl_crp, prefix = "corp_exempt_year::Other:Taxed:"),
                       diff  = list(model = rg_lvl_b83_crp, prefix = "corp_exempt_y83::Other:Taxed:"))),
    "ch07-fiscal-liable-vs-exempt", colours = c(`ST-exempt` = THESIS_COLS[2], `ST-liable` = THESIS_COLS[1]))
