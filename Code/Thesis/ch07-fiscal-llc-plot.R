## PRODUCT: Thesis/figures/ch07-fiscal-llc.png := @fig-fiscal-llc (LLCs). Same models as ch07-fiscal-llc-table.R.
source("Code/Thesis/ch07-fiscal-plot-helpers.R")
load(file.path(PRODUCTS_DIR, "921.1-DD.RData"))
fiscal_plot(list(
    `ST-exempt` = list(level = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Ltd. Co.:Exempt:"),
                       diff  = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Ltd. Co.:Exempt:")),
    `ST-liable` = list(level = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Ltd. Co.:Taxed:"),
                       diff  = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Ltd. Co.:Taxed:"))),
    "ch07-fiscal-llc", colours = c(`ST-exempt` = THESIS_COLS[2], `ST-liable` = THESIS_COLS[1]))
