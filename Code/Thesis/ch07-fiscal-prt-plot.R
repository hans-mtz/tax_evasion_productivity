## PRODUCT: Thesis/figures/ch07-fiscal-prt.png := @fig-fiscal-prt (proprietorships). Same models as ch07-fiscal-prt-table.R.
source("Code/Thesis/ch07-fiscal-plot-helpers.R")
load(file.path(PRODUCTS_DIR, "921.1-DD.RData"))
fiscal_plot(list(
    `ST-exempt` = list(level = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Proprietorship:Exempt:"),
                       diff  = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Proprietorship:Exempt:")),
    `ST-liable` = list(level = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Proprietorship:Taxed:"),
                       diff  = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Proprietorship:Taxed:"))),
    "ch07-fiscal-prt", colours = c(`ST-exempt` = "#E69F00", `ST-liable` = "black"))
