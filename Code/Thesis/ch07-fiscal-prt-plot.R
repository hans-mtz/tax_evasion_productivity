## 2026-09-26: models from 1530-fiscal-did.RData, NET-of-sales-tax log materials share, juridical organization codes 6-9 dropped (Hans).
## PRODUCT: Thesis/figures/ch07-fiscal-prt.png := @fig-fiscal-prt (proprietorships). Same models as ch07-fiscal-prt-table.R.
source("Code/Thesis/ch07-fiscal-plot-helpers.R")
load(file.path(PRODUCTS_DIR, "1530-fiscal-did.RData"))   # did$net: net-of-tax share, codes 6-9 dropped
list2env(did$net[c("rg_lvl_crp", "rg_lvl_jo", "rg_lvl_b83_crp", "rg_lvl_b83_jo")], environment())
fiscal_plot(list(
    `ST-exempt` = list(level = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Proprietorship:Exempt:"),
                       diff  = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Proprietorship:Exempt:")),
    `ST-liable` = list(level = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Proprietorship:Taxed:"),
                       diff  = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Proprietorship:Taxed:"))),
    "ch07-fiscal-prt", colours = c(`ST-exempt` = THESIS_COLS[2], `ST-liable` = THESIS_COLS[1]))
