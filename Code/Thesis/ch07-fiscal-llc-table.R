## PRODUCT: Thesis/tables/ch07-fiscal-llc.png := coefficient table behind
## @fig-fiscal-llc (LLCs in ST-exempt vs. ST-liable industries). Models rg_lvl_jo /
## rg_lvl_b83_jo from 921.1-DD.R (juridical-organization split).
source("Code/Thesis/ch07-fiscal-table-helpers.R")
load(file.path(PRODUCTS_DIR, "921.1-DD.RData"))
fiscal_table(
    list(`Level ` = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Ltd. Co.:Exempt:"),
         `Diff. ` = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Ltd. Co.:Exempt:"),
         Level = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Ltd. Co.:Taxed:"),
         Diff. = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Ltd. Co.:Taxed:")),
    "ch07-fiscal-llc",
    groups = list("ST-exempt" = 2:3, "ST-liable" = 4:5)
)
