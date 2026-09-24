## PRODUCT: Thesis/tables/ch07-fiscal-prt.png := coefficient table behind
## @fig-fiscal-prt (Proprietorships in ST-exempt vs. ST-liable industries). Models rg_lvl_jo /
## rg_lvl_b83_jo from 921.1-DD.R (juridical-organization split).
source("Code/Thesis/ch07-fiscal-table-helpers.R")
load(file.path(PRODUCTS_DIR, "921.1-DD.RData"))
fiscal_table(
    list(`Level ` = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Proprietorship:Exempt:"),
         `Diff. ` = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Proprietorship:Exempt:"),
         Level = list(model = rg_lvl_jo, prefix = "jo_exempt_year::Proprietorship:Taxed:"),
         Diff. = list(model = rg_lvl_b83_jo, prefix = "jo_exempt_y83::Proprietorship:Taxed:")),
    "ch07-fiscal-prt",
    groups = list("ST-exempt" = 2:3, "ST-liable" = 4:5)
)
