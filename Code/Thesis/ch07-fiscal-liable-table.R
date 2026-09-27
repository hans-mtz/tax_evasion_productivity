## 2026-09-26: models from 1530-fiscal-did.RData, NET-of-sales-tax log materials share, juridical organization codes 6-9 dropped (Hans).
## PRODUCT: Thesis/tables/ch07-fiscal-liable.png := coefficient table behind
## @fig-fiscal-liable (unincorporated firms in ST-exempt vs. ST-liable
## industries). Models rg_lvl_crp / rg_lvl_b83_crp from 921.1-DD.R.
source("Code/Thesis/ch07-fiscal-table-helpers.R")
load(file.path(PRODUCTS_DIR, "1530-fiscal-did.RData"))   # did$net: net-of-tax share, codes 6-9 dropped
list2env(did$net[c("rg_lvl_crp", "rg_lvl_jo", "rg_lvl_b83_crp", "rg_lvl_b83_jo")], environment())
fiscal_table(
    list(`Level ` = list(model = rg_lvl_crp, prefix = "corp_exempt_year::Other:Exempt:"),
         `Diff. ` = list(model = rg_lvl_b83_crp, prefix = "corp_exempt_y83::Other:Exempt:"),
         Level = list(model = rg_lvl_crp, prefix = "corp_exempt_year::Other:Taxed:"),
         Diff. = list(model = rg_lvl_b83_crp, prefix = "corp_exempt_y83::Other:Taxed:")),
    "ch07-fiscal-liable",
    groups = list("ST-exempt" = 2:3, "ST-liable" = 4:5)
)
