## 2026-09-26: models from 1530-fiscal-did.RData, NET-of-sales-tax log materials share, juridical organization codes 6-9 dropped (Hans).
## PRODUCT: Thesis/figures/ch07-fiscal-all-unincorp.png := @fig-fiscal-all. Same model as ch07-fiscal-all-table.R.
source("Code/Thesis/ch07-fiscal-plot-helpers.R")
load(file.path(PRODUCTS_DIR, "1530-fiscal-did.RData"))   # did$net$reg_all: same specification as before
reg <- did$net$reg_all
fiscal_plot(list(`Unincorporated firms` = list(level = list(model = reg[[1]], prefix = "corp::Other:year::"),
                                               diff  = list(model = reg[[2]], prefix = "corp::Other:year::"))),
            "ch07-fiscal-all-unincorp")
