## 2026-09-26: models from 1530-fiscal-did.RData, NET-of-sales-tax log materials share, juridical organization codes 6-9 dropped (Hans).
## PRODUCT: Thesis/tables/ch07-fiscal-all.png := coefficient table behind
## @fig-fiscal-all (all unincorporated firms, levels and differences to 1983).
## The regression is re-estimated here with exactly the specification in
## Code/Deconvolution/921.2-het-slides.R (reg_crp_all_inds, never saved).
source("Code/Thesis/ch07-fiscal-table-helpers.R")
load(file.path(PRODUCTS_DIR, "1530-fiscal-did.RData"))   # did$net$reg_all: same specification as before
reg <- did$net$reg_all
fiscal_table(
    list(Level = list(model = reg[[1]], prefix = "corp::Other:year::"),
         `Diff. to 1983` = list(model = reg[[2]], prefix = "corp::Other:year::")),
    "ch07-fiscal-all"
)
