## PRODUCT: Thesis/figures/ch07-fiscal-all-unincorp.png := @fig-fiscal-all. Same model as ch07-fiscal-all-table.R.
source("Code/Thesis/ch07-fiscal-plot-helpers.R")
load(file.path(PRODUCTS_DIR, "910-reg-results.RData")) # wip_df
reg <- feols(
    log_mats_share ~ sw(i(corp, year, "Corp"), corp + i(corp, year, "Corp", ref2 = 83)) | sic_3,
    cluster = ~ plant + year, data = wip_df
)
fiscal_plot(list(`Unincorporated firms` = list(level = list(model = reg[[1]], prefix = "corp::Other:year::"),
                                               diff  = list(model = reg[[2]], prefix = "corp::Other:year::"))),
            "ch07-fiscal-all-unincorp")
