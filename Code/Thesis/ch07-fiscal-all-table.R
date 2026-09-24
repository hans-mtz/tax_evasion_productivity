## PRODUCT: Thesis/tables/ch07-fiscal-all.png := coefficient table behind
## @fig-fiscal-all (all unincorporated firms, levels and differences to 1983).
## The regression is re-estimated here with exactly the specification in
## Code/Deconvolution/921.2-het-slides.R (reg_crp_all_inds, never saved).
source("Code/Thesis/ch07-fiscal-table-helpers.R")
load(file.path(PRODUCTS_DIR, "910-reg-results.RData")) # wip_df

reg <- feols(
    log_mats_share ~ sw(i(corp, year, "Corp"), corp + i(corp, year, "Corp", ref2 = 83)) | sic_3,
    cluster = ~ plant + year, data = wip_df
)
fiscal_table(
    list(Level = list(model = reg[[1]], prefix = "corp::Other:year::"),
         `Diff. to 1983` = list(model = reg[[2]], prefix = "corp::Other:year::")),
    "ch07-fiscal-all"
)
