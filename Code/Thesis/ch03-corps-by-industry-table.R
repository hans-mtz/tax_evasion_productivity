## Corporations-by-industry table for the Setting and Data chapter
## (@tbl-corps-by-inds). Adapted from Paper/sections/90-colombia-data.qmd's
## `tbl-corps-by-inds` chunk (top 20 industries in Colombia by number of
## firms). Same standardization as ch03-top-industries-table.R: kbl() ->
## tt(), inline chunk -> standalone script writing Thesis/tables/.

source("Code/Thesis/001-setup.R")
load(file.path(PRODUCTS_DIR, "global_vars.RData")) # provides top_20_inds_table

## No caption= (see ch03-summary-stats-table.R's note). Column name
## "Corps. (\\%)" escapes the literal % -- unescaped, it comments out the
## rest of that cell's LaTeX source line and corrupts the whole table (same
## trap documented there, caught a 3rd time here). width as a per-column
## vector (see ch03-top-industries-table.R's note): Industry is long free
## text, weighted ~4x a numeric column so it doesn't wrap to 5+ lines under
## an equal 1/6-each split.
tbl_obj <- top_20_inds_table[1:20, ] %>%
    ungroup() %>%
    mutate(across(where(is.numeric) & !any_of("sic_3"), ~ round(.x, 1))) %>%
    setNames(c(
        "Industry", "SIC", "N", "Corps. (N)", "Corps. (\\%)", "Market Share (Corps.)"
    )) %>%
    tt(width = c(4, 1, 1, 1, 1, 1.4))

render_thesis_table(tbl_obj, "ch03-corps-by-industry")
cat("Saved: Thesis/tables/ch03-corps-by-industry.{png,pdf}\n")
