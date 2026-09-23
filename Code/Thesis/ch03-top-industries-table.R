## Top-10-industries-by-revenue table for the Setting and Data chapter
## (@tbl-top-inds-rev). Adapted from Paper/sections/90-colombia-data.qmd's
## `tbl-top-inds-rev` chunk. That chunk rendered inline via kableExtra::kbl();
## here it's standardized to the project's tinytable -> pdflatex -> PNG
## pipeline (Code/Deconvolution/050-render-tbls.R), matching every other
## thesis table. Source object (top_10_revenue) and its columns are
## unchanged.

source("Code/Thesis/001-setup.R")
load(file.path(PRODUCTS_DIR, "global_vars.RData")) # provides top_10_revenue

## No caption= (see ch03-summary-stats-table.R's note): Quarto's own
## ![...]{#tbl-top-inds-rev} caption is the single source now. width=1:
## project default, full book text width. Raw column names (sic_3, n_sic,
## n_Corp) are renamed to plain labels here -- a literal `_` in tinytable's
## LaTeX output is read as a math-mode subscript trigger and garbles into
## broken typesetting (caught 2026-09-22, same family of bug as the
## unescaped %/& traps documented in ch03-summary-stats-table.R).
## width as a per-column vector (proportional, auto-normalized to 1 -- see
## ?tinytable::tt): Industry names are long free text, the other 7 columns
## are short numbers, so an equal 1/8-each split (plain width=1) wrapped
## "Industry" into up to 7 lines per row (caught 2026-09-22). Weighting
## Industry ~4x a numeric column keeps it to 1-2 lines for all 10 rows.
tbl_obj <- top_10_revenue[1:10, ] %>%
    mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
    setNames(c(
        "Industry", "SIC", "N", "Corps. (N)",
        "Market Share", "Cum. Mkt Share", "N Share", "Cum. N Share"
    )) %>%
    tt(width = c(4, 1, 1, 1, 1, 1, 1, 1))

render_thesis_table(tbl_obj, "ch03-top-industries")
cat("Saved: Thesis/tables/ch03-top-industries.{png,pdf}\n")
