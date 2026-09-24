## Shared helpers for the ch. 7 (1983 reform) coefficient tables, one per
## figure: ch07-fiscal-{all,liable,llc,prt}-table.R. Built 2026-09-23.
##
## Each table has one row per year (1981-1991) and, for each group, two
## columns: the level mu_t (unincorporated minus the pooled-corporations
## baseline, industry FE) and the difference relative to 1983, Delta mu_t.
## Cells: coefficient, SE in parentheses, stars (10/5/1%). SEs are two-way
## clustered by plant and year, exactly as in the source regressions
## (Code/Deconvolution/921.1-DD.R, 921.2-het-slides.R).
##
## The outcome in the source regressions is the log materials share with
## industry fixed effects; with those fixed effects the year coefficients are
## numerically identical to using V (= log share - ln D_j, a per-industry
## constant), up to any sample difference between wip_df and the stage-1
## sample (ch. 7, @eq-fiscal-lvl-reg).

source("Code/Thesis/001-setup.R")
suppressMessages(library(fixest))

YEARS <- 81:91

fmt_cell <- function(b, s, p) {
    stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
    sprintf("%.3f%s (%.3f)", b, stars, s)
}

## Pull the year path for one group out of a fixest model: coefficient names
## are "<prefix><year>", e.g. "corp_exempt_year::Other:Exempt:84".
## Missing years (the 1983 reference in the difference models) become "--".
year_path <- function(model, prefix) {
    ct <- coeftable(model)
    sapply(YEARS, function(y) {
        nm <- paste0(prefix, y)
        if (!nm %in% rownames(ct)) return("--")
        fmt_cell(ct[nm, 1], ct[nm, 2], ct[nm, 4])
    })
}

## Build and render one table. `cols` is a named list: column label ->
## list(model, prefix). `groups` (optional) is passed to group_tt(j = ...).
fiscal_table <- function(cols, slug, groups = NULL, note_extra = NULL) {
    tbl <- data.frame(Year = paste0("19", YEARS), check.names = FALSE)
    for (lab in names(cols)) tbl[[lab]] <- year_path(cols[[lab]]$model, cols[[lab]]$prefix)
    nobs_all <- unique(sapply(cols, function(c) nobs(c$model)))
    note <- paste0(
        "Coefficients with standard errors in parentheses, two-way clustered by plant and year. ",
        "Industry fixed effects in all columns. Level: $\\hat\\mu_t$; Diff.: $\\Delta\\hat\\mu_t=\\hat\\mu_t-\\hat\\mu_{1983}$ (1983 is the reference, --). ",
        "* p$<$0.1, ** p$<$0.05, *** p$<$0.01. Observations: ",
        paste(format(nobs_all, big.mark = ","), collapse = ", "), ".",
        if (!is.null(note_extra)) paste0(" ", note_extra) else ""
    )
    tt_obj <- tt(tbl, width = c(0.8, rep(1, ncol(tbl) - 1)), notes = note)
    if (!is.null(groups)) tt_obj <- group_tt(tt_obj, j = groups)
    tt_obj <- style_tt(tt_obj, "notes", fontsize = 0.8)
    print(tbl)
    render_thesis_table(tt_obj, slug)
    cat("Saved: Thesis/tables/", slug, ".{png,pdf}\n", sep = "")
}
