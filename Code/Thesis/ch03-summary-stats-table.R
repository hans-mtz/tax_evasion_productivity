## Summary statistics for the Setting and Data chapter (@tbl-sum-stats,
## @tbl-jo-summary). Replaces Paper/sections/90-colombia-data.qmd's
## `tbl-sum-stats` chunk (modelsummary::datasummary_skim(), inline). That
## dependency (modelsummary) isn't installed in the project's renv library;
## since the column set here is fixed and known, the same skim statistics
## (Missing %, Mean, SD, Q1, Median, Q3) are computed directly with dplyr and
## rendered with tinytable -- no new dependency, and the categorical variable
## (J. Org.) that datasummary_skim handled separately under the hood is now
## an explicit second table instead of a merged block whose exact row
## indices weren't reproducible from the legacy chunk alone.
## Decided in chat 2026-09-22: split, not reproduce-in-one-table.

source("Code/Thesis/001-setup.R")
load(file.path(PRODUCTS_DIR, "colombia_data.RData"))

base <- colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m)
    ) %>%
    mutate(
        JO_class = factor(JO_class, levels = c("Proprietorship", "Ltd. Co.", "Corporation", "Partnership"))
    )

## --- Table 1: numeric skim, revenue shares + intermediates decomposition --

share_labels <- c(
    share_sales_tax                    = "Sales Taxes",
    skilled_wage_bill_share            = "Skilled Labor (Wages)",
    unskilled_wage_bill_share          = "Unskilled Labor (Wages)",
    capital_share                      = "Capital",
    materials_share                    = "Materials (M)",
    energy_share                       = "Electricity (E)",
    fuels_share                        = "Fuels (F)",
    repair_maint_share                 = "Repair \\& Maintenance (R\\&M)",
    services_share                     = "Services (S)",
    deductible_intermediates_share     = "Deductible Inter. (M+E+F+R\\&M)",
    non_deductible_intermediates_share = "Non-Deductible Inter. (S)"
)
revenue_vars <- c("share_sales_tax", "skilled_wage_bill_share", "unskilled_wage_bill_share", "capital_share")
intermediate_vars <- setdiff(names(share_labels), revenue_vars)

skim_one <- function(x) {
    tibble(
        `Missing (\\%)` = 100 * mean(is.na(x)),
        Mean   = mean(x, na.rm = TRUE),
        SD     = sd(x, na.rm = TRUE),
        Q1     = quantile(x, .25, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        Q3     = quantile(x, .75, na.rm = TRUE)
    )
}

skim_tbl <- base %>%
    select(all_of(names(share_labels))) %>%
    summarise(across(everything(), skim_one)) %>%
    unnest(everything(), names_sep = "__") %>%
    pivot_longer(everything(), names_sep = "__", names_to = c("variable", "stat")) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(
        variable = factor(variable, levels = names(share_labels)),
        Variable = share_labels[as.character(variable)]
    ) %>%
    arrange(variable) %>%
    select(Variable, `Missing (\\%)`, Mean, SD, Q1, Median, Q3)

revenue_rows <- which(skim_tbl$Variable %in% share_labels[revenue_vars])
intermediate_rows <- which(skim_tbl$Variable %in% share_labels[intermediate_vars])

## group_tt() inserts a header row before each named position and shifts
## every later row down by one per header already inserted -- style_tt()
## must target the POST-insertion row numbers, not the ones computed above
## from the ungrouped table (caught 2026-09-22: bolding landed on "Capital",
## the last row of group 1, instead of the two group-header rows).
header1_row <- 1
header2_row <- 1 + length(revenue_vars) + 1

## No caption= here (nor below): Quarto's own crossref numbering/caption on
## the ![...]{#tbl-...} markdown reference is the single source of the
## caption now, so the R-generated image doesn't carry a second, redundant
## one (decided in chat 2026-09-22). width as a per-column vector (see
## ch03-top-industries-table.R's note): Variable holds longer text (e.g.
## "Deductible Inter. (M+E+F+R&M)") than the 6 numeric stat columns, so it's
## weighted ~2.5x to avoid the 3-line wrapping an equal 1/7-each split gave.
skim_tt <- skim_tbl %>%
    tt(digits = 3, width = c(2.5, 1, 1, 1, 1, 1, 1), notes = "Sample: firm-years with finite output, capital, labor, and materials (n as in the text). Shares are of total revenue.") %>%
    group_tt(i = list(
        "Share of Revenues" = min(revenue_rows),
        "Intermediates (Share of Revenues)" = min(intermediate_rows)
    )) %>%
    style_tt(i = c(header1_row, header2_row), bold = TRUE)

render_thesis_table(skim_tt, "ch03-summary-stats")
cat("Saved: Thesis/tables/ch03-summary-stats.{png,pdf}\n")

## --- Table 2: juridical organization composition --------------------------

jo_tt <- base %>%
    mutate(JO_class = forcats::fct_na_value_to_level(JO_class, "Missing")) %>%
    count(`J. Org.` = JO_class, name = "N") %>%
    mutate(`\\%` = round(100 * N / sum(N), 1)) %>%
    tt(width = 1)

render_thesis_table(jo_tt, "ch03-jo-summary")
cat("Saved: Thesis/tables/ch03-jo-summary.{png,pdf}\n")
