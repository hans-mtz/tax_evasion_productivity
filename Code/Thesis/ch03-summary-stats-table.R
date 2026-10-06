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
source("Code/Thesis/ch03-sample.R") # ch3_base: sample rule, codes 6-9 dropped (2026-09-26)

## Rows (2026-09-26, Hans): only what later chapters use or a claim rests on --
## the log production-function variables and the two sales-tax rates (the
## materials share dropped: it is m - y exactly, same deflator) of the two-tax model: tau_S = tax on sales / sales, tau_P =
## tax on purchases / materials. Production-function variables in logs, as
## they enter the estimation (Hans, 2026-09-26): y = log real gross output,
## k = log real capital, l = log employee-years, m = log real RAW MATERIALS
## (`materials`, what first_stage_panel_me rebuilds m from; the `m` column in
## colombia_data_frame is log intermediates and is only used as a filter). Dropped: energy, fuels, repair & maintenance, services,
## and the skilled/unskilled wage split (none used downstream). Values above 1 are treated as data errors and set to
## missing (counted in the Missing column).
base <- ch3_base %>%
    mutate(m_raw = ifelse(is.finite(log(materials)), log(materials), NA_real_),
           across(c(sales_tax_rate_sales, sales_tax_rate_purchases),
                  ~ ifelse(is.finite(.x) & .x <= 1, .x, NA_real_)))
cat(sprintf("ch. 3 sample: %d firm-years, %d plants, %d industries, %d-%d\n",
            nrow(base), n_distinct(base$plant), n_distinct(base$sic_3),
            1900 + min(base$year), 1900 + max(base$year)))

## --- Table 1: numeric skim, materials share + sales-tax rates ---------------

share_labels <- c(
    y                        = "Output (log)",
    k                        = "Capital (log)",
    l                        = "Labour (log)",
    m_raw                    = "Raw materials, as reported (log)",
    sales_tax_rate_sales     = "Sales-tax rate on sales",
    sales_tax_rate_purchases = "Sales-tax rate on purchases"
)

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

## Per-row formatting: logs to two decimals, shares and rates to three.
log_rows <- share_labels[c("y", "k", "l", "m_raw")]
fmt_row <- function(x, v) if (v %in% log_rows) sprintf("%.2f", x) else sprintf("%.3f", x)
skim_tbl <- skim_tbl %>%
    rowwise() %>%
    mutate(across(c(Mean, SD, Q1, Median, Q3), ~ fmt_row(.x, Variable))) %>%
    ungroup() %>%
    mutate(`Missing (\\%)` = sprintf("%.2f", `Missing (\\%)`))

## No caption= here (nor below): Quarto's own crossref numbering/caption on
## the ![...]{#tbl-...} markdown reference is the single source of the
## caption now, so the R-generated image doesn't carry a second, redundant
## one (decided in chat 2026-09-22). width as a per-column vector: the
## Variable labels are longer than the 6 numeric columns, so weighted ~3x.
skim_tt <- skim_tbl %>%
    tt(width = c(3.8, 0.8, 1, 1.1, 0.9, 0.9, 1), notes = "Firm-years with finite output, capital, labour and materials; corporations, LLCs, partnerships and proprietorships. Rate on sales: sales tax paid on sales over sales. Rate on purchases: sales tax paid on purchases over raw materials. Values above 1 are treated as data errors and counted as missing. Output, capital and raw materials are in logs of 1981 pesos; labour is in logs of employee-years.") %>%
    style_tt(i = "notes", fontsize = 0.8) %>%
    style_tt(j = 2:7, align = "r")

render_thesis_table(skim_tt, "ch03-summary-stats")
cat("Saved: Thesis/tables/ch03-summary-stats.{png,pdf}\n")

## --- Table 2: juridical organization composition --------------------------

jo_tt <- base %>%
    group_by(`J. Org.` = jo) %>%
    summarise(`Firm-years` = n(), Plants = n_distinct(plant), .groups = "drop") %>%
    mutate(`\\% of firm-years` = sprintf("%.1f", 100 * `Firm-years` / sum(`Firm-years`)),
           across(c(`Firm-years`, Plants), ~ format(.x, big.mark = ","))) %>%
    tt(width = 1, notes = "A plant that changes juridical organization is counted once in each.") %>%
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(jo_tt, "ch03-jo-summary")
cat("Saved: Thesis/tables/ch03-jo-summary.{png,pdf}\n")
