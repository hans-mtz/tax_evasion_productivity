## Observed sales-tax rate by survey year vs. the statutory rate in force
## (@tbl-st-by-year, Setting and Data chapter). Validates two things at once:
## (1) the year in the EAM panel is the ACTIVITY (reference) year, not the
## collection year -- each statutory change shows up in the year the law took
## effect, not one year later; (2) the timing of the 1983 reform: Decreto 3541
## (29 Dec 1983) applied from 1 April 1984, so 1984 mixes 3 months at 6% and
## 9 months at 10% (0.25*6 + 0.75*10 = 9%, the observed 1984 median).
## Ley 49 de 1990 art. 26: 12% from 1 January 1991.
##
## Rate = sales tax paid on sales / sales (`sales_tax_rate_sales`). Sample:
## firms with a strictly positive rate below 50% (drops ST-exempt firms and a
## handful of outliers). Built 2026-09-23.

source("Code/Thesis/001-setup.R")
source("Code/Thesis/ch03-sample.R") # ch3_base: same sample as every ch. 3 table (codes 6-9 dropped, 2026-09-26)

statutory <- c(
    "81" = "15\\% basic, 6\\% preferential",
    "82" = "15\\% basic, 6\\% preferential",
    "83" = "15\\% basic, 6\\% preferential",
    "84" = "10\\% from 1 April",
    "85" = "10\\%", "86" = "10\\%", "87" = "10\\%",
    "88" = "10\\%", "89" = "10\\%", "90" = "10\\%",
    "91" = "12\\% from 1 January"
)

## tau_P (sales tax paid on purchases / raw materials) added 2026-09-26: the
## rate at which each fictitious peso of materials is credited, the model's
## incentive. Its median is taken over firms with 0 < tau_P < 50%, separately
## from the tau_S sample.
rate_by_year <- function(v) {
    ch3_base %>%
        filter(is.finite(.data[[v]]), .data[[v]] > 0, .data[[v]] < 0.5) %>%
        group_by(year) %>%
        summarise(n = n(), median = median(.data[[v]]), mean = mean(.data[[v]]),
                  p75 = quantile(.data[[v]], 0.75), .groups = "drop")
}
s_rt <- rate_by_year("sales_tax_rate_sales")
p_rt <- rate_by_year("sales_tax_rate_purchases") %>% select(year, median_p = median)

tbl <- s_rt %>%
    left_join(p_rt, by = "year") %>%
    mutate(
        Year = paste0("19", year),
        `Statutory general rate` = statutory[as.character(year)],
        across(c(median, mean, p75, median_p), ~ sprintf("%.1f\\%%", 100 * .x)),
        n = format(n, big.mark = ",")
    ) %>%
    select(Year, `Statutory general rate`, N = n, `Median $\\tau_S$` = median,
           `Mean $\\tau_S$` = mean, `P75 $\\tau_S$` = p75, `Median $\\tau_P$` = median_p)

print(tbl)

tt_obj <- tbl |>
    tt(width = c(0.9, 2.6, 0.9, 1, 1, 1, 1),
       notes = "$\\tau_S$: sales tax charged on sales over sales; N and the $\\tau_S$ columns use firms with $0<\\tau_S<50\\%$ (ST-exempt firms excluded). $\\tau_P$: sales tax paid on purchases over raw materials, median over firms with $0<\\tau_P<50\\%$.") |>
    style_tt(i = c(4, 11), bold = TRUE) |>
    style_tt("notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch03-st-by-year")
cat("Saved: Thesis/tables/ch03-st-by-year.{png,pdf}\n")
