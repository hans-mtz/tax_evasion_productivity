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
load(file.path(PRODUCTS_DIR, "colombia_data.RData")) # colombia_data_frame

statutory <- c(
    "81" = "15\\% basic, 6\\% preferential",
    "82" = "15\\% basic, 6\\% preferential",
    "83" = "15\\% basic, 6\\% preferential",
    "84" = "10\\% from 1 April",
    "85" = "10\\%", "86" = "10\\%", "87" = "10\\%",
    "88" = "10\\%", "89" = "10\\%", "90" = "10\\%",
    "91" = "12\\% from 1 January"
)

tbl <- colombia_data_frame %>%
    ungroup() %>%
    filter(is.finite(sales_tax_rate_sales),
           sales_tax_rate_sales > 0, sales_tax_rate_sales < 0.5) %>%
    group_by(year) %>%
    summarise(
        n = n(),
        median = median(sales_tax_rate_sales),
        mean = mean(sales_tax_rate_sales),
        p75 = quantile(sales_tax_rate_sales, 0.75),
        .groups = "drop"
    ) %>%
    mutate(
        Year = paste0("19", year),
        `Statutory general rate` = statutory[as.character(year)],
        across(c(median, mean, p75), ~ sprintf("%.1f\\%%", 100 * .x)),
        n = format(n, big.mark = ",")
    ) %>%
    select(Year, `Statutory general rate`, N = n, Median = median, Mean = mean, P75 = p75)

print(tbl)

tt_obj <- tbl |>
    tt(width = c(1, 3, 1, 1, 1, 1),
       notes = "Rate = sales tax paid on sales over sales. Firms with a rate strictly between 0 and 50\\% (ST-exempt firms excluded).") |>
    style_tt(i = c(4, 11), bold = TRUE) |>
    style_tt("notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch03-st-by-year")
cat("Saved: Thesis/tables/ch03-st-by-year.{png,pdf}\n")
