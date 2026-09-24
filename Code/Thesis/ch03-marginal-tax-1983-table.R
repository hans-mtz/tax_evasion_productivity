## PRODUCT: Thesis/tables/ch03-marginal-tax-1983.png := implied MARGINAL
## income-tax rates by income interval, 1982 vs. 1983 schedules
## (@tbl-marg-tax-1983, Setting and Data chapter). Built 2026-09-23.
##
## Perry and Cardenas (1986) report only AVERAGE rates (Cuadro III.1). Tax at
## each point is T = a*y, so the average marginal rate over an interval is
## m = (T2 - T1)/(y2 - y1). With nine points, each figure averages over whatever
## the schedule does inside the interval (deduction phase-ins, the
## discontinuities Perry and Cardenas mention on p. 36), so read it as an
## interval average, not the statutory rate at a given income.
source("Code/Thesis/001-setup.R")
source("Code/Thesis/ch03-cuadro-III1-data.R")

d <- cuadro_III1 %>% mutate(T82 = rate_1982A / 100 * income, T83 = rate_1983 / 100 * income)
marg <- tibble(
    lo = head(d$income, -1), hi = tail(d$income, -1),
    m82 = 100 * diff(d$T82) / diff(d$income),
    m83 = 100 * diff(d$T83) / diff(d$income)
) %>% mutate(change = m83 - m82)
print(marg)

tbl <- marg %>% transmute(
    `Taxable income interval (thousand 1982 pesos)` = paste0(trimws(format(lo, big.mark = ",")), "--", trimws(format(hi, big.mark = ","))),
    `1982 schedule` = sprintf("%.1f\\%%", m82),
    `1983 schedule` = sprintf("%.1f\\%%", m83),
    `Change (pp)` = sprintf("%+.1f", change)
)
tt_obj <- tbl |>
    tt(width = c(2.4, 1, 1, 1),
       notes = "Implied marginal rates: the change in tax over the change in income between consecutive points of Perry and C\\'ardenas (1986), Cuadro III.1 (average rates). Each figure is an average over the interval.") |>
    style_tt("notes", fontsize = 0.8)
render_thesis_table(tt_obj, "ch03-marginal-tax-1983")
cat("Saved: Thesis/tables/ch03-marginal-tax-1983.{png,pdf}\n")
