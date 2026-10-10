## PRODUCT: Thesis/figures/ch08-cf-revenue-totals.png := counterfactual net sales-tax revenue, total over every firm-year in
## the sample (1981-1991, millions of real pesos), by change Delta in the purchases rate, true materials responding (headline).
## Whole economy only (exercise B of 1621; Hans 2026-10-09: the evader-industries exercise A is not reported): the ELVIS
## interior firms, the top 0.5% trimmed from estimation, and the corner firms, whose claims are (1+Delta) tau_P r M* and
## gross sales-tax revenue t1 r^beta (no overreporting, no response of overreporting).
## Source: Code/Products/ch08-cf-revenue-totals.csv (written by ch08-cf-revenue-totals-table.R from 1621-cf-economy-revenue-mr1.csv).
## Dashed lines join the lower and the upper bounds of the conservative 95% sets; the sets carry only the interior firms' uncertainty (the other groups add a known amount).
## Break-even (printed for the text, not drawn): where the estimate crosses zero, by linear interpolation between adjacent Delta (as ch08-cf-revenue-bounds-plot.R).
## Formulas: appendix A, @sec-app-cf-implementation.
source("Code/Thesis/001-setup.R")

d <- read.csv(file.path(PRODUCTS_DIR, "ch08-cf-revenue-totals.csv")) %>% arrange(Delta) %>%
    transmute(Delta, rev = revenue_B / 1e6, lo = revenue_B_hard_lo / 1e6, hi = revenue_B_hard_hi / 1e6)
stopifnot(0 %in% d$Delta, all(is.finite(c(d$rev, d$lo, d$hi))), all(d$lo <= d$rev & d$rev <= d$hi))
print(d)
k <- which(diff(sign(d$rev)) != 0)[1]
be <- if (is.na(k)) NA else d$Delta[k] - d$rev[k] * (d$Delta[k + 1] - d$Delta[k]) / (d$rev[k + 1] - d$rev[k])
cat(sprintf("Break-even Delta (whole economy, true materials respond): %.3f\n", be))

# Hans 2026-10-10: same style as fig-cf-revenue (ch08-cf-revenue-bounds-plot.R): no point estimate, no break-even line (the
# text gives it); the lower bounds and the upper bounds of the 95% sets are each joined by a dashed line.
pl <- d %>% select(Delta, lo, hi) %>% pivot_longer(c(lo, hi), names_to = "bound", values_to = "v")
p <- ggplot(pl, aes(x = 100 * Delta, y = v, group = bound)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_line(linewidth = 0.5, linetype = "dashed", colour = THESIS_COLS[1]) +
    geom_point(size = 1.4, colour = THESIS_COLS[1]) +
    scale_x_continuous(breaks = 100 * c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3), labels = function(x) paste0(ifelse(x > 0, "+", ifelse(x < 0, "−", "")), abs(x), "%")) +
    scale_y_continuous(labels = function(v) sub("^-", "−", scales::label_comma()(v))) +
    labs(x = expression("Change in the purchases rate, " * Delta), y = "Net sales-tax revenue, millions of real pesos") +
    theme_thesis()
save_thesis_plot(p, "ch08-cf-revenue-totals")
cat("Saved: Thesis/figures/ch08-cf-revenue-totals.{png,pdf}\n")
