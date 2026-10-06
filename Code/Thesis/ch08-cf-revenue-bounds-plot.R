## PRODUCT: Thesis/figures/ch08-cf-revenue-bounds.png := counterfactual net sales-tax revenue per firm-year (real pesos),
## evader industries' interior firms, by change Delta in the purchases rate (tau_P -> (1+Delta) tau_P), for the headline
## (true materials respond to the rate through the two-tax wedge) and the robustness check (true materials fixed).
## Sources: Code/Products/1651-cf-revenue-mr1-D<Delta>.csv (cf_mresp=1) and 1652-cf-revenue-mr0-D<Delta>.csv (fixed M;
## +-0.3 from the MacBook, suffix -macbook); Delta = 0 from 1653-cf-revenue-mr1-D0.csv (r = 1, so both versions coincide).
## mode=cfprofile, cf_target=revenue: theta fixed at the operating point 1616, gamma free, R = 1000, cf_multi with 3 starts
## (2026-10-05/06). Revenue = T x scale (scale = mean tau_P M*); bars = conservative 95% sets {T: TS <= chi2_{18,.95}}.
## Formulas: appendix A, @sec-app-cf-implementation.
source("Code/Thesis/001-setup.R")
rd <- function(f) { p <- file.path(PRODUCTS_DIR, f); pm <- sub("\\.csv$", "-macbook.csv", p)
    if (file.exists(p) && length(readLines(p)) > 1) read.csv(p) else if (file.exists(pm)) read.csv(pm) else NULL }
grid <- c(-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3)
d <- bind_rows(
    bind_rows(lapply(grid, function(x) rd(sprintf("1651-cf-revenue-mr1-D%s.csv", x)))) %>% mutate(version = "True materials respond"),
    bind_rows(lapply(grid, function(x) rd(sprintf("1652-cf-revenue-mr0-D%s.csv", x)))) %>% mutate(version = "True materials fixed"))
z <- rd("1653-cf-revenue-mr1-D0.csv")
if (!is.null(z)) d <- bind_rows(d, z %>% mutate(version = "True materials respond"), z %>% mutate(version = "True materials fixed"))
stopifnot(all(d$cf_target == "revenue"), all(is.finite(c(d$hard_lo, d$hard_hi))), all(d$TS_min < d$crit),
          nrow(d) == 2 * (length(grid) + !is.null(z)))
d <- d %>% mutate(rev = T_hat * scale, lo = hard_lo * scale, hi = hard_hi * scale,
                  version = factor(version, levels = c("True materials respond", "True materials fixed")))
print(d %>% arrange(version, Delta) %>% select(version, Delta, rev, lo, hi, TS_min))
write.csv(d %>% arrange(version, Delta) %>% select(version, Delta, rev, lo, hi, TS_min, crit),
          file.path(PRODUCTS_DIR, "ch08-cf-revenue.csv"), row.names = FALSE)
# break-even (Hans, 2026-10-06): where the headline estimate crosses zero, by linear interpolation between adjacent Delta
h <- d %>% filter(version == "True materials respond") %>% arrange(Delta)
k <- which(diff(sign(h$rev)) != 0)[1]
be <- if (is.na(k)) NA else h$Delta[k] - h$rev[k] * (h$Delta[k + 1] - h$Delta[k]) / (h$rev[k + 1] - h$rev[k])
cat(sprintf("Break-even Delta (true materials respond): %.3f\n", be))
dodge <- position_dodge(width = 1.6)
p <- ggplot(d, aes(x = 100 * Delta, y = rev, colour = version, group = version)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    { if (!is.na(be)) list(annotate("segment", x = 100 * be, xend = 100 * be, y = -Inf, yend = 0, colour = "grey55", linewidth = 0.3, linetype = "dashed"),
                           annotate("text", x = 100 * be, y = min(d$lo), label = sprintf("Break-even: %+.0f%%", 100 * be), colour = "grey35",
                                    hjust = 1.08, vjust = 0, size = 3.2, family = THESIS_FONT)) } +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 1.2, linewidth = 0.5, position = dodge) +
    geom_line(linewidth = 0.5, position = dodge) +
    geom_point(size = 2, position = dodge) +
    scale_colour_manual(values = c("True materials respond" = THESIS_COLS[1], "True materials fixed" = THESIS_COLS[2]), name = NULL) +
    scale_x_continuous(breaks = 100 * c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3), labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = "Change in the purchases tax rate", y = "Net sales-tax revenue per firm-year") +
    theme_thesis()
save_thesis_plot(p, "ch08-cf-revenue-bounds")
cat("Saved: Thesis/figures/ch08-cf-revenue-bounds.{png,pdf}; Code/Products/ch08-cf-revenue.csv\n")
