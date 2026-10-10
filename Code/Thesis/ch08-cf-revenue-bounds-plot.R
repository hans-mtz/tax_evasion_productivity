## PRODUCT: Thesis/figures/ch08-cf-revenue-bounds.png := 95% sets of counterfactual net sales-tax revenue per firm-year (real pesos),
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
bracket <- c(-0.225, -0.25, -0.275, -0.29)   # break-even bracket (1651, M responds only): table rows, not plotted
br <- bind_rows(lapply(bracket, function(x) rd(sprintf("1651-cf-revenue-mr1-D%s.csv", x)))) %>% mutate(version = "True materials respond")
stopifnot(nrow(br) == length(bracket), all(br$cf_target == "revenue"), all(br$cf_mresp == 1), all(is.finite(c(br$hard_lo, br$hard_hi))))
d <- bind_rows(
    bind_rows(lapply(grid, function(x) rd(sprintf("1651-cf-revenue-mr1-D%s.csv", x)))) %>% mutate(version = "True materials respond"),
    bind_rows(lapply(grid, function(x) rd(sprintf("1652-cf-revenue-mr0-D%s.csv", x)))) %>% mutate(version = "True materials fixed"))
z <- rd("1653-cf-revenue-mr1-D0.csv")
if (!is.null(z)) d <- bind_rows(d, z %>% mutate(version = "True materials respond"), z %>% mutate(version = "True materials fixed"))
stopifnot(all(d$cf_target == "revenue"), all(is.finite(c(d$hard_lo, d$hard_hi))), all(d$TS_min < d$crit),
          nrow(d) == 2 * (length(grid) + !is.null(z)))
d <- d %>% mutate(rev = T_hat * scale, lo = hard_lo * scale, hi = hard_hi * scale,
                  version = factor(version, levels = c("True materials respond", "True materials fixed")))
br <- br %>% mutate(rev = T_hat * scale, lo = hard_lo * scale, hi = hard_hi * scale, bracket = TRUE)
out <- bind_rows(d %>% mutate(bracket = FALSE), br) %>% mutate(version = as.character(version)) %>% arrange(version, Delta)
print(out %>% select(version, Delta, rev, lo, hi, TS_min, bracket))
write.csv(out %>% select(version, Delta, rev, lo, hi, TS_min, crit, bracket),
          file.path(PRODUCTS_DIR, "ch08-cf-revenue.csv"), row.names = FALSE)
# first bracket point whose 95% set excludes zero (headline)
z0 <- out %>% filter(version == "True materials respond", lo > 0) %>% arrange(desc(Delta)) %>% slice(1)
cat(sprintf("Largest Delta (closest to 0) whose set excludes zero: %.3f, set [%.0f, %.0f]\n", z0$Delta, z0$lo, z0$hi))
# break-even (Hans, 2026-10-06): where the headline estimate crosses zero, by linear interpolation between adjacent Delta
h <- d %>% filter(version == "True materials respond") %>% arrange(Delta)
k <- which(diff(sign(h$rev)) != 0)[1]
be <- if (is.na(k)) NA else h$Delta[k] - h$rev[k] * (h$Delta[k + 1] - h$Delta[k]) / (h$rev[k + 1] - h$rev[k])
cat(sprintf("Break-even Delta (true materials respond): %.3f\n", be))
# Figure (Hans 2026-10-09): true materials respond only (the fixed-M version stays in the table and Robustness); no point
# estimate; the lower bounds and the upper bounds of the 95% sets are each joined by a dashed line, in the spirit of the
# identified-set plots of AK2020. All evaluated Delta are drawn, the break-even bracket rows included.
pl <- out %>% filter(version == "True materials respond") %>% arrange(Delta) %>%
    select(Delta, lo, hi) %>% pivot_longer(c(lo, hi), names_to = "bound", values_to = "v")
p <- ggplot(pl, aes(x = 100 * Delta, y = v, group = bound)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_line(linewidth = 0.5, linetype = "dashed", colour = THESIS_COLS[1]) +
    geom_point(size = 1.4, colour = THESIS_COLS[1]) +
    scale_x_continuous(breaks = 100 * c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3), labels = function(x) paste0(ifelse(x > 0, "+", ifelse(x < 0, "\u2212", "")), abs(x), "%")) +
    scale_y_continuous(labels = function(v) sub("^-", "\u2212", scales::label_comma()(v))) +
    labs(x = expression("Change in the purchases rate, " * Delta), y = "Net sales-tax revenue per firm-year") +
    theme_thesis()
save_thesis_plot(p, "ch08-cf-revenue-bounds")
cat("Saved: Thesis/figures/ch08-cf-revenue-bounds.{png,pdf}; Code/Products/ch08-cf-revenue.csv\n")
