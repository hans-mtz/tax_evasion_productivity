## PRODUCT: Thesis/figures/ch08-cf-claims-bounds.png := counterfactual claimed purchase deductions per firm-year
## (real pesos), evader industries' interior firms, by change Delta in the purchases rate (tau_P -> (1+Delta) tau_P).
## Source: Code/Products/1622-cf-i-k0.75-kappa0.5.csv (mode=cfprofile, 2026-10-02): theta fixed at the passing
## operating point (design i, k = 0.75, kappa = 0.5, TS 23.7 < chi2_17), gamma free; T_hat = argmin of the profiled
## statistic (golden section); bounds by test inversion, located by bisection: hard = {T: TS <= chi2_{d_g,0.95}}
## (Theorem F.1), soft = {T: TS - TS_min <= chi2_{1,0.95}}. Claimed deductions = T x scale (scale = mean tau_P M*).
## Grey band = the current policy's (Delta = 0) hard interval; dashed grey = mechanical response (1+Delta) x baseline
## (no behavioural response). Same layout as ch08-claims-ci-hard-plot.R.
source("Code/Thesis/001-setup.R")
cf <- read.csv(file.path(PRODUCTS_DIR, "1622-cf-i-k0.75-kappa0.5.csv"))
stopifnot(all(is.finite(c(cf$hard_lo, cf$hard_hi, cf$soft_lo, cf$soft_hi))))   # an open (Inf) or empty (NaN) set needs explicit handling
delta_pct <- function(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 2), "%")
d <- cf %>% mutate(c_hat = T_hat * scale, hard_lo = hard_lo * scale, hard_hi = hard_hi * scale,
                   soft_lo = soft_lo * scale, soft_hi = soft_hi * scale,
                   mech = (1 + Delta) * c_hat[Delta == 0],
                   Delta_f = factor(Delta, levels = sort(Delta), labels = delta_pct(sort(Delta))))
print(d %>% select(Delta, c_hat, hard_lo, hard_hi, soft_lo, soft_hi, mech, TS_min))
z <- d %>% filter(Delta == 0)
band <- d %>% select(Delta_f, hard_lo, hard_hi) %>% pivot_longer(-Delta_f, names_to = "bound", values_to = "y") %>%
    mutate(bound = recode(bound, hard_lo = "Lower bound (95% test)", hard_hi = "Upper bound (95% test)"))
p <- ggplot(d, aes(x = Delta_f)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = z$hard_lo, ymax = z$hard_hi, fill = THESIS_BAND) +
    geom_linerange(aes(ymin = soft_lo, ymax = soft_hi, colour = "Soft region"), linewidth = 4) +
    geom_line(data = band, aes(y = y, group = bound, linetype = bound), colour = THESIS_COLS[1], linewidth = 0.5) +
    geom_line(aes(y = mech, group = 1, linetype = "Mechanical (no behavioural response)"), colour = "grey45", linewidth = 0.5) +
    geom_point(aes(y = c_hat, shape = "Estimate"), colour = THESIS_COLS[1], size = 2.2) +
    scale_linetype_manual(values = c("Lower bound (95% test)" = "dotted", "Upper bound (95% test)" = "twodash",
                                     "Mechanical (no behavioural response)" = "dashed"),
                          breaks = c("Upper bound (95% test)", "Lower bound (95% test)", "Mechanical (no behavioural response)")) +
    scale_shape_manual(values = c("Estimate" = 16)) +
    scale_colour_manual(values = c("Soft region" = THESIS_LIGHT)) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = "Change in the purchases tax rate", y = "Claimed deductions per firm-year") +
    theme_thesis() + theme(legend.box = "horizontal") + guides(linetype = guide_legend(ncol = 1, order = 3), shape = guide_legend(order = 1, ncol = 1), colour = guide_legend(order = 2, ncol = 1))
save_thesis_plot(p, "ch08-cf-claims-bounds")
cat("Saved: Thesis/figures/ch08-cf-claims-bounds.{png,pdf}\n")
