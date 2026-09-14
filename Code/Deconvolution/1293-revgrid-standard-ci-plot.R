## STANDARD counterfactual (Delta, R) test-inversion plot -- analogous to
## AK2020 Appendix F (F_figure1.R): x-axis = policy shifter (their kappa, our
## Delta), y-axis = the outcome (their theta_pet, our revenue R), passing
## points colored, non-passing points light gray, upper/lower bound of the
## passing region connected across Delta (their "upper bound"/"lower bound"
## twodash/dotted lines). This is now the standard chart for any (Delta,R)
## counterfactual result in this project -- extend this script rather than
## inventing a new plot design.
##
## Combines BOTH overnight fixed-theta runs (2026-09-11/12) so every tested
## (Delta,R) candidate appears: 1290 (narrow, +/-20% of baseline R) and 1291
## (wide, -50% to +150% of baseline R). Test used: TS_soft (min-subtracted
## CHT) -- the operative test for the confidence set per CLAUDE.md.
##
## A Delta whose passing region touches the EDGE of the tested R-grid on
## either side is a real, documented open-ended result (Delta=+1% passes at
## both tested extremes -- "this leg alone needs an even wider grid") --
## flagged with an open triangle + dashed extension arrow rather than drawn
## as if it were a closed CI, per this project's standing honesty-over-
## tidiness convention on set-identified results.

library(tidyverse)

narrow <- read.csv("Code/Products/1290-revgrid-fixedtheta-lag_m-analyzed.csv")
wide   <- read.csv("Code/Products/1291-revgrid-fixedtheta-wide-lag_m-analyzed.csv")

df <- bind_rows(narrow, wide) %>%
    select(Delta, R, TS_soft, pass_soft) %>%
    mutate(R_round = round(R, 0)) %>%
    distinct(Delta, R_round, .keep_all = TRUE) %>%
    select(-R_round)

n <- 32232; dg <- 10
qc95 <- qchisq(0.95, dg)

## per-Delta bounds of the passing region + whether each bound sits at the
## edge of the TESTED grid (open-ended, not a real rejection boundary)
bounds <- df %>%
    group_by(Delta) %>%
    summarise(
        r_min_tested = min(R), r_max_tested = max(R),
        lower = suppressWarnings(min(R[pass_soft])),
        upper = suppressWarnings(max(R[pass_soft])),
        .groups = "drop"
    ) %>%
    mutate(
        open_low  = is.finite(lower) & lower  <= r_min_tested + 1e-6,
        open_high = is.finite(upper) & upper >= r_max_tested - 1e-6
    )

band <- bounds %>%
    filter(is.finite(lower)) %>%
    select(Delta, lower, upper) %>%
    pivot_longer(c(lower, upper), names_to = "bound", values_to = "R") %>%
    mutate(bound = recode(bound, lower = "Lower bound", upper = "Upper bound"))

wong_blue <- "#0072B2"; gray_fail <- "grey75"

delta_pct <- function(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 2), "%")

delta_levels <- sort(unique(df$Delta))

df <- df %>% mutate(pass_lab = ifelse(pass_soft, "Passes (95% test)", "Rejected"),
                     Delta_f = factor(Delta, levels = delta_levels, labels = delta_pct(delta_levels)))
band <- band %>% mutate(Delta_f = factor(Delta, levels = delta_levels, labels = delta_pct(delta_levels)))

## open-ended annotation: short dashed rays pushing past the tested edge for
## Delta=+1%, both bounds -- flags "CI not yet closed here", not a plotting bug
open_rays <- bounds %>%
    filter(open_low | open_high) %>%
    rowwise() %>%
    mutate(ray = list(tibble(
        R = c(if (open_low) lower - 0.12 * (r_max_tested - r_min_tested) else NA,
              if (open_high) upper + 0.12 * (r_max_tested - r_min_tested) else NA)
    ))) %>%
    ungroup() %>%
    select(Delta, ray) %>%
    unnest(ray) %>%
    mutate(Delta_f = factor(Delta, levels = delta_levels, labels = delta_pct(delta_levels))) %>%
    filter(!is.na(R))

p <- ggplot() +
    geom_line(data = band, aes(x = Delta_f, y = R, group = bound, linetype = bound),
              color = wong_blue, linewidth = 0.8) +
    geom_point(data = df, aes(x = Delta_f, y = R, color = pass_lab, shape = pass_lab),
               size = 2.6, alpha = 0.9) +
    geom_point(data = open_rays, aes(x = Delta_f, y = R),
               shape = 17, color = wong_blue, size = 2.6, alpha = 0.7) +
    scale_color_manual(values = c("Passes (95% test)" = wong_blue, "Rejected" = gray_fail), name = NULL) +
    scale_shape_manual(values = c("Passes (95% test)" = 16, "Rejected" = 4), name = NULL) +
    scale_linetype_manual(values = c("Upper bound" = "twodash", "Lower bound" = "dotted"), name = NULL) +
    guides(color = guide_legend(order = 1), shape = guide_legend(order = 1),
           linetype = guide_legend(order = 2, override.aes = list(color = wong_blue))) +
    labs(
        x = expression(paste("Purchases-tax shifter ", Delta, " (candidate policy change)")),
        y = "Real mean revenue per firm-period, R (COP)",
        title = "Counterfactual test inversion: candidate (Δ, R) pairs, fixed θ",
        subtitle = paste0("Blue = passes soft (min-subtracted CHT) test at 95% (χ²₁₀=", round(qc95,1),
                           "); gray × = rejected. Lines join the passing region's bounds.\n",
                           "Filled triangles at Δ=+1%: passing region still open at the tested edge — needs a wider R-grid, not yet a closed CI."),
        caption = "Combined 1290 (±20% grid) + 1291 (-50%/+150% grid), both fixed θ=(δ₀,λ,δ₁,δ₂)=(3.464, 5.427e-7, 4.3, 0.54)."
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 10, hjust = 0.5, color = "grey30"),
        plot.caption = element_text(family = "Times", size = 8, color = "grey40", hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1293-revgrid-standard-ci.png", p, width = 10, height = 7.5, dpi = 300)
cat("Saved: Paper/images/1293-revgrid-standard-ci.png\n")
cat("\nPer-Delta passing-region bounds (soft test):\n")
print(bounds %>% select(Delta, lower, upper, open_low, open_high))
