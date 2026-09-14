## Counterfactual (Delta,R) grid: revenue vs. tax change, pass/fail under both
## the conservative (Theorem F.1, absolute TS=2n*Lhat) and the min-subtracted
## CHT-style (TS=2n*(Lhat-Lhat_min)) tests, chi2_10,.95 critical value
## (d_g=10: moment set A's 9 rows + 1 revenue auxiliary moment). 2026-09-10.

library(tidyverse)

df <- read.csv("Code/Products/1262-revgrid-lag_m-combined.csv")

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)
Lmin <- min(df$Lhat)
df <- df %>%
    mutate(
        TS_diff = 2 * n * (Lhat - Lmin),
        TS_abs  = 2 * n * Lhat,
        pass_diff = TS_diff <= qc,
        pass_abs  = TS_abs  <= qc,
        cat = case_when(
            pass_abs  ~ "Passes conservative (TS_abs)",
            pass_diff ~ "Passes diff-test only (TS_diff)",
            TRUE      ~ "Fails both"
        )
    )

## At this grid's own minimum Lhat~2.6e-11 (essentially 0), TS_diff and
## TS_abs are numerically indistinguishable everywhere -- both tests agree
## on every single point tested here (no point passes one but not the
## other). Noted in the subtitle rather than forcing a visual distinction
## that isn't actually present in the data.
stopifnot(all(df$pass_abs == df$pass_diff))

wong_red <- "#D55E00"; wong_blue <- "#0072B2"

p <- ggplot(df, aes(x = Delta, y = R)) +
    geom_line(aes(group = R), color = "grey75", linewidth = 0.4, linetype = "dotted") +
    geom_point(aes(color = cat, shape = cat, size = pmin(TS_abs, 40))) +
    geom_text(aes(label = sprintf("TS=%.2g", TS_abs)), vjust = -1.3, size = 2.9, family = "Times") +
    scale_color_manual(values = c(
        "Passes conservative (TS_abs)"   = wong_red,
        "Passes diff-test only (TS_diff)" = "#E69F00",
        "Fails both"                      = wong_blue
    )) +
    scale_shape_manual(values = c(
        "Passes conservative (TS_abs)"   = 16,
        "Passes diff-test only (TS_diff)" = 17,
        "Fails both"                      = 4
    )) +
    scale_size_continuous(range = c(2, 7), guide = "none") +
    scale_x_continuous(breaks = sort(unique(df$Delta)),
                        labels = scales::percent(sort(unique(df$Delta)))) +
    labs(x = expression(Delta~"(change in purchases-side tax rate)"),
         y = "R (real revenue per firm-period, deflated)",
         color = NULL, shape = NULL,
         title = "Counterfactual (Delta,R) grid: lag_m, moment set A + revenue moment (d_g=10)",
         subtitle = sprintf("chi2_10,.95=%.3g. TS_diff and TS_abs agree on every tested point (grid min Lhat~0). n_keep=3000, full theta+gamma profiling/cell.", qc)) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 8.5, hjust = 0.5),
        legend.position = "bottom",
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1263-stage2-revgrid-delta-R.png", p, width = 9.5, height = 7, dpi = 300)
cat("Saved: Paper/images/1263-stage2-revgrid-delta-R.png\n")
