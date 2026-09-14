## Revenue level + %-change-from-previous-step plot, Delta in [-0.04, 0.01],
## gap at Delta=0 (no value plotted there -- it's the base, nothing to report).
## Two panels, same x-range, sharing the axis. 2026-09-10.
##
## DELIBERATELY MIXED resolution, per the user's explicit call after seeing
## the fine (0.001-step) version on both sides: the fine grid (1281) was
## tried and rejected for the cut side -- it diluted the actual headline
## story. Cut side uses the NATIVE 1pp-step grid (1275): "a 1% cut buys
## +3.51%, the next 1% cut only +0.77% -- diminishing fast." Increase side
## stays at its native 0.1pp resolution (1276): "a mere 0.1% increase already
## costs -3.13%, nearly erasing the entire benefit of that first full 1% cut
## at a tenth of the move." The contrast IS the story -- forcing both sides
## to the same step size (either all coarse or all fine) was tried and both
## versions buried this asymmetry instead of showing it.

library(tidyverse)
library(cowplot)

neg <- read.csv("Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv") %>%
    mutate(Delta = round(Delta, 6)) %>%
    filter(Delta >= -0.04, Delta < 0) %>%
    arrange(desc(Delta))
pos <- read.csv("Code/Products/1276-revenue-onset-finegrid-lag_m.csv") %>%
    filter(Delta > 0, Delta <= 0.01) %>%
    arrange(Delta)
base_row <- read.csv("Code/Products/1276-revenue-onset-finegrid-lag_m.csv") %>% filter(Delta == 0)
R0 <- base_row$mean_R_real

neg2 <- neg %>%
    mutate(
        prev_R = lag(mean_R_real, default = R0[1]),
        pct_from_prev = (mean_R_real - prev_R) / prev_R
    )
pos2 <- pos %>%
    mutate(
        prev_R = lag(mean_R_real, default = R0[1]),
        pct_from_prev = (mean_R_real - prev_R) / prev_R
    )

level <- bind_rows(neg2, base_row, pos2) %>% arrange(Delta)
pct <- bind_rows(neg2, pos2) %>% arrange(Delta)  # base excluded -- nothing to report there

wong_blue <- "#0072B2"; wong_orange <- "#E69F00"

p_level <- ggplot(level, aes(x = Delta, y = mean_R_real)) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.5, linetype = "dashed") +
    geom_line(color = wong_blue, linewidth = 1.1) +
    geom_point(data = filter(level, Delta == 0), color = "grey20", size = 2.6) +
    annotate("text", x = 0.0007, y = max(level$mean_R_real) * 0.985,
             label = "current\npolicy", hjust = 0, family = "Times", size = 3.2,
             color = "grey40", lineheight = 0.85) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(-0.04, 0.01, 0.01)) +
    scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
    labs(x = NULL, y = "Mean real revenue\nper firm-period",
         title = "Revenue around current policy",
         subtitle = "Pure forward-simulation at the best-fit operating point — not yet run through the (Δ,R) test-inversion grid") +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(family = "Times", size = 13.5, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 9, hjust = 0.5, color = "grey35"),
        text = element_text(family = "Times")
    )

headline <- pct %>% filter(Delta %in% c(-0.01, -0.02, 0.001))

p_pct <- ggplot(pct, aes(x = Delta, y = pct_from_prev)) +
    geom_hline(yintercept = 0, color = "grey80", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.5, linetype = "dashed") +
    geom_line(aes(group = sign(Delta)), color = wong_orange, linewidth = 1.1) +
    geom_point(color = wong_orange, size = 2) +
    geom_point(data = headline, color = "grey15", size = 3) +
    annotate("text", x = -0.01, y = 0.0351 + 0.006, label = "1st 1% cut:\n+3.51%",
             hjust = 0.5, family = "Times", size = 3, color = "grey15", lineheight = 0.9) +
    annotate("text", x = -0.02, y = 0.0077 + 0.006, label = "next 1% cut:\nonly +0.77%",
             hjust = 0.5, family = "Times", size = 3, color = "grey15", lineheight = 0.9) +
    annotate("text", x = 0.001, y = -0.0313 + 0.006, label = "a mere 0.1% increase:\n-3.13%",
             hjust = 0, vjust = 0, family = "Times", size = 3, color = "grey15", lineheight = 0.9) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(-0.04, 0.01, 0.01)) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    labs(x = expression(Delta~"(purchases-tax rate change)"), y = "% change in revenue\nfrom the previous step",
         title = "The headline in numbers",
         subtitle = "a 1% cut buys +3.51%, the next only +0.77% — a 0.1% increase alone costs -3.13%") +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(family = "Times", size = 12.5, face = "bold", hjust = 0.5, color = "grey15"),
        plot.subtitle = element_text(family = "Times", size = 9.5, hjust = 0.5, color = "grey35"),
        text = element_text(family = "Times")
    )

combined <- plot_grid(p_level, p_pct, ncol = 1, rel_heights = c(1, 1), align = "v")

ggsave("Paper/images/1280-pct-change-from-prev.png", combined, width = 9, height = 8, dpi = 300)
cat("Saved: Paper/images/1280-pct-change-from-prev.png\n")
