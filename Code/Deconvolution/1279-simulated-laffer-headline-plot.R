## Simulated Laffer-curve headline plot (fallback for the real tested (Delta,R)
## grid, in case there isn't time to finish that before presenting): pure
## forward-simulation, no optimization/test-inversion, at the best no-eta
## lag_m operating point (lambda=3.501e-7, delta1=4.315, delta2=0.541).
##
## Zoomed to Delta in [-0.10, 0.02] per the user's request. Revised 2026-09-10
## after the user flagged that lumping the cut side (Delta<0) and the
## reversing-an-increase side (Delta>0) onto one log-scaled MR axis obscured
## the mechanism rather than explaining it. Fix: split MR into two separately-
## scaled linear panels, since they are genuinely two different regimes, not
## points on one smooth continuum -- every firm's own clip threshold is
## Delta=-2*lambda*e_i (<=0 always), so for Delta>0 EVERY firm is unclipped
## and shares the same huge intercept-driven slope (~1/(2*lambda)~1.43M),
## while for Delta<0 most firms are still clipped and only the ones with
## small e_i (median firm's own threshold ~-0.00059, essentially at zero)
## have started contributing that huge slope -- hence MR ~750-1400 just
## below zero vs. ~52,000-53,000 just above it. Real mechanism, not a
## sign-mixing bug, but the two sides need separate panels, not one axis.

library(tidyverse)
library(cowplot)

## Revenue level, Delta in [-0.10, 0.02]: negative side from the 0.01-step
## grid (1275), Delta>=0 from the finer 0.001-step onset grid (1276) so the
## sharp transition right at Delta=0 is resolved, not interpolated over.
neg <- read.csv("Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv") %>%
    mutate(Delta = round(Delta, 6)) %>%  # file stores Delta=0 as 1.04083e-17 (grid float noise)
    filter(Delta >= -0.10, Delta < 0)
pos <- read.csv("Code/Products/1276-revenue-onset-finegrid-lag_m.csv") %>%
    filter(Delta >= 0, Delta <= 0.02)
level <- bind_rows(neg, pos) %>% arrange(Delta)

## Cut-side MR: benefit of the next 1% cut, Delta in [-0.10, 0] (0.01 step).
mr_cut <- read.csv("Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv") %>%
    mutate(Delta = round(Delta, 6)) %>%  # file stores Delta=0 as 1.04083e-17 (grid float noise)
    filter(Delta >= -0.10, Delta <= 0) %>%
    arrange(Delta) %>%
    mutate(MR = (lag(mean_R_real) - mean_R_real) / (Delta - lag(Delta))) %>%
    filter(!is.na(MR))

## Increase-side: raw revenue LOST per actual 0.1pp step, Delta in (0, 0.02]
## (0.001 step). Deliberately NOT rescaled to "per unit Delta" like the cut
## side -- that rescaling is only meaningful where the underlying slope is
## roughly constant over a WIDE Delta range (true for the cut side, zone 1
## spans a full 0.75 in Delta), but here the slope is only locally like this
## over a fraction of a percentage point (already down 2% by Delta=0.02) --
## dividing a genuinely small, local ~$53 step-loss by a tiny step (0.001)
## manufactures a huge, not-actually-meaningful "per unit Delta" number.
## User caught this (2026-09-10): "increasing a little after zero increased
## marginal revenue a lot -- that cannot be." Direction was fine (revenue
## FALLS, this is the loss, correctly signed); the per-unit-Delta scaling
## was the misleading part.
mr_inc <- read.csv("Code/Products/1276-revenue-onset-finegrid-lag_m.csv") %>%
    filter(Delta >= 0, Delta <= 0.02) %>%
    arrange(Delta) %>%
    mutate(loss = lag(mean_R_real) - mean_R_real) %>%
    filter(!is.na(loss))

stopifnot(all(mr_cut$MR > 0), all(mr_inc$loss > 0))

wong_orange <- "#E69F00"; wong_blue <- "#0072B2"; wong_grey <- "grey45"

p_level <- ggplot(level, aes(x = Delta, y = mean_R_real)) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.5, linetype = "dashed") +
    geom_line(color = wong_blue, linewidth = 1.1) +
    geom_point(data = filter(level, Delta == 0), color = "grey20", size = 2.6) +
    annotate("text", x = 0.0015, y = max(level$mean_R_real) * 0.95,
             label = "current\npolicy", hjust = 0, family = "Times", size = 3.2,
             color = "grey40", lineheight = 0.85) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(-0.10, 0.02, 0.02)) +
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

p_mr_cut <- ggplot(mr_cut, aes(x = Delta, y = MR)) +
    geom_hline(yintercept = 743, color = wong_grey, linewidth = 0.5, linetype = "dotted") +
    geom_line(color = wong_orange, linewidth = 1.1) +
    geom_point(color = wong_orange, size = 1.8) +
    annotate("text", x = -0.098, y = 850, hjust = 0, vjust = 0,
             label = "floor ≈ $743", family = "Times", size = 3, color = "grey30") +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(-0.10, 0, 0.02)) +
    scale_y_continuous(labels = scales::label_dollar(prefix = "$"), limits = c(0, NA)) +
    labs(x = expression(Delta~"(cut)"), y = "Marginal revenue of\nthe next 1% cut",
         title = "Cutting further (Δ ≤ 0)",
         subtitle = "diminishing returns, ~$743 floor up toward Δ=0\n(per-unit-Δ rate — meaningful here, slope holds over a wide range)") +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(family = "Times", size = 11.5, hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 8, hjust = 0.5, color = "grey40"),
        text = element_text(family = "Times")
    )

## Fair comparison at matched step size: the cut side's own LAST step
## (Delta=-0.01 to 0, raw $59.70 over a 1pp step) rescaled down to the same
## 0.1pp step size as this panel is $5.97 -- so the real, honestly-scaled
## jump right at Delta=0 is ~$53/$5.97 =~ 8.9x, not the misleading "~9x from
## 5,970 to 52,932" figure computed by comparing two different step sizes'
## per-unit-Delta extrapolations.
cutside_last_step_rescaled <- (1760.623256 - 1700.920471) / 10

p_mr_inc <- ggplot(mr_inc, aes(x = Delta, y = loss)) +
    geom_hline(yintercept = cutside_last_step_rescaled, color = wong_grey,
               linewidth = 0.5, linetype = "dotted") +
    annotate("text", x = 0.0155, y = cutside_last_step_rescaled + 1.2, vjust = 0,
             label = "cut side's own last step, same 0.1pp size ≈ $5.97",
             family = "Times", size = 2.8, color = "grey30") +
    geom_line(color = "#0072B2", linewidth = 1.1) +
    geom_point(color = "#0072B2", size = 1.4) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 0.1), breaks = seq(0, 0.02, 0.01)) +
    scale_y_continuous(labels = scales::label_dollar(prefix = "$"), limits = c(0, NA)) +
    labs(x = expression(Delta~"(increase past current policy)"), y = "Revenue LOST per firm-period,\nfor this actual 0.1pp step",
         title = "Past current policy (Δ > 0)",
         subtitle = "raw $ per 0.1pp step, NOT rescaled to \"per unit Δ\"\n(that rescaling is what made this look ~9x bigger than it is)") +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(family = "Times", size = 11.5, hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 8, hjust = 0.5, color = "grey40"),
        text = element_text(family = "Times")
    )

mr_row <- plot_grid(p_mr_cut, p_mr_inc, ncol = 2, align = "h")
combined <- plot_grid(p_level, mr_row, ncol = 1, rel_heights = c(1, 1.1))

ggsave("Paper/images/1279-simulated-laffer-headline.png", combined, width = 10, height = 8.5, dpi = 300)
cat("Saved: Paper/images/1279-simulated-laffer-headline.png\n")
