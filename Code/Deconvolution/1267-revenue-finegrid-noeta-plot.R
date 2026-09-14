## Forward-simulated (no optimization) baseline revenue across a fine Delta
## grid (-0.5 to 0.5, step 0.01), at the fixed Phase-0 operating point --
## used to pick a sensible Delta range for the actual (Delta,R) joint grid,
## given the closed-form e'(Delta) blows up fast at this lambda. 2026-09-10.

library(tidyverse)

df <- read.csv("Code/Products/1266-revenue-baseline-finegrid-noeta-lag_m.csv")

wong_red <- "#D55E00"; wong_blue <- "#0072B2"

p <- ggplot(df, aes(x = Delta, y = mean_R_real)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "grey50", linewidth = 0.4, linetype = "dashed") +
    geom_line(color = wong_blue, linewidth = 0.9) +
    labs(x = expression(Delta~"(change in purchases-side tax rate)"),
         y = "Mean R per firm-period (real, deflated)",
         title = "Forward-simulated baseline revenue vs. tax change, lag_m operating point",
         subtitle = "No optimization -- fixed (lambda,delta0,delta1,delta2,eta,gamma) at the Phase-0 point. lambda=2.2e-7 (no eta).") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 9, hjust = 0.5),
        text = element_text(family = "Times")
    )
ggsave("Paper/images/1267-revenue-finegrid-noeta-full.png", p, width = 9, height = 6, dpi = 300)

## Zoomed version around where it actually stays sane (-0.5 to 0.05)
p2 <- p + coord_cartesian(xlim = c(-0.5, 0.05), ylim = c(-3000, 2500)) +
    labs(title = "Same, zoomed to the region that doesn't collapse")
ggsave("Paper/images/1267-revenue-finegrid-noeta-zoom.png", p2, width = 9, height = 6, dpi = 300)

cat("Saved: Paper/images/1267-revenue-finegrid-noeta-full.png\n")
cat("Saved: Paper/images/1267-revenue-finegrid-noeta-zoom.png\n")
