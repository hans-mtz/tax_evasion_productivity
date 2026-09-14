## Heatmap of the 25-point (delta1,delta2) deltagrid (lambda free), lag_m,
## no-eta mechanism -- built to check whether +-1/+-0.2 spacing is wide
## enough for the eventual 3D cube. 2026-09-11.

library(tidyverse)

df <- read.csv("Code/Products/1283-deltagrid-lag_m-combined.csv")
n <- 32232; dg <- 9
qc95 <- qchisq(0.95, dg)
Lmin <- min(df$Lhat)
df <- df %>% mutate(
    TS_soft = 2*n*(Lhat - Lmin),
    pass = TS_soft <= qc95,
    TS_capped = pmin(TS_soft, 150)   # cap for color scale legibility
)

wong_orange <- "#E69F00"

p <- ggplot(df, aes(x = factor(delta1), y = factor(delta2))) +
    geom_tile(aes(fill = TS_capped), color = "white", linewidth = 1.2) +
    geom_text(aes(label = sprintf("%.0f", TS_soft)), family = "Times", size = 3.6,
              color = ifelse(df$TS_capped > 90, "white", "black")) +
    geom_tile(data = filter(df, pass), fill = NA, color = wong_orange, linewidth = 1.6) +
    scale_fill_viridis_c(name = "TS (soft test)\n(capped at 150)", option = "rocket", direction = -1) +
    labs(x = expression(delta[1]), y = expression(delta[2]),
         title = "(δ1, δ2) grid at the best-fit lambda region -- lag_m, lambda free",
         subtitle = "Numbers = TS_soft = 2n(Lhat-Lmin); orange outline = passes 95% (threshold 16.9); only cross-neighbors tested so far") +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid = element_blank(),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 9.5, hjust = 0.5, color = "grey30"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1286-deltagrid-heatmap-lag_m.png", p, width = 8.5, height = 7, dpi = 300)
cat("Saved: Paper/images/1286-deltagrid-heatmap-lag_m.png\n")
