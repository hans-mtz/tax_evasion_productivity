## Stage-2 ELVIS: Lhat vs lambda, colored by seeding strategy, faceted by
## instrument -- moment set A (9 moments), trim=0.5% (2026-09-08). Same style
## as Paper/images/1212-stage2-A-lhat-lambda.png (log-x, line+points, Wong
## palette, Times), but color now carries seeding strategy (naive chain vs.
## lag_m-seeded) since 1212's color slot (instrument) becomes the facet here.

library(tidyverse)

all_fits <- read.csv("Code/Products/1237-stage2-estimates-so-far.csv")

INS_LABELS <- c(lag_m = "m*(t−1)", lag_2_cal_W = "\U0001D4B2(t−2)")
STRAT_LABELS <- c(naive_chain = "Naive chain (own GMM warmstart)",
                   lag_m_seed = "lag_m-seeded (independent, per point)")
wong_cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536", "#0072B2", "#D55E00", "#CC79A7")

plot_df <- all_fits %>%
    mutate(panel = INS_LABELS[ins], strategy = STRAT_LABELS[seed_strategy]) %>%
    arrange(ins, seed_strategy, lambda)

p <- ggplot(plot_df, aes(x = lambda, y = Lhat, color = strategy)) +
    geom_line(linewidth = 0.6, alpha = 0.85) +
    geom_point(size = 2.4, alpha = 0.9) +
    scale_color_manual(values = c("Naive chain (own GMM warmstart)" = wong_cb_palette[7],
                                   "lag_m-seeded (independent, per point)" = wong_cb_palette[6])) +
    facet_wrap(~panel, nrow = 1) +
    scale_x_log10(labels = scales::label_scientific()) +
    labs(x = expression(lambda~"(log scale)"), y = expression(hat(L)[n]~"(CUE objective)"), color = NULL,
         title = "Stage-2 ELVIS: CUE objective across the lambda grid, by seeding strategy",
         subtitle = "Moment set A (9 rows), n_burn=500, n_keep=1000, corner_mode=include_zero, trim=0.5%") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        strip.text = element_text(family = "Times", size = 12, face = "bold"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 9, hjust = 0.5),
        legend.position = "bottom"
    )

ggsave("Paper/images/1238-stage2-A9-lhat-lambda-by-seedstrategy.png", plot = p, width = 12, height = 6.5, units = "in", dpi = 300)
cat("Saved: Paper/images/1238-stage2-A9-lhat-lambda-by-seedstrategy.png\n")
