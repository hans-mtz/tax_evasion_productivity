## Stage-2 ELVIS: L_n(lambda) vs lambda, no-eta mechanism, moment set A (9
## rows), coarse 8-point grid within the new [1.63e-8,0.0161] bounds. Marks
## which point(s) pass the soft (min-subtracted CHT-style) test actually
## used for the confidence set, and separately the hard (conservative,
## Theorem F.1, absolute) test. 2026-09-10.

library(tidyverse)

df <- read.csv("Code/Products/1268-lambdagrid-noeta-lag_m-combined.csv")

n <- 32232; dg <- 9
qc <- qchisq(0.95, dg)
Lmin <- min(df$Lhat)
df <- df %>%
    mutate(
        TS_soft = 2*n*(Lhat - Lmin),
        TS_hard = 2*n*Lhat,
        pass_soft = TS_soft <= qc,
        pass_hard = TS_hard <= qc,
        cat = case_when(
            pass_hard ~ "Passes hard (conservative, Thm F.1)",
            pass_soft ~ "Passes soft (min-subtracted CHT) only",
            TRUE      ~ "Fails both"
        )
    )

wong_red <- "#D55E00"; wong_orange <- "#E69F00"; wong_blue <- "#0072B2"

p <- ggplot(df, aes(x = lambda, y = Lhat)) +
    geom_hline(yintercept = Lmin + qc/(2*n), color = wong_orange, linewidth = 0.5, linetype = "dashed") +
    geom_hline(yintercept = qc/(2*n), color = wong_red, linewidth = 0.5, linetype = "dotted") +
    geom_line(color = "grey50", linewidth = 0.6) +
    geom_point(aes(color = cat, shape = cat), size = 3.5) +
    geom_text(aes(label = sprintf("TS_soft=%.0f\nTS_hard=%.0f", TS_soft, TS_hard)),
              vjust = -0.6, size = 2.6, family = "Times", lineheight = 0.85) +
    scale_color_manual(values = c(
        "Passes hard (conservative, Thm F.1)"   = wong_red,
        "Passes soft (min-subtracted CHT) only" = wong_orange,
        "Fails both"                            = wong_blue
    )) +
    scale_shape_manual(values = c(
        "Passes hard (conservative, Thm F.1)"   = 17,
        "Passes soft (min-subtracted CHT) only" = 16,
        "Fails both"                            = 4
    )) +
    scale_x_log10(labels = scales::label_scientific()) +
    scale_y_log10() +
    labs(x = expression(lambda~"(log scale)"), y = expression(hat(L)[n]~"(log scale)"),
         color = NULL, shape = NULL,
         title = "Stage-2 ELVIS: L_n(lambda), lag_m, no eta, moment set A (9 rows)",
         subtitle = "Coarse 8-pt grid, free (delta0,delta1,delta2,gamma). Dashed=soft threshold, dotted=hard threshold.") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 8.5, hjust = 0.5),
        legend.position = "bottom",
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1269-lambdagrid-noeta-lhat-lambda.png", p, width = 9.5, height = 7, dpi = 300)
cat("Saved: Paper/images/1269-lambdagrid-noeta-lhat-lambda.png\n")
