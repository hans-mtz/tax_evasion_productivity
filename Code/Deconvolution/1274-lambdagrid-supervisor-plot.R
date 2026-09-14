## Stage-2 ELVIS: L_n(lambda) vs lambda -- supervisor-clean version of 1272.
## Same data, no per-point TS labels, uniform circular points (no X's for
## failing points, color alone distinguishes pass/fail), test threshold
## lines (soft dashed, hard dotted) kept, connecting trend line dropped.
## 2026-09-10.

library(tidyverse)

df <- read.csv("Code/Products/1270-lambdagrid-lag_m-allcombined.csv")

n <- 32232; dg <- 9
qc <- qchisq(0.95, dg)
Lmin <- min(df$Lhat)
df <- df %>%
    mutate(
        TS_soft = 2*n*(Lhat - Lmin),
        pass_soft = TS_soft <= qc,
        cat = if_else(pass_soft, "Passes (95% CI)", "Rejected")
    )

wong_orange <- "#E69F00"; wong_red <- "#D55E00"; wong_blue <- "#0072B2"

p <- ggplot(df, aes(x = lambda, y = Lhat)) +
    geom_hline(yintercept = Lmin + qc/(2*n), color = wong_orange, linewidth = 0.6, linetype = "dashed") +
    geom_hline(yintercept = qc/(2*n), color = wong_red, linewidth = 0.6, linetype = "dotted") +
    geom_line(color = "grey50", linewidth = 0.6) +
    geom_point(aes(color = cat), size = 4.2) +
    scale_color_manual(values = c(
        "Passes (95% CI)" = wong_orange,
        "Rejected"        = wong_blue
    )) +
    scale_x_log10(labels = scales::label_scientific()) +
    scale_y_log10() +
    labs(x = expression(lambda~"(log scale)"), y = expression(hat(L)[n]~"(log scale)"),
         color = NULL,
         title = "Stage-2 ELVIS: L_n(lambda), lag_m") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1274-lambdagrid-supervisor-lhat-lambda.png", p, width = 9, height = 6.5, dpi = 300)
cat("Saved: Paper/images/1274-lambdagrid-supervisor-lhat-lambda.png\n")
