## Fixed-theta (Delta,R) wide revgrid results, both soft and hard tests shown
## together: one panel per Delta, R on x-axis, TS on y-axis, two curves
## (soft=min-subtracted CHT, hard=absolute Theorem F.1) each with its own
## threshold line. Shows directly how wide/overlapping the CIs are across
## adjacent Deltas. 2026-09-12.

library(tidyverse)

df <- read.csv("Code/Products/1291-revgrid-fixedtheta-wide-lag_m-analyzed.csv")

n <- 32232; dg <- 10
qc95 <- qchisq(0.95, dg); qc99 <- qchisq(0.99, dg)

long <- df %>%
    select(Delta, R, TS_soft, TS_hard) %>%
    pivot_longer(c(TS_soft, TS_hard), names_to = "test", values_to = "TS") %>%
    mutate(test = recode(test, TS_soft = "Soft (min-subtracted)", TS_hard = "Hard (absolute)"))

thr <- tibble(test = c("Soft (min-subtracted)", "Hard (absolute)"), thresh = c(qc95, qc99))

wong_orange <- "#E69F00"; wong_blue <- "#0072B2"

delta_labs <- c(`-0.04` = "Δ = -4%", `-0.03` = "Δ = -3%", `-0.02` = "Δ = -2%",
                `-0.01` = "Δ = -1%", `0` = "Δ = 0%", `0.001` = "Δ = +0.1%",
                `0.01` = "Δ = +1%")

p <- ggplot(long, aes(x = R, y = TS, color = test)) +
    geom_hline(data = thr, aes(yintercept = thresh, color = test), linetype = "dashed", linewidth = 0.6) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    scale_color_manual(values = c("Soft (min-subtracted)" = wong_orange, "Hard (absolute)" = wong_blue), name = NULL) +
    facet_wrap(~Delta, labeller = as_labeller(delta_labs), nrow = 2) +
    labs(x = "Candidate R (real terms, mean per firm-period)", y = "Test statistic (TS)",
         title = "Fixed-θ (Δ,R) test inversion: soft vs. hard test, all 7 Δ's",
         subtitle = "Dashed lines = each test's own 95%/99% threshold (χ²₁₀). Points below the line pass.") +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(family = "Times", size = 13.5, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 10, hjust = 0.5, color = "grey30"),
        strip.text = element_text(family = "Times", size = 11, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1292-revgrid-wide-soft-hard.png", p, width = 12, height = 7, dpi = 300)
cat("Saved: Paper/images/1292-revgrid-wide-soft-hard.png\n")
