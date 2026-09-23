## Slide version of the lambda-grid plot (lag_m vs lag_2_cal_W), stripped of
## the TS/chi-sq pass-fail machinery -- just the L_n_hat(lambda) profile with
## each instrument's own grid minimum marked. Companion to 1285 (which shows
## the 95% CI band); this one is for the "$\lambda$ Search" slide in
## 650-stage2-prelim-results.qmd, which only wants "here's the minimum",
## not the formal test (that's introduced later, in "How the Test Works").
## 2026-09-17.

library(tidyverse)

lagm <- read.csv("Code/Products/1270-lambdagrid-lag_m-allcombined.csv") %>%
    filter(lambda <= 8.412e-07) %>%
    transmute(lambda, Lhat, instrument = "lag_m")

lag2w <- read.csv("Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv") %>%
    transmute(lambda, Lhat, instrument = "lag_2_cal_W")

df <- bind_rows(lagm, lag2w) %>%
    mutate(instrument = factor(instrument, levels = c("lag_m", "lag_2_cal_W"),
                                labels = c("Instrument 1 (m*[t-1])", "Instrument 2 (W~[t-2])")))

mins <- df %>% group_by(instrument) %>% slice_min(Lhat, n = 1) %>% ungroup() %>%
    mutate(label = sprintf("lambda-hat = %.3g", lambda))

wong_orange <- "#E69F00"

p <- ggplot(df, aes(x = lambda, y = Lhat)) +
    geom_line(color = "grey60", linewidth = 0.6) +
    geom_point(color = "grey40", size = 2.2) +
    geom_point(data = mins, color = wong_orange, size = 5) +
    geom_text(data = mins, aes(label = label), color = wong_orange,
              family = "Times", fontface = "bold", size = 4, vjust = -1.2) +
    scale_x_log10(labels = scales::label_scientific(),
                  expand = expansion(mult = c(0.08, 0.3))) +
    scale_y_log10() +
    facet_wrap(~instrument) +
    labs(x = expression(lambda~"(detection intensity, log scale)"),
         y = expression(hat(L)[n]~"(fit quality, log scale, lower=better)"),
         title = "How well does the model fit at each detection intensity?",
         subtitle = "Orange = grid minimum") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 10.5, hjust = 0.5, color = "grey30"),
        strip.text = element_text(family = "Times", size = 12, face = "bold"),
        axis.title = element_text(size = 11.5),
        axis.text = element_text(size = 9.5),
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1305-lambdagrid-minonly.png", p, width = 10, height = 5.5, dpi = 300)
cat("Saved: Paper/images/1305-lambdagrid-minonly.png\n")
for (i in seq_len(nrow(mins))) {
    cat(sprintf("%s: lambda=%.4g, Lhat=%.6g\n", mins$instrument[i], mins$lambda[i], mins$Lhat[i]))
}
