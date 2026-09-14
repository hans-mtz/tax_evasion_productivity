## Slide version of the lag_m vs lag_2_cal_W lambda-grid comparison (1278),
## for a non-specialist committee audience: same data, shared x-axis range
## (lag_m's 5 higher, already-rejected lambda values dropped so both panels
## cover the same window), but plain labels -- no "CHT"/"soft test" jargon,
## just "95% confidence region". 2026-09-11.

library(tidyverse)

n <- 32232; dg <- 9
qc <- qchisq(0.95, dg)

lagm <- read.csv("Code/Products/1270-lambdagrid-lag_m-allcombined.csv") %>%
    filter(lambda <= 8.412e-07) %>%
    transmute(lambda, Lhat, instrument = "lag_m")

lag2w <- read.csv("Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv") %>%
    transmute(lambda, Lhat, instrument = "lag_2_cal_W")

df <- bind_rows(lagm, lag2w) %>%
    group_by(instrument) %>%
    mutate(
        Lmin = min(Lhat),
        TS = 2*n*(Lhat - Lmin),
        pass = TS <= qc,
        cat = if_else(pass, "Not rejected", "Rejected")
    ) %>%
    ungroup() %>%
    mutate(
        instrument = factor(instrument, levels = c("lag_m", "lag_2_cal_W"),
                             labels = c("Instrument 1 (m*[t-1])", "Instrument 2 (W~[t-2])")),
        is_floor = instrument == "Instrument 2 (W~[t-2])" & lambda == min(lambda)
    )

hlines <- df %>% group_by(instrument) %>% summarise(y = min(Lhat) + qc/(2*n), .groups = "drop")

wong_orange <- "#E69F00"; wong_blue <- "#0072B2"

p <- ggplot(df, aes(x = lambda, y = Lhat)) +
    geom_hline(data = hlines, aes(yintercept = y), color = wong_orange,
               linewidth = 0.6, linetype = "dashed") +
    geom_line(color = "grey60", linewidth = 0.6) +
    geom_point(aes(color = cat), size = 4) +
    scale_color_manual(values = c("Not rejected" = wong_orange, "Rejected" = wong_blue)) +
    scale_x_log10(labels = scales::label_scientific()) +
    scale_y_log10() +
    facet_wrap(~instrument) +
    labs(x = expression(lambda~"(detection intensity, log scale)"),
         y = expression(hat(L)[n]~"(fit quality, log scale, lower=better)"),
         color = NULL,
         title = "How well does the model fit at each detection intensity?",
         subtitle = "Dashed line = 95% confidence region cutoff") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 10.5, hjust = 0.5, color = "grey30"),
        strip.text = element_text(family = "Times", size = 12, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 11.5),
        axis.text = element_text(size = 9.5),
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1285-lambdagrid-slide.png", p, width = 10, height = 5.5, dpi = 300)
cat("Saved: Paper/images/1285-lambdagrid-slide.png\n")
