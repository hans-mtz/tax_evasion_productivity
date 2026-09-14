## Compare L_n(lambda) vs lambda across instruments: lag_m (clean contiguous CI)
## vs lag_2_cal_W (coarse grid, two disjoint passing points, floor pass not
## economically meaningful). Two-panel, shared log-log axes, own soft-CI
## threshold line per instrument (each has its own Lmin). 2026-09-10.

library(tidyverse)

n <- 32232; dg <- 9
qc <- qchisq(0.95, dg)

## lag_m's own grid tested 5 higher lambda values (6.05e-6 to 0.0161) that
## lag_2_cal_W's grid never covered -- all decisively rejected, not
## informative, and they force a much wider x-axis than the interesting
## region needs. Dropped here so both panels share the same x-axis range
## (2026-09-11, for the slide version of this plot).
lagm <- read.csv("Code/Products/1270-lambdagrid-lag_m-allcombined.csv") %>%
    filter(lambda <= 8.412e-07) %>%
    transmute(lambda, delta1_hat, delta2_hat, Lhat,
              instrument = "lag_m")

lag2w <- read.csv("Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv") %>%
    transmute(lambda, delta1_hat, delta2_hat, Lhat,
              instrument = "lag_2_cal_W")

df <- bind_rows(lagm, lag2w) %>%
    group_by(instrument) %>%
    mutate(
        Lmin = min(Lhat),
        TS_soft = 2*n*(Lhat - Lmin),
        pass_soft = TS_soft <= qc,
        cat = if_else(pass_soft, "Passes (95% CI)", "Rejected")
    ) %>%
    ungroup() %>%
    mutate(
        instrument = factor(instrument, levels = c("lag_m", "lag_2_cal_W"),
                             labels = c("lag_m  (m*[t-1])", "lag_2_cal_W  (W~[t-2])")),
        is_floor = instrument == "lag_2_cal_W  (W~[t-2])" & lambda == min(lambda)
    )

hlines <- df %>% group_by(instrument) %>% summarise(y = min(Lhat) + qc/(2*n), .groups = "drop")

wong_orange <- "#E69F00"; wong_blue <- "#0072B2"; wong_grey <- "grey40"

p <- ggplot(df, aes(x = lambda, y = Lhat)) +
    geom_hline(data = hlines, aes(yintercept = y), color = wong_orange,
               linewidth = 0.6, linetype = "dashed") +
    geom_line(color = "grey60", linewidth = 0.6) +
    geom_point(aes(color = cat, shape = is_floor), size = 4) +
    geom_text(data = filter(df, is_floor), label = "floor\n(uninformative)",
              vjust = -0.9, size = 3.1, color = "grey30", lineheight = 0.85) +
    scale_color_manual(values = c("Passes (95% CI)" = wong_orange, "Rejected" = wong_blue)) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
    scale_x_log10(labels = scales::label_scientific()) +
    scale_y_log10() +
    facet_wrap(~instrument, scales = "free_y") +
    labs(x = expression(lambda~"(log scale)"), y = expression(hat(L)[n]~"(log scale)"),
         color = NULL,
         title = "Stage-2 ELVIS: L_n(lambda) by instrument",
         subtitle = "Dashed line = each instrument's own soft (min-subtracted CHT) 95% threshold") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 10.5, hjust = 0.5, color = "grey30"),
        strip.text = element_text(family = "Times", size = 12, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 9.5),
        text = element_text(family = "Times")
    )

ggsave("Paper/images/1278-lambdagrid-lagm-vs-lag2W.png", p, width = 11, height = 6.5, dpi = 300)
cat("Saved: Paper/images/1278-lambdagrid-lagm-vs-lag2W.png\n")

## Delta comparison at each instrument's own passing point(s), vs. lag_m's best fit
cat("\n--- delta1/delta2 at passing points ---\n")
df %>% filter(pass_soft) %>%
    arrange(instrument, lambda) %>%
    select(instrument, lambda, delta1_hat, delta2_hat, Lhat, is_floor) %>%
    print(digits = 4)
