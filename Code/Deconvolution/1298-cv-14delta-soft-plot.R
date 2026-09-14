## Updated standard AK2020-style CI plot (soft/min-subtracted CHT test) for
## the CV-adjusted-R grid, now extended from 11 to 14 Delta points with the
## addition of -7%, -6%, +0.5%. Global min Lhat recomputed over all 14
## Deltas (unchanged from the original 11-Delta minimum, confirming
## consistency). 2026-09-13.

suppressMessages(library(tidyverse))

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)
wong_blue <- "#0072B2"; gray_fail <- "grey75"
delta_pct <- function(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 2), "%")

df <- read.csv("Code/Products/1298-cv-all14-combined.csv") %>%
    mutate(R_round = round(R, 4)) %>% distinct(Delta, R_round, .keep_all = TRUE) %>% select(-R_round)

Lmin <- min(df$Lhat)
df <- df %>% mutate(TS = 2 * n * (Lhat - Lmin), pass = TS <= qc)

bounds <- df %>% group_by(Delta) %>%
    summarise(r_min_tested = min(R), r_max_tested = max(R),
              lower = suppressWarnings(min(R[pass])), upper = suppressWarnings(max(R[pass])),
              .groups = "drop") %>%
    mutate(open_low = is.finite(lower) & lower <= r_min_tested + 1e-6,
           open_high = is.finite(upper) & upper >= r_max_tested - 1e-6)

band <- bounds %>% filter(is.finite(lower)) %>% select(Delta, lower, upper) %>%
    pivot_longer(c(lower, upper), names_to = "bound", values_to = "R") %>%
    mutate(bound = recode(bound, lower = "Lower bound", upper = "Upper bound"))

delta_levels <- sort(unique(df$Delta))
df <- df %>% mutate(pass_lab = ifelse(pass, "Passes (95% test)", "Rejected"),
                     Delta_f = factor(Delta, levels = delta_levels, labels = delta_pct(delta_levels)))
band <- band %>% mutate(Delta_f = factor(Delta, levels = delta_levels, labels = delta_pct(delta_levels)))

open_side_rows <- bounds %>% filter(open_low | open_high)
open_rays <- if (nrow(open_side_rows) == 0) {
    tibble(Delta = numeric(0), R = numeric(0), Delta_f = factor())
} else {
    open_side_rows %>% rowwise() %>%
    mutate(ray = list(tibble(R = c(
        if (open_low) lower - 0.12 * (r_max_tested - r_min_tested) else NA,
        if (open_high) upper + 0.12 * (r_max_tested - r_min_tested) else NA
    )))) %>% ungroup() %>% select(Delta, ray) %>% unnest(ray) %>%
    mutate(Delta_f = factor(Delta, levels = delta_levels, labels = delta_pct(delta_levels))) %>%
    filter(!is.na(R))
}

p <- ggplot() +
    geom_line(data = band, aes(x = Delta_f, y = R, group = bound, linetype = bound),
              color = wong_blue, linewidth = 0.8) +
    geom_point(data = df, aes(x = Delta_f, y = R, color = pass_lab, shape = pass_lab),
               size = 2.6, alpha = 0.9) +
    { if (nrow(open_rays) > 0) geom_point(data = open_rays, aes(x = Delta_f, y = R),
               shape = 17, color = wong_blue, size = 2.6, alpha = 0.7) } +
    scale_color_manual(values = c("Passes (95% test)" = wong_blue, "Rejected" = gray_fail), name = NULL) +
    scale_shape_manual(values = c("Passes (95% test)" = 16, "Rejected" = 4), name = NULL) +
    scale_linetype_manual(values = c("Upper bound" = "twodash", "Lower bound" = "dotted"), name = NULL) +
    guides(color = guide_legend(order = 1), shape = guide_legend(order = 1),
           linetype = guide_legend(order = 2, override.aes = list(color = wong_blue))) +
    labs(x = expression(paste("Purchases-tax shifter ", Delta)), y = "Real mean revenue per firm-period, R (COP)",
         title = "CV-adjusted R, SOFT test — extended to 14 Δ points",
         subtitle = paste0("Blue = passes soft (min-subtracted CHT) test at 95% (χ²₁₀=", round(qc, 1),
                            "); gray × = rejected. Adds −7%, −6%, +0.5% to the original grid."),
         caption = "−7%'s upper bound is OPEN (triangle) -- widest tested candidate still passes; not a closed CI on that side yet.") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), axis.line = element_line(color = "black"),
          plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(family = "Times", size = 9.5, hjust = 0.5, color = "grey30"),
          plot.caption = element_text(family = "Times", size = 8, color = "grey40", hjust = 0),
          legend.position = "bottom", legend.box = "vertical", text = element_text(family = "Times"))

ggsave("Paper/images/1298-cv-14delta-soft.png", p, width = 11, height = 7.5, dpi = 300)
cat("Saved: Paper/images/1298-cv-14delta-soft.png\n")
print(bounds %>% select(Delta, lower, upper, open_low, open_high))
