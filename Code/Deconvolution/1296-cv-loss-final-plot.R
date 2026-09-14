## Final combine + standard AK2020-style CI plot (per 1293's template) for
## the CV-adjusted-R and Loss (Delta,target) grids: combines each moment
## type's coarse+fine points, computes the global-min-subtracted TS_soft
## (same convention as 1263/1290/1291), reports the passing-region bounds
## per Delta, and produces one plot per moment type. 2026-09-12.

suppressMessages(library(tidyverse))

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)
wong_blue <- "#0072B2"; gray_fail <- "grey75"

delta_pct <- function(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 2), "%")

analyze_and_plot <- function(coarse_path, fine_path, label, out_png, y_label) {
    coarse <- read.csv(coarse_path)
    fine <- if (file.exists(fine_path)) read.csv(fine_path) else coarse[0, ]
    df <- bind_rows(coarse, fine) %>%
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
        labs(x = expression(paste("Purchases-tax shifter ", Delta)), y = y_label,
             title = label,
             subtitle = paste0("Blue = passes soft (min-subtracted CHT) test at 95% (χ²₁₀=", round(qc, 1), "); gray × = rejected.")) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(), axis.line = element_line(color = "black"),
              plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(family = "Times", size = 9.5, hjust = 0.5, color = "grey30"),
              legend.position = "bottom", legend.box = "vertical", text = element_text(family = "Times"))

    ggsave(out_png, p, width = 10, height = 7.5, dpi = 300)
    cat(sprintf("\nSaved: %s\n", out_png))
    cat(sprintf("\n=== %s: per-Delta bounds ===\n", label))
    print(bounds %>% select(Delta, lower, upper, open_low, open_high))
    bounds
}

cv_bounds <- analyze_and_plot(
    "Code/Products/1294-coarse-cv-combined.csv", "Code/Products/1294-fine-cv-combined.csv",
    "CV-adjusted R: candidate (Δ, R) pairs, fixed θ", "Paper/images/1296-cv-standard-ci.png",
    "Real mean revenue per firm-period, R (COP)"
)

loss_bounds <- analyze_and_plot(
    "Code/Products/1294-coarse-loss-combined.csv", "Code/Products/1294-fine-loss-combined.csv",
    "Revenue Loss (uncaught evasion): candidate (Δ, Loss) pairs, fixed θ", "Paper/images/1296-loss-standard-ci.png",
    "Real mean revenue loss per firm-period (COP)"
)

write.csv(cv_bounds, "Code/Products/1296-cv-bounds-final.csv", row.names = FALSE)
write.csv(loss_bounds, "Code/Products/1296-loss-bounds-final.csv", row.names = FALSE)
cat("\nDone.\n")
