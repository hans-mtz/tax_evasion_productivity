## CV-adjusted R (Delta,R) grid: same AK2020-style standard plot as 1296, but
## using the HARD (absolute Theorem F.1, TS_hard=2n*Lhat) test instead of the
## SOFT (min-subtracted CHT, TS_soft=2n*(Lhat-Lhat_min)) test -- both compared
## against the SAME chi2(10,.95) threshold (the standard convention used
## throughout this project's lambdagrid/revgrid scripts: hard and soft differ
## in whether Lhat is min-subtracted, not in the significance level). Reuses
## 1296's already-computed coarse+fine CV data, no new estimation. 2026-09-13.

suppressMessages(library(tidyverse))

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)
wong_blue <- "#0072B2"; gray_fail <- "grey75"

delta_pct <- function(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 2), "%")

coarse <- read.csv("Code/Products/1294-coarse-cv-combined.csv")
fine <- read.csv("Code/Products/1294-fine-cv-combined.csv")
df <- bind_rows(coarse, fine) %>%
    mutate(R_round = round(R, 4)) %>% distinct(Delta, R_round, .keep_all = TRUE) %>% select(-R_round)

Lmin <- min(df$Lhat)
cat(sprintf("Global min Lhat = %.6g (at Delta=%.2f, R=%.2f) -> min-subtraction shift = %.3f\n",
            Lmin, df$Delta[which.min(df$Lhat)], df$R[which.min(df$Lhat)], 2 * n * Lmin))
cat(sprintf("chi2(10,.95) = %.3f\n\n", qc))

make_plot <- function(test_type) {
    if (test_type == "soft") {
        dfx <- df %>% mutate(TS = 2 * n * (Lhat - Lmin))
        test_label <- "SOFT (min-subtracted CHT)"
        out_png <- "Paper/images/1297-cv-soft-test.png"
    } else {
        dfx <- df %>% mutate(TS = 2 * n * Lhat)
        test_label <- "HARD (absolute, Theorem F.1)"
        out_png <- "Paper/images/1297-cv-hard-test.png"
    }
    dfx <- dfx %>% mutate(pass = TS <= qc)

    bounds <- dfx %>% group_by(Delta) %>%
        summarise(r_min_tested = min(R), r_max_tested = max(R),
                  lower = suppressWarnings(min(R[pass])), upper = suppressWarnings(max(R[pass])),
                  n_pass = sum(pass), .groups = "drop") %>%
        mutate(open_low = is.finite(lower) & lower <= r_min_tested + 1e-6,
               open_high = is.finite(upper) & upper >= r_max_tested - 1e-6,
               all_reject = n_pass == 0)

    band <- bounds %>% filter(is.finite(lower)) %>% select(Delta, lower, upper) %>%
        pivot_longer(c(lower, upper), names_to = "bound", values_to = "R") %>%
        mutate(bound = recode(bound, lower = "Lower bound", upper = "Upper bound"))

    delta_levels <- sort(unique(dfx$Delta))
    dfx <- dfx %>% mutate(pass_lab = ifelse(pass, "Passes (95% test)", "Rejected"),
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
        geom_point(data = dfx, aes(x = Delta_f, y = R, color = pass_lab, shape = pass_lab),
                   size = 2.6, alpha = 0.9) +
        { if (nrow(open_rays) > 0) geom_point(data = open_rays, aes(x = Delta_f, y = R),
                   shape = 17, color = wong_blue, size = 2.6, alpha = 0.7) } +
        scale_color_manual(values = c("Passes (95% test)" = wong_blue, "Rejected" = gray_fail), name = NULL) +
        scale_shape_manual(values = c("Passes (95% test)" = 16, "Rejected" = 4), name = NULL) +
        scale_linetype_manual(values = c("Upper bound" = "twodash", "Lower bound" = "dotted"), name = NULL) +
        guides(color = guide_legend(order = 1), shape = guide_legend(order = 1),
               linetype = guide_legend(order = 2, override.aes = list(color = wong_blue))) +
        labs(x = expression(paste("Purchases-tax shifter ", Delta)), y = "Real mean revenue per firm-period, R (COP)",
             title = paste0("CV-adjusted R, ", test_label, " test"),
             subtitle = paste0("Blue = passes at 95% (χ²₁₀=", round(qc, 1), "); gray × = rejected.")) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(), axis.line = element_line(color = "black"),
              plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(family = "Times", size = 9.5, hjust = 0.5, color = "grey30"),
              legend.position = "bottom", legend.box = "vertical", text = element_text(family = "Times"))

    ggsave(out_png, p, width = 10, height = 7.5, dpi = 300)
    cat(sprintf("Saved: %s\n", out_png))
    bounds
}

cat("=== SOFT test bounds ===\n")
soft_bounds <- make_plot("soft")
print(soft_bounds %>% select(Delta, lower, upper, n_pass, open_low, open_high))

cat("\n=== HARD test bounds ===\n")
hard_bounds <- make_plot("hard")
print(hard_bounds %>% select(Delta, lower, upper, n_pass, open_low, open_high, all_reject))
