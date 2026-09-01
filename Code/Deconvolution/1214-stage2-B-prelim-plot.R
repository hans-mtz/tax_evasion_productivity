## Stage-2 ELVIS, moment set B -- preliminary-results figure + table numbers --
## for Quarto-Slides/sections/650-stage2-prelim-results.qmd (2026-09-01).
##
## Reuses the fits from the 2026-09-01 overnight run
## (1211-stage2-elvis-driver-AB.R, n_burn=500, n_keep=1000, maxeval_b=5000,
## xtol_rel_b=1e-4, maxtime_b=600 -- loosened from the smoke-test defaults
## specifically because every point hit the maxeval=1000 cap at the old
## settings; see CLAUDE.md's 2026-09-01 entries) -- no new estimation here,
## just plotting + summary numbers, mirroring 1212's structure for moment
## set A.

library(tidyverse)

RESULT_FILES <- c(
    lag_m       = "Code/Products/1211-stage2-elvis-AB-lag_m-B-include_zero-nburn500-nkeep1000-maxevalb5000.RData",
    lag_2_cal_W = "Code/Products/1211-stage2-elvis-AB-lag_2_cal_W-B-include_zero-nburn500-nkeep1000-maxevalb5000.RData"
)

extract_fits <- function(path, ins_label) {
    e <- new.env(); load(path, envir = e); fits <- e$fits
    do.call(rbind, lapply(fits, function(f) data.frame(
        ins = ins_label, phase = f$phase, lambda = f$lambda, Lhat = f$value,
        n_used = f$n, p_boundary = f$p_boundary, conv = f$convergence, iter = f$iter,
        max_abs_gamma = f$max_abs_gamma,
        delta0 = f$par[1], delta1 = f$par[2], delta2 = f$par[3], eta = f$par[4]
    )))
}
df <- do.call(rbind, Map(extract_fits, RESULT_FILES, names(RESULT_FILES)))
df$omega_star <- df$delta1 / (2 * df$delta2)
df$converged  <- df$conv == 1   # conv=6 is NLOPT_MAXTIME_REACHED, not real convergence

best <- do.call(rbind, lapply(split(df, df$ins), function(s) s[which.min(s$Lhat), ]))

cat("---- Best fit per ins (moment set B) ----\n")
print(best[, c("ins", "phase", "lambda", "Lhat", "conv", "iter", "delta0", "delta1", "delta2", "omega_star", "max_abs_gamma")], digits = 5)
cat("\n---- Convergence summary (conv=1 real, conv=6 = maxtime cap) ----\n")
print(table(df$ins, df$conv))

## %% Figure -----------------------------------------------------------------
## Non-converged points (conv=6) marked with a hollow/open marker so a
## reviewer can see at a glance which points to trust less -- only one such
## point in this run (lag_2_cal_W, a non-final quantile-phase point).
wong_cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536", "#0072B2", "#D55E00", "#CC79A7")
ins_colors <- c(lag_m = wong_cb_palette[6], lag_2_cal_W = wong_cb_palette[7])

df$phase_lbl <- ifelse(df$phase == "quantile", "Quantile grid", "Linear refinement")
df$ins_lbl   <- factor(df$ins, levels = c("lag_m", "lag_2_cal_W"), labels = c("lag_m", "lag_2_cal_W"))
df$shape_lbl <- ifelse(df$converged, "Converged (xtol_rel)", "Maxtime cap (not converged)")

## Two-panel layout via facet_wrap on a duplicated-data trick (no patchwork/
## gridExtra installed) -- lag_2_cal_W's range (0.001 to 1.8) flattens the
## refined region into the x-axis on a single panel, so the full-range panel
## and a zoomed-to-refinement panel are shown side by side, free y scales.
df_full <- df %>% mutate(panel = "Full range (both phases)")
df_zoom <- df %>% filter(phase == "linear_refine") %>% mutate(panel = "Refined region (zoom)")
df2 <- bind_rows(df_full, df_zoom)
df2$panel <- factor(df2$panel, levels = c("Full range (both phases)", "Refined region (zoom)"))

p <- df2 %>%
    arrange(ins, lambda) %>%
    ggplot(aes(x = lambda, y = Lhat, color = ins_lbl, linetype = phase_lbl, group = interaction(ins, phase))) +
    geom_line(linewidth = 0.7) +
    geom_point(aes(shape = shape_lbl), size = 2.2) +
    facet_wrap(~panel, scales = "free", nrow = 1) +
    scale_x_log10(labels = scales::label_scientific()) +
    scale_color_manual(values = ins_colors, name = "Instrument") +
    scale_linetype_manual(values = c("Quantile grid" = "22", "Linear refinement" = "solid"), name = "Phase") +
    scale_shape_manual(values = c("Converged (xtol_rel)" = 16, "Maxtime cap (not converged)" = 1), name = "Status") +
    labs(
        x = expression(lambda~"(log scale)"),
        y = expression(hat(L)[n]~"(CUE objective)"),
        title = "Stage-2 ELVIS: CUE objective across the lambda grid",
        subtitle = "Moment set B, n_burn=500, n_keep=1000, corner_mode=include_zero"
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        axis.text = element_text(family = "Times", size = 10),
        axis.title = element_text(family = "Times", size = 12, face = "bold"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 11, hjust = 0.5),
        strip.text = element_text(family = "Times", size = 11, face = "bold"),
        legend.text = element_text(family = "Times", size = 10),
        legend.title = element_text(family = "Times", size = 11, face = "bold"),
        legend.position = "right"
    )

ggsave("1214-stage2-B-lhat-lambda.png", plot = p, path = "Paper/images/", width = 12.5, height = 5.5, units = "in", dpi = 300)
cat("\nSaved: Paper/images/1214-stage2-B-lhat-lambda.png\n")
