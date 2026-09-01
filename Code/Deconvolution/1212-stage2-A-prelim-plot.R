## Stage-2 ELVIS, moment set A -- preliminary-results figure + table numbers --
## for Quarto-Slides/sections/650-stage2-prelim-results.qmd (2026-08-31).
##
## Reuses the fits already saved by 1211-stage2-elvis-driver-AB.R
## (n_burn=500, n_keep=1000, ins in {lag_m, lag_2_cal_W}, moment_set=A,
## corner_mode=include_zero) -- no new estimation here, just plotting +
## a few derived summary numbers for the slide table.
##
## The "implied P(detection)" table column originally computed here (via
## E[V]=E[u]-E[eps], then e=mean_Mstar*(1-exp(-E[u]))) was WRONG, caught by
## the user same day: u=ln((M+e)/M) is nonlinear in e, so V=u-eps never
## decomposes additively into e at all -- no shortcut through V's own
## moments works, regardless of E[.] vs Med(.). Superseded by
## Code/Deconvolution/1213-stage2-A-aux-e.R, which treats E[e] and Med[e] as
## genuine ELVIS auxiliary parameters (Schennach's own E[U] device),
## evaluated post-hoc at the already-fitted (theta_hat,gamma_hat) via the new
## tilted_e_diag_A_cpp export -- see that file's header and CLAUDE.md's
## 2026-08-31 correction entry for the full derivation. This file now only
## produces the lambda-vs-Lhat figure; the table's aux-e column comes from
## 1213's output (Code/Products/1213-stage2-A-aux-e.rds).

library(tidyverse)

RESULT_FILES <- c(
    lag_m       = "Code/Products/1211-stage2-elvis-AB-lag_m-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData",
    lag_2_cal_W = "Code/Products/1211-stage2-elvis-AB-lag_2_cal_W-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData"
)

extract_fits <- function(path, ins_label) {
    e <- new.env(); load(path, envir = e); fits <- e$fits
    do.call(rbind, lapply(fits, function(f) data.frame(
        ins = ins_label, phase = f$phase, lambda = f$lambda, Lhat = f$value,
        n_used = f$n, p_boundary = f$p_boundary, conv = f$convergence,
        max_abs_gamma = f$max_abs_gamma,
        delta0 = f$par[1], delta1 = f$par[2], delta2 = f$par[3], eta = f$par[4]
    )))
}
df <- do.call(rbind, Map(extract_fits, RESULT_FILES, names(RESULT_FILES)))
df$omega_star <- df$delta1 / (2 * df$delta2)

best <- do.call(rbind, lapply(split(df, df$ins), function(s) s[which.min(s$Lhat), ]))

cat("---- Best fit per ins ----\n")
print(best[, c("ins", "phase", "lambda", "Lhat", "delta0", "delta1", "delta2", "omega_star", "max_abs_gamma", "p_boundary")])
cat("\n(For E[e]/Med[e] auxiliary parameters, see 1213-stage2-A-aux-e.R)\n")

## %% Figure -----------------------------------------------------------------
## Instrument labels (2026-09-01, per user): lag_m = m*_(it-1), lag_2_cal_W =
## W_(it-2). Plain-text approximation here (ggplot2 legends can't render full
## LaTeX/calligraphic fonts) -- the slide prose/tables use the exact LaTeX
## ($m^*_{it-1}$, $\mathcal{W}_{it-2}$) where it renders natively.
INS_LABELS <- c(lag_m = "m*(t−1)", lag_2_cal_W = "\U0001D4B2(t−2)")
wong_cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536", "#0072B2", "#D55E00", "#CC79A7")
ins_colors <- setNames(wong_cb_palette[6:7], INS_LABELS[c("lag_m", "lag_2_cal_W")])

df$phase_lbl <- ifelse(df$phase == "quantile", "Quantile grid", "Linear refinement")
df$ins_lbl   <- factor(INS_LABELS[df$ins], levels = INS_LABELS[c("lag_m", "lag_2_cal_W")])

p <- df %>%
    arrange(ins, lambda) %>%
    ggplot(aes(x = lambda, y = Lhat, color = ins_lbl, linetype = phase_lbl, group = interaction(ins, phase))) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.6) +
    scale_x_log10(labels = scales::label_scientific()) +
    scale_color_manual(values = ins_colors, name = "Instrument") +
    scale_linetype_manual(values = c("Quantile grid" = "22", "Linear refinement" = "solid"), name = "Phase") +
    labs(
        x = expression(lambda~"(log scale)"),
        y = expression(hat(L)[n]~"(CUE objective)"),
        title = "Stage-2 ELVIS: CUE objective across the lambda grid",
        subtitle = "Moment set A, n_burn=500, n_keep=1000, corner_mode=include_zero"
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        axis.text = element_text(family = "Times", size = 10),
        axis.title = element_text(family = "Times", size = 12, face = "bold"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 11, hjust = 0.5),
        legend.text = element_text(family = "Times", size = 10),
        legend.title = element_text(family = "Times", size = 11, face = "bold"),
        legend.position = "right"
    )

ggsave("1212-stage2-A-lhat-lambda.png", plot = p, path = "Paper/images/", width = 9.5, height = 5.5, units = "in", dpi = 300)
cat("\nSaved: Paper/images/1212-stage2-A-lhat-lambda.png\n")
