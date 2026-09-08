## Stage-2 ELVIS -- all-gridpoints pass/fail figure, re-sweep with the fixed
## h' score moment (2026-09-07) ----------------------------------------------
## Same construction as the original 1223-stage2-trim-all-gridpoints.png
## (2026-09-05, generated ad hoc, no script saved -- this fills that gap) but
## for the SMALLER 5-level re-sweep run after (a) the h_prime_of_e floor fix
## (Code/Rcpp/1200-stage2-elvis-common.h, h_denom) and (b) switching the score
## moment to exp(h_prime) instead of the raw (unboundedly divergent) value --
## see CLAUDE.md's "Estimation" section and Research-log/log.md for the full
## diagnosis. Every point here is a genuine test-inversion result: reject if
## TS(lambda)=2*n*[Lhat(lambda)-min_lambda' Lhat(lambda')] > chi2_{d_g,0.95},
## d_g=8 for moment set A (Theorem F.1's conservative chi^2 shortcut, same
## convention used throughout this project). Plotting EVERY tested grid point
## (not just a summary CI band, unlike 1221-stage2-trim-cutoff-plot.R) is
## deliberate -- CLAUDE.md's own lesson from the first trim-cutoff attempt:
## a summary CI can hide whether a "passing" region is a real plateau or an
## isolated optimizer-artifact spike; this figure lets that be checked by eye.

library(tidyverse)

d_g   <- 8
alpha <- 0.05
crit  <- qchisq(1 - alpha, df = d_g)
cat(sprintf("d_g=%d, chi2_{%d,%.2f} critical value = %.3f\n", d_g, d_g, 1 - alpha, crit))

trims       <- c(0, 0.001, 0.005, 0.01, 0.05)
trim_labels <- c("0%", "0.1%", "0.5%", "1%", "5%")
ins_choices <- c("lag_m", "lag_2_cal_W")

## Filenames: trim=0 has NO "-trim..." suffix (build_run_sample's trim_tag is
## "" when trim_top_pct==0, see 1211-stage2-elvis-driver-AB.R) -- every other
## level does. Matches this run's actual product files exactly (verified via
## `stat -f '%Sm'` against the Sept 7 run before writing this, since several
## STALE files from the old Sept 5 10-level sweep share overlapping trim
## values, e.g. trim0.005 exists from both sessions -- this run's own files
## were all freshly overwritten today so there is no ambiguity in practice,
## but flagged here in case this script is ever rerun after a stale restore).
product_path <- function(ins, trim) {
    tag <- if (trim > 0) sprintf("-trim%g", trim) else ""
    sprintf("Code/Products/1211-stage2-elvis-AB-%s-A-include_zero-nburn500-nkeep1000-maxevalb2000%s.RData", ins, tag)
}

extract_grid <- function(ins, trim) {
    f <- product_path(ins, trim)
    if (!file.exists(f)) { warning(sprintf("Missing: %s", f)); return(NULL) }
    e <- new.env(); load(f, envir = e); fits <- e$fits
    d <- data.frame(
        ins    = ins,
        trim   = trim,
        lambda = sapply(fits, `[[`, "lambda"),
        Lhat   = sapply(fits, `[[`, "value"),
        n      = sapply(fits, `[[`, "n"),
        conv   = sapply(fits, function(x) if (!is.null(x$convergence)) x$convergence else NA)
    )
    d$Qn <- d$Lhat - min(d$Lhat, na.rm = TRUE)
    d$TS <- 2 * d$n * d$Qn
    d$reject <- d$TS > crit
    d
}

all_grid <- bind_rows(lapply(ins_choices, function(i) bind_rows(lapply(trims, extract_grid, ins = i))))

## Sanity check, printed not just assumed: flag any point whose BOBYQA fit
## didn't converge (conv != 1) -- a non-converged point's Lhat shouldn't be
## trusted for the test inversion even if it happens to pass/fail either way.
nonconv <- all_grid %>% filter(!is.na(conv) & conv != 1)
if (nrow(nonconv) > 0) {
    cat("\n---- WARNING: non-converged grid points (conv != 1) ----\n")
    print(as.data.frame(nonconv), digits = 4)
} else {
    cat("\nAll grid points converged (conv=1) -- no non-convergence caveat needed.\n")
}

write.csv(all_grid, "Code/Products/1222-stage2-trim-all-gridpoints.csv", row.names = FALSE)
cat("\nSaved: Code/Products/1222-stage2-trim-all-gridpoints.csv\n")

## %% Figure: trim on a DISCRETE, equally-spaced x-axis (not proportional to
## the trim value itself -- trims span 0% to 5%, and a continuous axis was
## tried and rejected the first time this figure was made, 2026-09-05: it
## bunches every point unreadably close to zero). y = lambda, log scale.
## Point shape/color = reject vs fail-to-reject, so a real plateau (many
## adjacent passing points) is visually distinct from an isolated passing
## spike surrounded by rejects (the exact failure mode CLAUDE.md warns about).
INS_LABELS <- c(lag_m = "m*(t−1)", lag_2_cal_W = "\U0001D4B2(t−2)")
wong_cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536", "#0072B2", "#D55E00", "#CC79A7")

plot_df <- all_grid %>%
    mutate(
        panel      = INS_LABELS[ins],
        trim_f     = factor(trim, levels = trims, labels = trim_labels),
        status     = ifelse(reject, "Rejected", "Fail to reject (95% CI)")
    )

argmin_df <- plot_df %>%
    group_by(ins, trim_f, panel) %>%
    slice_min(Lhat, n = 1) %>%
    ungroup()

p <- ggplot(plot_df, aes(x = trim_f, y = lambda)) +
    geom_point(aes(color = status, shape = status), size = 2.6, alpha = 0.85,
               position = position_jitter(width = 0.08, height = 0, seed = 1)) +
    geom_point(data = argmin_df, aes(x = trim_f, y = lambda),
               shape = 21, size = 4.2, fill = NA, color = wong_cb_palette[1], stroke = 1.1) +
    scale_color_manual(values = c("Rejected" = wong_cb_palette[7], "Fail to reject (95% CI)" = wong_cb_palette[6])) +
    scale_shape_manual(values = c("Rejected" = 4, "Fail to reject (95% CI)" = 16)) +
    facet_wrap(~panel, nrow = 1) +
    scale_y_log10(labels = scales::label_scientific()) +
    labs(
        x = "Trim threshold (top % of interior firms by M* dropped)",
        y = expression(lambda~"(log scale)"),
        color = NULL, shape = NULL,
        title = "Stage-2 lambda grid: test-inversion pass/fail at every tested point",
        subtitle = sprintf("Re-swept with the fixed (floored + exp-bounded) h' score moment. Open circle = argmin within that trim level.\nReject if TS=2n[Lhat-min(Lhat)] > chi2_{%d,0.95}=%.2f. x-axis is discrete/equally-spaced, not proportional to trim %%.", d_g, crit)
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        strip.text = element_text(family = "Times", size = 12, face = "bold"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 9, hjust = 0.5),
        legend.position = "bottom"
    )

ggsave("Paper/images/1222-stage2-trim-all-gridpoints-refixed.png", plot = p, width = 12, height = 6.5, units = "in", dpi = 300)
cat("\nSaved: Paper/images/1222-stage2-trim-all-gridpoints-refixed.png\n")
