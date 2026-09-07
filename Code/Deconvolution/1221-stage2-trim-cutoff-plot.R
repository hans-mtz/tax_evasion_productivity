## Stage-2 ELVIS -- trim-cutoff figure (2026-09-05) --------------------------
## The headline deliverable for this session's trim-robustness work: x-axis =
## trim threshold (top X% of interior firms by M_star dropped), y-axis =
## lambda, showing the point estimate (argmin within each trim level's own
## grid) and the 95% test-inversion CI (same Theorem F.1 chi^2 shortcut used
## throughout this project), for both `ins` choices. Mirrors AK2020's own
## F_figure1.R construction (upper/lower bound lines from inverting the same
## test at each grid point) -- see CLAUDE.md for the full derivation and why
## this is the right way to read off a confidence band, not a shortcut.
##
## Expected/hoped-for shape (per the user, motivating this whole exercise):
## lambda increases with trim (extreme evaders no longer pin it near 0), then
## STABILIZES (a plateau -- the point where enough leverage has been removed
## without discarding informative data), then increases AGAIN at some point
## (over-trimming: too much real data discarded, or precision degrading). The
## stable plateau is the trim cutoff -- this figure is what documents that
## decision for the paper and the next supervisor meeting.

library(tidyverse)

d_g <- 8
alpha <- 0.05
crit <- qchisq(1 - alpha, df = d_g)

## 2026-09-05, revised: capped at 10% (user's reasoning -- a few aggressive
## over-reporters biasing lambda doesn't justify throwing away more than
## that much of the sample), dense 0-5%, sparse 5-10%; also switched from
## independent-per-level runs to cross-trim-level warm-started sequential
## chains (run-trim-sequential.sh) after the independent version produced an
## unusably noisy plot (see CLAUDE.md, "Trim-cutoff sweep noise" entry).
trims_new <- c(0.0005, 0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.07, 0.10)
ins_choices <- c("lag_m", "lag_2_cal_W")

## %% New trim-sweep results (linear q, 16-point continuous grid each) ------
extract_one <- function(trim, ins) {
    f <- sprintf("Code/Products/1211-stage2-elvis-AB-%s-A-include_zero-nburn500-nkeep1000-maxevalb2000-trim%s.RData",
                 ins, sprintf("%g", trim))
    if (!file.exists(f)) {
        warning(sprintf("Missing: %s", f)); return(NULL)
    }
    e <- new.env(); load(f, envir = e); fits <- e$fits
    d <- data.frame(lambda = sapply(fits, `[[`, "lambda"), Lhat = sapply(fits, `[[`, "value"), n = sapply(fits, `[[`, "n"))
    d <- d[order(d$lambda), ]
    n <- d$n[1]
    Lhat_min <- min(d$Lhat, na.rm = TRUE)
    threshold <- Lhat_min + crit / (2 * n)
    passing <- d$lambda[d$Lhat <= threshold]
    data.frame(
        trim = trim, ins = ins, n = n,
        lambda_hat = d$lambda[which.min(d$Lhat)],
        ci_lo = min(passing), ci_hi = max(passing),
        lo_at_edge = min(passing) == min(d$lambda),
        hi_at_edge = max(passing) == max(d$lambda)
    )
}

new_rows <- bind_rows(lapply(trims_new, function(tr) bind_rows(lapply(ins_choices, extract_one, trim = tr))))

## %% trim=0: reuse Phase 0's already-resolved fine sweep, NOT rerun here ----
## Lower CI bound never resolved even at lambda=1e-12 (CLAUDE.md, Phase-0
## entry) -- represented as NA (not zero, not the smallest tested point) so
## the plot can show it as visually distinct (an open/unresolved bound) from
## every trimmed level's genuinely bracketed lower bound.
load("Code/Products/1218-stage2-lambda-ci.RData")  # df, df2, summary_tbl, ci_bounds
max_tested_by_ins <- df %>% group_by(ins) %>% summarise(max_lambda = max(lambda), .groups = "drop")
zero_rows <- ci_bounds %>%
    left_join(summary_tbl %>% select(ins, n), by = "ins") %>%
    left_join(max_tested_by_ins, by = "ins") %>%
    transmute(trim = 0, ins, n, lambda_hat, ci_lo = NA_real_, ci_hi = lambda_hi,
              lo_at_edge = NA, hi_at_edge = (lambda_hi == max_lambda))

all_rows <- bind_rows(zero_rows, new_rows) %>% arrange(ins, trim)

cat("---- Trim-cutoff sweep: point estimate + 95% CI for lambda, by trim level ----\n")
print(as.data.frame(all_rows), digits = 4)

## Flag any level where the CI still touches its own grid's edge -- same
## honesty check as everywhere else this session; these numbers are directional
## at that trim level, not a fully closed bracket.
edge_flags <- all_rows %>% filter(isTRUE(lo_at_edge) | isTRUE(hi_at_edge))
if (nrow(edge_flags) > 0) {
    cat("\n---- WARNING: CI touches tested grid edge at these levels (bound may extend further) ----\n")
    print(as.data.frame(edge_flags), digits = 4)
}

write.csv(all_rows, "Code/Products/1221-stage2-trim-cutoff-summary.csv", row.names = FALSE)
cat("\nSaved: Code/Products/1221-stage2-trim-cutoff-summary.csv\n")

## %% Figure -- AK2020 F_figure1.R style: trim (%) on x, lambda (log10) on y,
## point-estimate line + upper/lower CI bound lines, one panel per ins. -----
INS_LABELS <- c(lag_m = "m*(t−1)", lag_2_cal_W = "\U0001D4B2(t−2)")
wong_cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536", "#0072B2", "#D55E00", "#CC79A7")

plot_df <- all_rows %>% mutate(panel = INS_LABELS[ins], trim_pct = trim * 100)

p <- ggplot(plot_df, aes(x = trim_pct)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = wong_cb_palette[3], alpha = 0.25, na.rm = TRUE) +
    geom_line(aes(y = ci_hi), color = wong_cb_palette[6], linetype = "dashed", linewidth = 0.6, na.rm = TRUE) +
    geom_line(aes(y = ci_lo), color = wong_cb_palette[6], linetype = "dotted", linewidth = 0.6, na.rm = TRUE) +
    geom_line(aes(y = lambda_hat), color = wong_cb_palette[1], linewidth = 0.8) +
    geom_point(aes(y = lambda_hat), color = wong_cb_palette[1], size = 1.8) +
    facet_wrap(~panel, nrow = 1) +
    scale_y_log10(labels = scales::label_scientific()) +
    labs(
        x = "Trim threshold (top % of interior firms by M* dropped)",
        y = expression(hat(lambda)~"(log scale)"),
        title = "Stage-2 lambda vs. trim threshold: point estimate and 95% test-inversion CI",
        subtitle = "Solid = point estimate (argmin); dashed/dotted = upper/lower 95% CI bound; shaded = CI band.\nlambda=0% trim's lower bound is unresolved (never rejected down to 1e-12) -- shown as an open band, not a point."
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        strip.text = element_text(family = "Times", size = 12, face = "bold"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 9.5, hjust = 0.5)
    )

ggsave("Paper/images/1221-stage2-trim-cutoff.png", plot = p, width = 12, height = 6, units = "in", dpi = 300)
cat("\nSaved: Paper/images/1221-stage2-trim-cutoff.png\n")
