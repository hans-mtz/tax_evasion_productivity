## Consolidated stage-2 ELVIS estimates so far, moment set A (9 moments),
## trim=0.5% -- both instruments, both seeding strategies (2026-09-08) -------
## Pools every fit computed today: the original naive-chain coarse+fine grids
## (each instrument warm-started from its own GMM warmstart file, chaining
## sequentially across lambda -- CLAUDE.md's original convention) against the
## lag_m-seeded grids (every lambda point fit INDEPENDENTLY from lag_m's own
## best-fit par -- no cross-lambda chaining at all, the corrected approach
## after the CLI par_init override was found to be a no-op for anything past
## the grid's first point). Purpose: one picture + one table documenting
## where things stand before starting the (delta1,delta2) grid.

library(tidyverse)
library(tinytable)
## Sourced directly (not via sys.source(..., attach(NULL,...))): that pattern
## gives render_png_tt_tbl a closure environment whose parent chain does not
## reach tinytable's namespace once library(tinytable) attaches AFTER the
## attach(NULL,...) environment was created -- save_tt() then fails to
## resolve from inside the function body (hit directly, not just reasoned
## about: "no se pudo encontrar la función 'save_tt'"). Plain source() gives
## the function the correct (.GlobalEnv-rooted) closure instead.
source("Code/Deconvolution/050-render-tbls.R")

extract <- function(path, ins, seed_strategy) {
    if (!file.exists(path)) { warning(sprintf("Missing: %s", path)); return(NULL) }
    e <- new.env(); load(path, envir = e)
    fits <- if (!is.null(e$fits)) e$fits else if (!is.null(e$lagm_fine) && grepl("lagm_fine", path)) e$lagm_fine else NULL
    do.call(rbind, lapply(fits, function(f) data.frame(
        ins = ins, seed_strategy = seed_strategy,
        lambda = f$lambda, Lhat = f$value,
        delta0 = f$par[1], delta1 = f$par[2], delta2 = f$par[3], eta = f$par[4],
        max_abs_gamma = f$max_abs_gamma, conv = f$convergence
    )))
}

## lag_m: naive-chain coarse (16) + naive-chain fine (8) + lag_m-seed fine check (4)
lagm_naive_coarse <- extract("Code/Products/1211-stage2-elvis-AB-lag_m-A-include_zero-nburn500-nkeep1000-maxevalb2000-trim0.005.RData.bak-9moment-coarse",
                              "lag_m", "naive_chain")
lagm_naive_fine   <- extract("Code/Products/1211-stage2-elvis-AB-lag_m-A-include_zero-nburn500-nkeep1000-maxevalb2000-trim0.005.RData.bak-9moment-fine-naiveseed",
                              "lag_m", "naive_chain")

## lag_2_cal_W: naive-chain coarse (16) + lag_m-seed all-points (16) + lag_m-seed low-lambda (3)
lag2w_naive_coarse <- extract("Code/Products/1211-stage2-elvis-AB-lag_2_cal_W-A-include_zero-nburn500-nkeep1000-maxevalb2000-trim0.005.RData.bak-9moment-coarse",
                               "lag_2_cal_W", "naive_chain")
lag2w_lagmseed_all <- extract("Code/Products/1235-stage2-lag2W-A9-trim0.005-coarse-lagmseed-allpoints.RData",
                               "lag_2_cal_W", "lag_m_seed")

## 1236 holds both lag_m's fine check (lagm_fine) and lag_2_cal_W's low-lambda check (lag2W_lowlambda)
e1236 <- new.env(); load("Code/Products/1236-stage2-checks-lowlambda-lagm-fine.RData", envir = e1236)
lagm_lagmseed_fine <- do.call(rbind, lapply(e1236$lagm_fine, function(f) data.frame(
    ins = "lag_m", seed_strategy = "lag_m_seed",
    lambda = f$lambda, Lhat = f$value,
    delta0 = f$par[1], delta1 = f$par[2], delta2 = f$par[3], eta = f$par[4],
    max_abs_gamma = f$max_abs_gamma, conv = f$convergence
)))
lag2w_lagmseed_low <- do.call(rbind, lapply(e1236$lag2W_lowlambda, function(f) data.frame(
    ins = "lag_2_cal_W", seed_strategy = "lag_m_seed",
    lambda = f$lambda, Lhat = f$value,
    delta0 = f$par[1], delta1 = f$par[2], delta2 = f$par[3], eta = f$par[4],
    max_abs_gamma = f$max_abs_gamma, conv = f$convergence
)))

all_fits <- bind_rows(lagm_naive_coarse, lagm_naive_fine, lagm_lagmseed_fine,
                       lag2w_naive_coarse, lag2w_lagmseed_all, lag2w_lagmseed_low) %>%
    mutate(omega_star = delta1 / (2 * delta2))

write.csv(all_fits, "Code/Products/1237-stage2-estimates-so-far.csv", row.names = FALSE)
cat("Saved: Code/Products/1237-stage2-estimates-so-far.csv (", nrow(all_fits), "rows)\n")

## %% Plot: delta1 vs delta2, color = instrument, shape = seeding strategy ---
INS_LABELS <- c(lag_m = "m*(t−1)", lag_2_cal_W = "\U0001D4B2(t−2)")
wong_cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536", "#0072B2", "#D55E00", "#CC79A7")

plot_df <- all_fits %>% mutate(panel = INS_LABELS[ins],
                                strategy_label = ifelse(seed_strategy == "naive_chain",
                                                         "Naive chain (own GMM warmstart)",
                                                         "lag_m-seeded (independent, per point)"))

best_df <- plot_df %>% group_by(ins) %>% slice_min(Lhat, n = 1) %>% ungroup()

p <- ggplot(plot_df, aes(x = delta1, y = delta2, color = panel, shape = strategy_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.6, alpha = 0.8) +
    geom_point(data = best_df, aes(x = delta1, y = delta2), shape = 21, size = 5,
               fill = NA, color = wong_cb_palette[1], stroke = 1.2, inherit.aes = FALSE) +
    scale_color_manual(values = c(setNames(wong_cb_palette[6:7], INS_LABELS))) +
    scale_shape_manual(values = c("Naive chain (own GMM warmstart)" = 4, "lag_m-seeded (independent, per point)" = 16)) +
    labs(x = expression(delta[1]), y = expression(delta[2]), color = "Instrument", shape = "Seeding strategy",
         title = "Stage-2 ELVIS: (δ1, δ2) estimates so far, moment set A, trim=0.5%",
         subtitle = "Open circle = each instrument's current best (lowest ˆL_n) point. Dashed lines mark zero.") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 9, hjust = 0.5),
        legend.position = "bottom", legend.box = "vertical"
    )

ggsave("Paper/images/1237-stage2-estimates-so-far-delta1-delta2.png", plot = p, width = 9, height = 7, units = "in", dpi = 300)
cat("Saved: Paper/images/1237-stage2-estimates-so-far-delta1-delta2.png\n")

## %% Best-result table, rendered to PNG via the project's tt()/render_png_tt_tbl convention
## (plain ASCII labels only -- tinytable's LaTeX backend silently drops the
## unicode script-W/minus-sign INS_LABELS used for the ggplot above, and an
## underscore/hyphen in "lag_m-seeded" gets parsed as LaTeX math mode,
## rendering as "lag" with a subscript m and a minus sign -- both hit
## directly on first render, not just anticipated)
INS_LABELS_ASCII <- c(lag_m = "m*(t-1)", lag_2_cal_W = "W(t-2)")
best_tbl <- best_df %>%
    transmute(
        Instrument = INS_LABELS_ASCII[ins],
        Seeding = ifelse(seed_strategy == "naive_chain", "Naive chain", "Seeded from lag-m"),
        lambda = signif(lambda, 3),
        delta0 = round(delta0, 3), delta1 = round(delta1, 3), delta2 = round(delta2, 3),
        eta = signif(eta, 3), `omega star` = round(omega_star, 3),
        Lhat = signif(Lhat, 4)
    )

render_png_tt_tbl(best_tbl |> tt(), "1237-stage2-best-estimates")
cat("Saved: Paper/tbls/1237-stage2-best-estimates.png\n")
print(best_tbl)
