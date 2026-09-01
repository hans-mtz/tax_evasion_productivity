## Stage-2 ELVIS -- exploratory profile-likelihood-style CI for lambda -------
## (2026-09-01) NOT for the slides -- exploratory only, per the user's own
## framing ("just for me"). Was scratchpad-only initially; the user asked to
## keep this one (2026-09-01) alongside 1212/1214 for future comparison as
## the grid gets extended, so it now saves to Paper/images/ like those --
## still not referenced from any .qmd, still not a slide-ready figure.
## Paper/images/, and this script is not referenced from any .qmd.
##
## Schennach's profiled statistic (her Theorem F.1 / the supplement's
## implementation notes, already documented in CLAUDE.md):
##   Q_n(beta) = -( sup_{eta,gamma} L_n(beta,eta,gamma) - sup_{theta,gamma} L_n(theta,gamma) )
## In OUR sign convention (Lhat = -L_n: AK2020-style, positive, MINIMIZED,
## not Schennach's own negative/maximized L_n -- see CLAUDE.md's 2026-08-31
## "Sign/scaling convention" entry), this becomes simply:
##   Q_n(lambda) = Lhat_profile(lambda) - min(Lhat across the whole grid)
## i.e. subtract the grid's own global minimum from every point. Lhat itself
## is ALREADY the correct profile at each lambda (fit_one_lambda_A/B jointly
## optimizes (delta0-2,eta,mu_m,gamma) via BOBYQA at each lambda grid point
## -- exactly sup_{eta,gamma} at fixed lambda, nothing new needed there).
##
## 95% confidence region (Theorem F.1's conservative chi^2 shortcut, no
## subsampling): {lambda : 2*n*Q_n(lambda) <= chi^2_{d_g,0.95}}, equivalently
## {lambda : Lhat(lambda) <= min(Lhat) + chi^2_{d_g,0.95}/(2n)} -- a simple
## horizontal threshold line on the existing Lhat(lambda) plots.
##
## d_g: A=8 (fixed); B=8+J+6, J=29 industries (confirmed in-session).

library(tidyverse)

extract_fits <- function(path, ins_label, moment_set) {
    e <- new.env(); load(path, envir = e); fits <- e$fits
    do.call(rbind, lapply(fits, function(f) data.frame(
        moment_set = moment_set, ins = ins_label, phase = f$phase,
        lambda = f$lambda, Lhat = f$value, n_used = f$n, conv = f$convergence
    )))
}
files <- list(
    A = c(lag_m       = "Code/Products/1211-stage2-elvis-AB-lag_m-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData",
          lag_2_cal_W = "Code/Products/1211-stage2-elvis-AB-lag_2_cal_W-A-include_zero-nburn500-nkeep1000-maxevalb1000.RData"),
    B = c(lag_m       = "Code/Products/1211-stage2-elvis-AB-lag_m-B-include_zero-nburn500-nkeep1000-maxevalb5000.RData",
          lag_2_cal_W = "Code/Products/1211-stage2-elvis-AB-lag_2_cal_W-B-include_zero-nburn500-nkeep1000-maxevalb5000.RData")
)
df <- bind_rows(lapply(names(files), function(ms) {
    bind_rows(Map(function(p, n) extract_fits(p, n, ms), files[[ms]], names(files[[ms]])))
}))

d_g   <- c(A = 8, B = 8 + 29 + 6)
alpha <- 0.05
crit  <- qchisq(1 - alpha, df = d_g)

summary_tbl <- df %>%
    group_by(moment_set, ins) %>%
    summarise(Lhat_min = min(Lhat), n = n_used[which.min(Lhat)], lambda_min = lambda[which.min(Lhat)], .groups = "drop") %>%
    mutate(dg = d_g[moment_set], crit = crit[moment_set], threshold = Lhat_min + crit / (2 * n))

cat("---- 95% profile-CI threshold per (moment_set, ins) ----\n")
print(as.data.frame(summary_tbl), digits = 6)

df2 <- df %>%
    left_join(summary_tbl %>% select(moment_set, ins, threshold, Lhat_min, lambda_min), by = c("moment_set", "ins")) %>%
    mutate(in_CI = Lhat <= threshold)

cat("\n---- grid points inside the band (NOTE: argmin sits at the smallest\n")
cat("tested lambda in all four cases, so the true CI lower bound is NOT\n")
cat("resolved by this grid -- almost certainly extends further left) ----\n")
print(as.data.frame(df2 %>% filter(in_CI) %>% arrange(moment_set, ins, lambda) %>%
    select(moment_set, ins, phase, lambda, Lhat, threshold)), digits = 5)

## %% Figure ------------------------------------------------------------
## Exploratory: shows the threshold line and shades the (grid-visible) band.
## The left edge of every panel is annotated as OPEN (unresolved) since the
## argmin sits exactly at the smallest tested lambda everywhere.
## Instrument labels (2026-09-01, per user): lag_m = m*_(it-1), lag_2_cal_W =
## W_(it-2) -- see 1212's matching comment for why this is a plain-text
## approximation rather than full LaTeX/calligraphic.
INS_LABELS <- c(lag_m = "m*(t−1)", lag_2_cal_W = "\U0001D4B2(t−2)")
wong_cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536", "#0072B2", "#D55E00", "#CC79A7")

df2$panel <- paste0(df2$moment_set, " / ", INS_LABELS[df2$ins])
df2$phase_lbl <- ifelse(df2$phase == "quantile", "Quantile grid", "Linear refinement")

p <- df2 %>%
    filter(phase == "linear_refine") %>%   # zoom to the refined region -- the CI band only ever lives here
    arrange(panel, lambda) %>%
    ggplot(aes(x = lambda, y = Lhat)) +
    geom_hline(aes(yintercept = threshold), color = wong_cb_palette[7], linetype = "dashed", linewidth = 0.6) +
    geom_line(color = wong_cb_palette[6], linewidth = 0.7) +
    geom_point(aes(color = in_CI), size = 2.4) +
    scale_color_manual(values = c(`TRUE` = wong_cb_palette[4], `FALSE` = wong_cb_palette[6]),
                        labels = c(`TRUE` = "Inside 95% band", `FALSE` = "Outside"), name = NULL) +
    facet_wrap(~panel, scales = "free", nrow = 2) +
    scale_x_log10(labels = scales::label_scientific()) +
    labs(
        x = expression(lambda~"(log scale, refined-region points only)"),
        y = expression(hat(L)[n]),
        title = "Exploratory: 95% profile-CI threshold, Q_n(lambda) shortcut (Theorem F.1)",
        subtitle = "Dashed line = min(Lhat) + chi^2_{d_g,0.95}/(2n)  --  NOT for the slides, sanity check only.\nArgmin sits at the smallest tested lambda in all 4 panels -- left edge is OPEN, not resolved by this grid."
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        strip.text = element_text(family = "Times", size = 11, face = "bold"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 10, hjust = 0.5),
        legend.position = "bottom"
    )

ggsave("1215-stage2-profile-CI-explore.png", plot = p, path = "Paper/images/", width = 11, height = 8, units = "in", dpi = 300)
cat("\nSaved: Paper/images/1215-stage2-profile-CI-explore.png\n")
