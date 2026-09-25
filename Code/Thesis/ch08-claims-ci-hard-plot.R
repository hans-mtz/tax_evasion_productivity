## PRODUCT: Thesis/figures/ch08-claims-ci-hard.png := change in purchases-side deduction
## claims (Claims_i(Delta) = tau_tilde_P*[M_i+(1-q(e'))e'], the term actually shocked by
## the policy), HARD test (Theorem F.1), 9 tested Delta points.
##
## Promoted to headline (2026-09-23, Hans's call): revenue R = t1 - Claims bundles in the
## sales-tax term t1, whose own cross-firm heterogeneity dominates R's sampling variance
## (SD(t1/pgdp) approx 100,507 vs. SD(R_real) approx 100,423 -- almost identical; confirmed
## directly from the per-firm Delta=0 output, Code/Products/1479-*-firmlevel.csv joined with
## the estimation input). Claims never contains t1 at all (SD(Claims_real) approx 2,628, an
## order of magnitude tighter with zero correction), so it isolates the actual counterfactual
## mechanism -- the evasion/detection response to the purchases-tax shifter -- without
## importing noise from an unrelated, policy-invariant revenue stream. No control-variate
## needed (Hans, 2026-09-23): the row9_mode=3 ("theory", beta=1 exact identity
## R=t1-tau_tilde*[...]) runs already computed for the CV-vs-theory validation check are
## algebraically the raw Claims moment (theta fixed, gamma free, same construction as the
## R-headline) -- no new estimation, just relabeling already-computed output. See
## Code/Deconvolution/1306-theory-*.csv (9 Delta points: -8%,-5%,-4%,-3%,-2%,0%,+0.5%,+1%,+2%)
## and CLAUDE.md's "exogeneity/Delta-invariance concern... substantially addressed" entry for
## the beta=1 theory-coefficient's own validation (nests inside CV-R's own CI everywhere).
##
## row9_mode=3 computes g[9] = (R_real - t1/pgdp) - target = -Claims_real - target, so the
## tested candidate ("R" column in these files) already equals -Claims_real; no unshifting
## by mu_c needed (that step, in the earlier theory-vs-cv validation script, was only to put
## theory's candidates on the same R_real axis as CV-R's for a direct visual comparison).
source("Code/Thesis/001-setup.R")

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)
wong_red <- THESIS_COLS[1]; gray_fail <- THESIS_REJECT
delta_pct <- function(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 2), "%")

theory_files <- c(
    "1306-theory-coarse-delta0.csv", "1306-theory-coarse-delta1.csv",
    "1306-theory-coarse-delta2.csv", "1306-theory-coarse-delta3.csv",
    "1306-theory-anchor.csv", "1306-theory-remaining-delta0.csv",
    "1306-theory-remaining-deltam08.csv", "1306-theory-remaining-deltap005.csv",
    "1306-theory-remaining-deltap01.csv", "1306-theory-remaining-deltap02.csv"
)
df <- do.call(rbind, lapply(theory_files, function(f) read.csv(file.path(PRODUCTS_DIR, f))))
df <- df %>% mutate(Delta = round(Delta, 4), Claims = -R) %>%
    mutate(Claims_round = round(Claims, 4)) %>% distinct(Delta, Claims_round, .keep_all = TRUE) %>%
    select(-Claims_round) %>%
    mutate(TS = 2 * n * Lhat, pass = TS <= qc)

bounds <- df %>% group_by(Delta) %>%
    summarise(c_min_tested = min(Claims), c_max_tested = max(Claims),
              lower = suppressWarnings(min(Claims[pass])), upper = suppressWarnings(max(Claims[pass])),
              .groups = "drop") %>%
    mutate(open_low = is.finite(lower) & lower <= c_min_tested + 1e-6,
           open_high = is.finite(upper) & upper >= c_max_tested - 1e-6)

band <- bounds %>% filter(is.finite(lower)) %>% select(Delta, lower, upper) %>%
    pivot_longer(c(lower, upper), names_to = "bound", values_to = "Claims") %>%
    mutate(bound = recode(bound, lower = "Lower bound", upper = "Upper bound"))

delta_levels <- sort(unique(df$Delta))
df <- df %>% mutate(pass_lab = ifelse(pass, "Not rejected", "Rejected"),
                     Delta_f = factor(Delta, levels = delta_levels, labels = delta_pct(delta_levels)))
band <- band %>% mutate(Delta_f = factor(Delta, levels = delta_levels, labels = delta_pct(delta_levels)))

zero_bounds <- bounds %>% filter(Delta == 0)

p <- ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = zero_bounds$lower, ymax = zero_bounds$upper,
              fill = THESIS_BAND) +
    geom_line(data = band, aes(x = Delta_f, y = Claims, group = bound, linetype = bound),
              color = wong_red, linewidth = 0.5) +
    geom_point(data = df, aes(x = Delta_f, y = Claims, color = pass_lab, shape = pass_lab),
               size = 2.2) +
    scale_color_manual(values = c("Not rejected" = wong_red, "Rejected" = gray_fail), name = NULL) +
    scale_shape_manual(values = c("Not rejected" = 16, "Rejected" = 4), name = NULL) +
    scale_linetype_manual(values = c("Upper bound" = "twodash", "Lower bound" = "dotted"), name = NULL) +
    guides(color = guide_legend(order = 1), shape = guide_legend(order = 1),
           linetype = guide_legend(order = 2, override.aes = list(color = wong_red))) +
    labs(x = expression(paste("Change in the tax rate, ", Delta)),
         y = "Claimed deductions per firm-period (real COP)") +
    theme_thesis() +
    theme(legend.box = "horizontal")
save_thesis_plot(p, "ch08-claims-ci-hard", width = THESIS_WIDTH, height = 4.6)
cat("Saved: Thesis/figures/ch08-claims-ci-hard.{png,pdf}\n")
print(bounds %>% select(Delta, lower, upper, open_low, open_high))
