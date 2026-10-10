## PRODUCT: Thesis/figures/ch08-cf-elasticities-mresp.png := behavioural parts of the arc elasticity of claimed purchase
## deductions with respect to tau_P, true materials responding (headline), by change Delta in the purchases rate
## (+-1, 1.5, 2%): overreporting response and materials response, estimates and 95% conservative-test sets. ELVIS interior firms.
## Source: Code/Products/ch08-cf-elasticities-mresp.csv, written by ch08-cf-elasticities-mresp-table.R (run it first);
## see that script's header for the runs (1655, 1653) and the definitions. Replaces ch08-behavioural-elasticity
## (fixed M, pre-audit solver).
source("Code/Thesis/001-setup.R")

d <- read.csv(file.path(PRODUCTS_DIR, "ch08-cf-elasticities-mresp.csv"))
b <- bind_rows(
    d %>% transmute(Delta, part = "Overreporting response", est = evasion, lo = evasion_lo, hi = evasion_hi),
    d %>% transmute(Delta, part = "Materials response", est = input, lo = input_lo, hi = input_hi)) %>%
    mutate(part = factor(part, levels = c("Overreporting response", "Materials response")))
# Hans 2026-10-10: equally spaced positions, Delta = 0 skipped (no wider gap between -1 and +1 than between the other points);
# the dashed vertical line sits halfway between -1 and +1
lv <- sort(unique(b$Delta)); stopifnot(all(lv != 0), sum(lv < 0) == sum(lv > 0))
b <- b %>% mutate(x = match(Delta, lv) - (length(lv) + 1) / 2)
lab <- function(D) paste0(ifelse(D > 0, "+", "\u2212"), abs(100 * D), "%")
print(b)

p <- ggplot(b, aes(x = x, y = est, colour = part)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4, linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.35, linewidth = 0.6,
                    position = position_dodge(width = 0.15)) +
    scale_colour_manual(values = c("Overreporting response" = THESIS_COLS[1], "Materials response" = THESIS_COLS[2])) +
    scale_x_continuous(breaks = seq_along(lv) - (length(lv) + 1) / 2, labels = lab(lv)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = expression("Change in the purchases rate, " * Delta), y = "Elasticity of claims, behavioural part") +
    theme_thesis()

save_thesis_plot(p, "ch08-cf-elasticities-mresp", height = 3.6)
cat("Saved: Thesis/figures/ch08-cf-elasticities-mresp.{png,pdf}\n")
