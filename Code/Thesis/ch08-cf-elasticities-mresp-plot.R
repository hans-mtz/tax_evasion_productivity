## PRODUCT: Thesis/figures/ch08-cf-elasticities-mresp.png := behavioural parts of the arc elasticity of claimed purchase
## deductions with respect to tau_P, true materials responding (headline), by change Delta in the purchases rate
## (+-1, 1.5, 2%): evasion response and input response, estimates and 95% conservative-test sets. ELVIS interior firms.
## Source: Code/Products/ch08-cf-elasticities-mresp.csv, written by ch08-cf-elasticities-mresp-table.R (run it first);
## see that script's header for the runs (1655, 1653) and the definitions. Replaces ch08-behavioural-elasticity
## (fixed M, pre-audit solver).
source("Code/Thesis/001-setup.R")

d <- read.csv(file.path(PRODUCTS_DIR, "ch08-cf-elasticities-mresp.csv"))
b <- bind_rows(
    d %>% transmute(Delta, part = "Evasion response", est = evasion, lo = evasion_lo, hi = evasion_hi),
    d %>% transmute(Delta, part = "Input response", est = input, lo = input_lo, hi = input_hi)) %>%
    mutate(x = 100 * Delta, part = factor(part, levels = c("Evasion response", "Input response")))
print(b)

p <- ggplot(b, aes(x = x, y = est, colour = part)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4, linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.35, linewidth = 0.6,
                    position = position_dodge(width = 0.15)) +
    scale_colour_manual(values = c("Evasion response" = THESIS_COLS[1], "Input response" = THESIS_COLS[2])) +
    scale_x_continuous(breaks = unique(b$x), labels = function(v) paste0(ifelse(v > 0, "+", ifelse(v < 0, "−", "")), abs(v), "%")) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = expression("Change in the purchases rate, " * Delta), y = "Elasticity of claims, behavioural part") +
    theme_thesis()

save_thesis_plot(p, "ch08-cf-elasticities-mresp", height = 3.6)
cat("Saved: Thesis/figures/ch08-cf-elasticities-mresp.{png,pdf}\n")
