## PRODUCT: Thesis/figures/ch08-behavioural-elasticity.png := behavioural elasticity of claimed purchase deductions
## with respect to tau_P by tax-rate change Delta (+-1, +-1.5, +-2%), estimates and 95% conservative-test sets.
## Source: Code/Products/ch08-behavioural-elasticity.csv, written by ch08-elasticities-table.R (run it first); see
## that script's header for the runs (1639, 1640, 1622) and the definition.
source("Code/Thesis/001-setup.R")

b <- read.csv(file.path(PRODUCTS_DIR, "ch08-behavioural-elasticity.csv")) %>%
    mutate(side = ifelse(Delta < 0, "Rate cut", "Rate increase"), x = 100 * Delta)
print(b)

p <- ggplot(b, aes(x = x, y = est, colour = side)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4, linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.35, linewidth = 0.6) +
    scale_colour_manual(values = c("Rate cut" = THESIS_COLS[2], "Rate increase" = THESIS_COLS[1]),
                        breaks = c("Rate increase", "Rate cut")) +
    scale_x_continuous(breaks = b$x, labels = function(v) paste0(ifelse(v > 0, "+", ifelse(v < 0, "−", "")), abs(v), "%")) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = expression("Change in the purchases rate, " * Delta), y = "Behavioural elasticity of claims") +
    theme_thesis()

save_thesis_plot(p, "ch08-behavioural-elasticity", height = 3.6)
cat("Saved: Thesis/figures/ch08-behavioural-elasticity.{png,pdf}\n")
