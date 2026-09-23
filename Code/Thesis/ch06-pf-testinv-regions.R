## PRODUCT: Thesis/figures/ch06-pf-testinv-regions.png := Figure 6.x, joint test-inversion regions for
## (alpha_K, alpha_L), both instruments (m*_{it-1}, W~_{it-2}), beta fixed. Companion to ch06-pf-comparison.png:
## shows BOTH the sharp (chi2_3) and conservative (chi2_5) regions for all 5 paper industries, including 313
## (whose sharp region is empty -- see Research-log 2026-09-21).
## Reads: Code/Products/1477-pf-joint-testinv-grid.csv (already-computed grid; no re-estimation here).
source("Code/Thesis/001-setup.R")
res <- read.csv(file.path(PRODUCTS_DIR, "1477-pf-joint-testinv-grid.csv")) %>%
    mutate(pass = J <= qchisq(.95, 3), pass_cons = J <= qchisq(.95, 5))
smry <- res %>% group_by(sic_3) %>% summarise(aK_at_min = aK[which.min(J)], aL_at_min = aL[which.min(J)], .groups = "drop")

pal <- "#0072B2"
p <- ggplot(res, aes(aK, aL)) +
    geom_tile(data = res %>% filter(pass_cons), fill = pal, alpha = .25) +
    geom_tile(data = res %>% filter(pass), fill = pal, alpha = .6) +
    geom_point(data = smry, aes(aK_at_min, aL_at_min), shape = 4, size = 2.5, stroke = 1.1, colour = "black") +
    facet_wrap(~sic_3, nrow = 2, labeller = labeller(sic_3 = \(z) paste("Industry", z))) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = expression(alpha[K]), y = expression(alpha[L]),
         subtitle = "Dark = chi2_3,.95 (sharp, gamma profiled); light = chi2_5,.95 (conservative); cross = grid minimum") +
    theme_thesis(base_size = 11)
save_thesis_plot(p, "ch06-pf-testinv-regions", width = 11, height = 7)
cat("Saved: Thesis/figures/ch06-pf-testinv-regions.{png,pdf}\n")
