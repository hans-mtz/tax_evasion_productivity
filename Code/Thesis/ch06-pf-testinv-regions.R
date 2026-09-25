## PRODUCT: Thesis/figures/ch06-pf-testinv-regions.png := Figure 6.x, joint test-inversion regions for
## (alpha_K, alpha_L), both instruments (m*_{it-1}, W~_{it-2}), beta fixed. Companion to ch06-pf-comparison.png:
## shows BOTH the sharp (chi2_3) and conservative (chi2_5) regions for all 5 paper industries, including 313
## (whose sharp region is empty -- see Research-log 2026-09-21).
## Reads: Code/Products/1477-pf-joint-testinv-grid.csv (already-computed grid; no re-estimation here).
source("Code/Thesis/001-setup.R")
res <- read.csv(file.path(PRODUCTS_DIR, "1477-pf-joint-testinv-grid.csv")) %>%
    mutate(pass = J <= qchisq(.95, 3), pass_cons = J <= qchisq(.95, 5))
smry <- res %>% group_by(sic_3) %>% summarise(aK_at_min = aK[which.min(J)], aL_at_min = aL[which.min(J)], .groups = "drop")

regions <- bind_rows(
    res %>% filter(pass_cons) %>% mutate(test = "Conservative test"),
    res %>% filter(pass) %>% mutate(test = "Sharp test")
) %>% mutate(test = factor(test, c("Sharp test", "Conservative test")))
p <- ggplot(regions, aes(aK, aL)) +
    geom_tile(aes(fill = test)) +
    geom_point(data = smry, aes(aK_at_min, aL_at_min), shape = 4, size = 2, stroke = 0.9, colour = "grey10") +
    facet_wrap(~sic_3, nrow = 2, labeller = labeller(sic_3 = \(z) paste("Industry", z))) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_x_continuous(breaks = c(0, .5, 1)) + scale_y_continuous(breaks = c(0, .5, 1)) +
    scale_fill_manual(values = c(`Sharp test` = THESIS_COLS[1], `Conservative test` = THESIS_LIGHT)) +
    labs(x = expression(alpha[K]), y = expression(alpha[L])) +
    theme_thesis()
save_thesis_plot(p, "ch06-pf-testinv-regions", width = THESIS_WIDTH, height = 5)
