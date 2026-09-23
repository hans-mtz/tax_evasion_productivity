## PRODUCT: Thesis/figures/ch08-laffer-ci-hard.png := CV-adjusted R, HARD test, all 15
## Delta points, fully bisected -- the counterfactual's headline result figure.
## Sources the original Code/Deconvolution/1300-cv-15delta-hard-plot.R (unchanged, still
## writes its own Paper/images/ copy). Same DPI-tagging fix and same "preserve the
## original theme" rationale as ch08-trim-cutoff-plot.R.
source("Code/Thesis/001-setup.R")
source("Code/Deconvolution/1300-cv-15delta-hard-plot.R")

stopifnot(exists("p"))
# Thesis wording: "conservative" test, not "hard" (1300 left as-is; it also feeds the slides)
p <- p + ggplot2::labs(title = "CV-adjusted R, conservative test (Theorem F.1) — all 15 Δ points, fully bisected",
                       x = expression(paste("Tax-rate shifter ", Delta)),
                       caption = "Δ = −8% is the smallest tax cut statistically distinguishable from Δ = 0. Cuts of −1% to −7% still overlap the Δ = 0 band; increases (≥ +0.5%) separate cleanly.") +
    ggplot2::theme(text = ggplot2::element_text(family = "Times", size = 16),
                   plot.title = ggplot2::element_text(family = "Times", size = 17, face = "bold", hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(family = "Times", size = 12.5, hjust = 0.5, color = "grey30"),
                   plot.caption = ggplot2::element_text(family = "Times", size = 11, color = "grey40", hjust = 0),
                   axis.text = ggplot2::element_text(size = 13), legend.text = ggplot2::element_text(size = 14))
save_thesis_plot(p, "ch08-laffer-ci-hard", width = 11, height = 7.5)
cat("Saved: Thesis/figures/ch08-laffer-ci-hard.{png,pdf}\n")
