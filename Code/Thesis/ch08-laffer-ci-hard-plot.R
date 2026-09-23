## PRODUCT: Thesis/figures/ch08-laffer-ci-hard.png := CV-adjusted R, HARD test, all 15
## Delta points, fully bisected -- the counterfactual's headline result figure.
## Sources the original Code/Deconvolution/1300-cv-15delta-hard-plot.R (unchanged, still
## writes its own Paper/images/ copy). Same DPI-tagging fix and same "preserve the
## original theme" rationale as ch08-trim-cutoff-plot.R.
source("Code/Thesis/001-setup.R")
source("Code/Deconvolution/1300-cv-15delta-hard-plot.R")

stopifnot(exists("p"))
# Thesis wording: "conservative" test, not "hard" (1300 left as-is; it also feeds the slides)
p <- p + ggplot2::labs(title = "CV-adjusted R, conservative test (Theorem F.1) — all 15 Δ points, fully bisected")
save_thesis_plot(p, "ch08-laffer-ci-hard", width = 11, height = 7.5)
cat("Saved: Thesis/figures/ch08-laffer-ci-hard.{png,pdf}\n")
