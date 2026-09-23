## PRODUCT: Thesis/figures/ch08-lambda-grid.png := L_n_hat(lambda) profile, both
## instruments, grid minimum marked.
## Sources the original Code/Deconvolution/1305-lambdagrid-minonly-plot.R (unchanged,
## still writes its own Paper/images/ copy). Same DPI-tagging fix and same "preserve the
## original theme" rationale as ch08-trim-cutoff-plot.R.
source("Code/Thesis/001-setup.R")
source("Code/Deconvolution/1305-lambdagrid-minonly-plot.R")

stopifnot(exists("p"))
save_thesis_plot(p, "ch08-lambda-grid", width = 10, height = 5.5)
cat("Saved: Thesis/figures/ch08-lambda-grid.{png,pdf}\n")
