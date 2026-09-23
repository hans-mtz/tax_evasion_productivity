## PRODUCT: Thesis/figures/ch08-trim-cutoff.png := stage-2 lambda grid, test-inversion
## pass/fail at every tested point, all trim levels.
## Sources the original Code/Deconvolution/1222-stage2-trim-all-gridpoints-plot.R
## (unchanged, still writes its own Paper/images/ copy as a side effect -- harmless) and
## re-saves the resulting plot object `p` through the standardized Thesis pipeline. Fixes
## the DPI-tagging bug (2026-09-22): the old Thesis/figures/ copy was untagged 72dpi
## (`identify` showed units=Undefined, 3600x1950 at nominal 72x72 -- a raw, unscaled ggsave
## default rather than a real 300dpi save), unlike figures already run through
## save_thesis_plot() (which correctly writes DPI into the PNG's own pHYs chunk via ggsave).
## Original figure's own theme (Times font, colorblind-safe palette, theme_minimal base) is
## deliberately preserved as-is -- already a coherent, polished style; not re-themed here.
source("Code/Thesis/001-setup.R")
source("Code/Deconvolution/1222-stage2-trim-all-gridpoints-plot.R")

stopifnot(exists("p"))
save_thesis_plot(p, "ch08-trim-cutoff", width = 12, height = 6.5)
cat("Saved: Thesis/figures/ch08-trim-cutoff.{png,pdf}\n")
