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
# House style: not rejected in slot 1, rejected as grey crosses, grid minimum ringed in dark grey.
relab <- function(x) factor(ifelse(grepl("^m", x), "Instrument~m[t-1]^'*'", "Instrument~italic(W)[t-2]"),
                            levels = c("Instrument~m[t-1]^'*'", "Instrument~italic(W)[t-2]"))
p$data$panel <- relab(p$data$panel); p$layers[[2]]$data$panel <- relab(p$layers[[2]]$data$panel)
p$layers[[2]]$aes_params$colour <- "grey20"; p$layers[[2]]$aes_params$size <- 3
p$layers[[1]]$aes_params$size <- 1.6; p$layers[[1]]$aes_params$alpha <- 1
p <- p + scale_color_manual(values = c("Rejected" = THESIS_REJECT, "Fail to reject (95% CI)" = THESIS_COLS[1]),
                            labels = c("Rejected" = "Rejected", "Fail to reject (95% CI)" = "Not rejected")) +
    scale_shape_manual(values = c("Rejected" = 4, "Fail to reject (95% CI)" = 16),
                       labels = c("Rejected" = "Rejected", "Fail to reject (95% CI)" = "Not rejected")) +
    labs(title = NULL, subtitle = NULL, x = "Share of largest non-corner firms dropped (by reported materials)") +
    facet_wrap(~panel, nrow = 1, labeller = label_parsed) +
    theme_thesis() + theme(panel.grid.major.x = element_blank())
save_thesis_plot(p, "ch08-trim-cutoff", width = THESIS_WIDTH, height = 3.8)
cat("Saved: Thesis/figures/ch08-trim-cutoff.{png,pdf}\n")
