## PRODUCT: Thesis/figures/ch08-lambda-grid.png := L_n_hat(lambda) profile, both
## instruments, grid minimum marked.
## Sources the original Code/Deconvolution/1305-lambdagrid-minonly-plot.R (unchanged,
## still writes its own Paper/images/ copy). Same DPI-tagging fix and same "preserve the
## original theme" rationale as ch08-trim-cutoff-plot.R.
source("Code/Thesis/001-setup.R")
source("Code/Deconvolution/1305-lambdagrid-minonly-plot.R")

stopifnot(exists("p"))
# House style: grey path, grid minimum in slot 1, thesis font, no in-image title.
# Instrument 2 is lag_2_cal_W, the UNtilded W_{t-2} (the old label carried a tilde by mistake).
p$data$instrument <- factor(p$data$instrument, labels = c("Instrument~m[t-1]^'*'", "Instrument~italic(W)[t-2]"))
p$layers[[3]]$data$instrument <- factor(p$layers[[3]]$data$instrument, labels = levels(p$data$instrument))
p$layers[[4]]$data$instrument <- factor(p$layers[[4]]$data$instrument, labels = levels(p$data$instrument))
p$layers[[3]]$aes_params$colour <- THESIS_COLS[1]; p$layers[[3]]$aes_params$size <- 3
p$layers[[4]]$aes_params$colour <- THESIS_COLS[1]; p$layers[[4]]$aes_params$family <- THESIS_FONT
p$layers[[4]]$aes_params$fontface <- "plain"; p$layers[[4]]$aes_params$size <- 3.3
p$layers[[4]]$aes_params$vjust <- -1.4
p$layers[[4]]$data$label <- sprintf("%.2g", p$layers[[4]]$data$lambda)
p <- p + labs(title = NULL, subtitle = NULL,
              x = expression(lambda~"(log scale)"), y = expression(hat(L)[n]~"(log scale)")) +
    facet_wrap(~instrument, labeller = label_parsed) +
    theme_thesis() + theme(panel.grid.major.x = element_blank())
save_thesis_plot(p, "ch08-lambda-grid", width = THESIS_WIDTH, height = 3.6)
cat("Saved: Thesis/figures/ch08-lambda-grid.{png,pdf}\n")
