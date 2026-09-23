## PRODUCT: Thesis/figures/ch08-elvis-cube.png := 3D cube plot of the 27-point
## (lambda,delta1,delta2) grid (lag_m, no-eta, grid3d mode).
## Data-prep and drawing logic reproduced from Code/Deconvolution/1289-cube-3dplot.R (same
## rationale as ch08-delta-grid-plot.R -- original file's png()/dev.off() aren't reusable
## as a function, drawing code wrapped here into a plot_fn; original left untouched).
## Fixes the same DPI-tagging bug as the other ch08 figures.
source("Code/Thesis/001-setup.R")

df <- read.csv(file.path(PRODUCTS_DIR, "1288-cube-lag_m-combined.csv"))

n <- 32232; dg <- 9
qc95 <- qchisq(0.95, dg)
df$TS_hard <- 2 * n * df$Lhat
df$pval <- 1 - pchisq(df$TS_hard, dg)
df$pass_hard <- df$TS_hard <= qc95
df$logl <- log10(df$lambda)

wong_blue <- "#0072B2"; pink <- "#E6399B"

d1r <- range(df$delta1); d2r <- range(df$delta2); lr <- range(df$logl)
dummy_z <- matrix(mean(lr), 2, 2)

plot_fn <- function() {
    par(family = "Times")
    res <- persp(d1r, d2r, dummy_z, zlim = lr, theta = -50, phi = 20, expand = 0.75,
                 col = NA, border = NA, box = TRUE, ticktype = "detailed",
                 xlab = "delta1", ylab = "delta2", zlab = "log10(lambda)",
                 main = "Stage-2 ELVIS: 27-pt cube (lag_m, moment set A, no-eta)",
                 sub = sprintf("Ball size ~ p-value of the conservative test (bigger=more significant). Pink=passes, blue=fails. Min at d1=%.2g d2=%.2g lambda=%.3g.",
                               df$delta1[which.min(df$Lhat)], df$delta2[which.min(df$Lhat)], df$lambda[which.min(df$Lhat)]))

    ord <- order(df$pval)
    dfo <- df[ord, ]
    pts <- trans3d(dfo$delta1, dfo$delta2, dfo$logl, res)
    cex_v <- 1.0 + 5.0 * dfo$pval
    fill_col <- ifelse(dfo$pass_hard, pink, wong_blue)
    points(pts, pch = 21, cex = cex_v,
           bg = adjustcolor(fill_col, alpha.f = 0.8),
           col = "grey20", lwd = 0.6)

    floor_z <- lr[1]
    for (i in seq_len(nrow(dfo))) {
        seg <- trans3d(rep(dfo$delta1[i], 2), rep(dfo$delta2[i], 2), c(dfo$logl[i], floor_z), res)
        lines(seg, col = adjustcolor("grey60", alpha.f = 0.5), lty = 3)
    }

    legend("topright",
           legend = c("Passes conservative test", "Fails conservative test"),
           pt.bg = c(pink, wong_blue),
           col = "grey20", pch = 21, pt.cex = 1.6, bty = "n", cex = 0.9)
}

save_thesis_base_plot(plot_fn, "ch08-elvis-cube", width = 2600/220, height = 2000/220)
cat("Saved: Thesis/figures/ch08-elvis-cube.{png,pdf}\n")
