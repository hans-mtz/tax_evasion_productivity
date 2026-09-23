## PRODUCT: Thesis/figures/ch08-delta-grid.png := (delta1,delta2) deltagrid 3D surface
## (lag_m), grid minimum marked.
## Data-prep and drawing logic reproduced from Code/Deconvolution/1306-deltagrid-minonly-
## 3dplot.R (that script's own png()/dev.off() calls aren't reusable as a function, so the
## drawing code is wrapped here into a plot_fn for save_thesis_base_plot()'s dual-device
## pattern -- the original file is left untouched, still feeds Paper/images/ for slides).
## Fixes the same DPI-tagging bug as the other ch08 figures.
source("Code/Thesis/001-setup.R")

df <- read.csv(file.path(PRODUCTS_DIR, "1283-deltagrid-lag_m-combined.csv"))

d1s <- sort(unique(df$delta1))
d2s <- sort(unique(df$delta2))
Z <- matrix(NA, length(d1s), length(d2s))
for (i in seq_along(d1s)) for (j in seq_along(d2s)) {
    v <- df$Lhat[df$delta1 == d1s[i] & df$delta2 == d2s[j]]
    Z[i, j] <- if (length(v) == 1) v else NA
}

imin <- which(df$Lhat == min(df$Lhat))
d1_min <- df$delta1[imin]; d2_min <- df$delta2[imin]; L_min <- df$Lhat[imin]

wong_orange <- "#E69F00"

zrange <- range(Z, na.rm = TRUE)
zfloor <- zrange[1] - 0.25 * diff(zrange)
zlim <- c(zfloor, zrange[2])

plot_fn <- function() {
    par(family = "Times")
    res <- persp(d1s, d2s, Z, zlim = zlim, theta = -35, phi = 22, expand = 0.6, col = "lightgrey",
                 border = "grey30", ticktype = "detailed", shade = 0.4,
                 xlab = "delta1", ylab = "delta2", zlab = "L_n_hat",
                 main = "Grid Search over (delta1, delta2)",
                 sub = sprintf("Minimum at (delta1,delta2)=(%.2g,%.2g), L_n_hat=%.4g", d1_min, d2_min, L_min))
    line_pts <- trans3d(rep(d1_min, 2), rep(d2_min, 2), c(zfloor, L_min), res)
    lines(line_pts, col = wong_orange, lwd = 2, lty = "dashed")
    pt <- trans3d(d1_min, d2_min, L_min, res)
    points(pt, pch = 19, cex = 2, col = wong_orange)
    legend("topright", legend = "Grid minimum", col = wong_orange, pch = 19, bty = "n", cex = 1)
}

save_thesis_base_plot(plot_fn, "ch08-delta-grid", width = 2400/220, height = 1800/220)
cat("Saved: Thesis/figures/ch08-delta-grid.{png,pdf}\n")
cat(sprintf("Minimum: delta1=%.4g, delta2=%.4g, Lhat=%.6g\n", d1_min, d2_min, L_min))
