## Slide version of the (delta1,delta2) deltagrid 3D surface (lag_m,
## lambda free at each point), stripped of the chi-sq threshold plane and
## pass/fail coloring that 1287 has -- just the L_n_hat(delta1,delta2)
## surface with the grid's own minimum marked. For the "$\delta$ Search"
## slide in 650-stage2-prelim-results.qmd. 2026-09-17.

df <- read.csv("Code/Products/1283-deltagrid-lag_m-combined.csv")

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

## Extend zlim below the surface's own range so a "flagpole" dropped from the
## minimum down to the floor is actually visible, not zero-length (the
## minimum point sits ON the surface, i.e. at its own lowest height).
zrange <- range(Z, na.rm = TRUE)
zfloor <- zrange[1] - 0.25 * diff(zrange)
zlim <- c(zfloor, zrange[2])

png("Paper/images/1306-deltagrid-lag_m-3d-minonly.png", width = 2400, height = 1800, res = 220)
par(family = "Times")
res <- persp(d1s, d2s, Z, zlim = zlim, theta = -35, phi = 22, expand = 0.6, col = "lightgrey",
             border = "grey30", ticktype = "detailed", shade = 0.4,
             xlab = "delta1", ylab = "delta2", zlab = "L_n_hat",
             main = "Grid Search over (delta1, delta2)",
             sub = sprintf("Minimum at (delta1,delta2)=(%.2g,%.2g), L_n_hat=%.4g", d1_min, d2_min, L_min))

## Vertical drop line ("flagpole") from the minimum down to the floor, then the point itself
line_pts <- trans3d(rep(d1_min, 2), rep(d2_min, 2), c(zfloor, L_min), res)
lines(line_pts, col = wong_orange, lwd = 2, lty = "dashed")

pt <- trans3d(d1_min, d2_min, L_min, res)
points(pt, pch = 19, cex = 2, col = wong_orange)

legend("topright", legend = "Grid minimum", col = wong_orange, pch = 19, bty = "n", cex = 1)
dev.off()
cat("Saved: Paper/images/1306-deltagrid-lag_m-3d-minonly.png\n")
cat(sprintf("Minimum: delta1=%.4g, delta2=%.4g, Lhat=%.6g\n", d1_min, d2_min, L_min))
