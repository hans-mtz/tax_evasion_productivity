## Stage-2 ELVIS: (delta1,delta2) grid, 3D CUE-objective surface + chi-sq
## test-inversion, moment set A (9 rows), no-eta mechanism, lag_m, lambda
## FREE at each point (deltagrid mode, 2026-09-11). Same persp() surface +
## translucent threshold plane construction as 1241 (that one was the old
## eta-era 7x7 grid); this one is the new 5x5=25-point grid built to check
## cube spacing.

df <- read.csv("Code/Products/1283-deltagrid-lag_m-combined.csv")

qc <- qchisq(0.95, 9); n <- 32232
Lmin <- min(df$Lhat)
thr <- Lmin + qc / (2 * n)

d1s <- sort(unique(df$delta1))
d2s <- sort(unique(df$delta2))
Z <- matrix(NA, length(d1s), length(d2s))
for (i in seq_along(d1s)) for (j in seq_along(d2s)) {
    v <- df$Lhat[df$delta1 == d1s[i] & df$delta2 == d2s[j]]
    Z[i, j] <- if (length(v) == 1) v else NA
}

wong_blue <- "#0072B2"; wong_red <- "#D55E00"

png("Paper/images/1287-deltagrid-lag_m-3d.png", width = 2400, height = 1800, res = 220)
par(family = "Times")
res <- persp(d1s, d2s, Z, theta = -35, phi = 22, expand = 0.6, col = "lightgrey",
             border = "grey30", ticktype = "detailed", shade = 0.4,
             xlab = "delta1", ylab = "delta2", zlab = "L_n_hat",
             main = "Stage-2 ELVIS: L_n(delta1,delta2), lag_m, moment set A (9 rows), no-eta, lambda free",
             sub = sprintf("Threshold plane: Lhat_min+chi2_{9,.95}/(2n)=%.5g. Red=inside 95%% CI, blue=outside.", thr))

## Threshold plane (semi-transparent), drawn as 4 corner points of the (d1,d2) box at z=thr
poly_pts <- trans3d(c(min(d1s), max(d1s), max(d1s), min(d1s)),
                     c(min(d2s), min(d2s), max(d2s), max(d2s)), thr, res)
polygon(poly_pts, col = adjustcolor("grey40", alpha.f = 0.25), border = NA)

## Overlay every actual fitted point, colored pass/fail
pts <- trans3d(df$delta1, df$delta2, df$Lhat, res)
points(pts, pch = 19, cex = 1.1, col = ifelse(df$Lhat <= thr, wong_red, wong_blue))

legend("topright", legend = c("Inside 95% CI (fail to reject)", "Outside (rejected)"),
       col = c(wong_red, wong_blue), pch = 19, bty = "n", cex = 0.9)
dev.off()
cat("Saved: Paper/images/1287-deltagrid-lag_m-3d.png\n")
