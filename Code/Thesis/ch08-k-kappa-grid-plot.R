## PRODUCT: Thesis/figures/ch08-k-kappa-grid.png := conservative test statistic over the detection parameters (k, kappa),
## 3D surfaces in the style of ch08-delta-grid: left, the coarse grid (1616: k in {0.65, 0.70, 0.75} x kappa in
## {0.5, 4.5, 9}); right, the fine grid around the operating point (1619: k in {0.725, 0.75, 0.775} x kappa in
## {0.4, 0.5, 0.6}; centre (0.75, 0.5) from 1616). Each cell is its own fit (delta0-2 and gamma free, (k, kappa) pinned,
## same start for every cell, seed 30, design i, 0.5% trim); height = TS_cons = 2 n Lhat from the fit's own adiag at
## R = 1000. Translucent plane = chi2_{17,.95}; blue dots pass, grey crosses are rejected; missing cells are left open.
source("Code/Thesis/001-setup.R")

ts_of <- function(run, k, ka) {
    f <- file.path(PRODUCTS_DIR, sprintf("%s-i-k%s-kappa%s-adiag-R1000.txt", run, k, ka))
    if (!file.exists(f)) return(NA_real_)
    l <- grep("^Lhat \\(recomputed\\)", readLines(f), value = TRUE)[1]
    if (is.na(l)) NA_real_ else as.numeric(sub(".*TS = 2 n Lhat = ([0-9.]+).*", "\\1", l))
}
grid_of <- function(ks, kas, run_of) {
    d <- expand.grid(k = ks, kappa = kas, stringsAsFactors = FALSE)
    d$TS <- mapply(function(k, ka) ts_of(run_of(k, ka), k, ka), d$k, d$kappa)
    d
}
coarse <- grid_of(c("0.65", "0.7", "0.75"), c("0.5", "4.5", "9"), function(k, ka) "1616")
fine <- grid_of(c("0.725", "0.75", "0.775"), c("0.4", "0.5", "0.6"),
                function(k, ka) if (k == "0.75" && ka == "0.5") "1616" else "1619")
crit <- qchisq(0.95, 17)
print(coarse); print(fine)
write.csv(rbind(cbind(grid = "coarse", coarse), cbind(grid = "fine", fine)),
          file.path(PRODUCTS_DIR, "ch08-k-kappa-grid.csv"), row.names = FALSE)

panel <- function(d, theta = -35) {
    ks <- sort(unique(as.numeric(d$k))); kas <- sort(unique(as.numeric(d$kappa)))
    Z <- matrix(NA, length(ks), length(kas))
    for (i in seq_along(ks)) for (j in seq_along(kas))
        Z[i, j] <- d$TS[as.numeric(d$k) == ks[i] & as.numeric(d$kappa) == kas[j]]
    zr <- range(c(Z, crit), na.rm = TRUE); zfloor <- zr[1] - 0.15 * diff(zr)
    res <- persp(ks, kas, Z, zlim = c(zfloor, zr[2]), theta = theta, phi = 22, expand = 0.65, col = "grey90",
                 border = "grey50", ticktype = "detailed", shade = 0.35, nticks = 3,
                 xlab = "k", ylab = "κ", zlab = "TS", cex.lab = 1.1, cex.axis = 0.75)
    pl <- trans3d(c(ks[1], ks[length(ks)], ks[length(ks)], ks[1]), c(kas[1], kas[1], kas[length(kas)], kas[length(kas)]),
                  rep(crit, 4), res)
    polygon(pl, col = adjustcolor(THESIS_COLS[2], 0.18), border = THESIS_COLS[2], lty = "dashed")
    ok <- !is.na(d$TS)
    p <- trans3d(as.numeric(d$k[ok]), as.numeric(d$kappa[ok]), d$TS[ok], res)
    pass <- d$TS[ok] < crit
    points(p$x[!pass], p$y[!pass], pch = 4, cex = 1.3, lwd = 2, col = THESIS_REJECT)
    points(p$x[pass], p$y[pass], pch = 19, cex = 1.5, col = THESIS_COLS[1])
    im <- which.min(d$TS)
    lines(trans3d(rep(as.numeric(d$k[im]), 2), rep(as.numeric(d$kappa[im]), 2), c(zfloor, d$TS[im]), res),
          col = THESIS_COLS[1], lwd = 1.5, lty = "dashed")
}

plot_fn <- function() {
    par(mfrow = c(1, 2), mar = c(1, 1, 1, 0.5))
    panel(coarse); panel(fine)
    par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), new = TRUE)
    plot.new()
    legend("bottom", horiz = TRUE, bty = "n", cex = 0.85, inset = 0.01,
           legend = c("Not rejected", "Rejected", sprintf("χ²(17) critical value, %.2f", crit)),
           pch = c(19, 4, 15), col = c(THESIS_COLS[1], THESIS_REJECT, adjustcolor(THESIS_COLS[2], 0.35)), pt.cex = c(1.3, 1.2, 1.8))
}

save_thesis_base_plot(plot_fn, "ch08-k-kappa-grid", width = 10, height = 5)
cat("Saved: Thesis/figures/ch08-k-kappa-grid.{png,pdf}\n")
