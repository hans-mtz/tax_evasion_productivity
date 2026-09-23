## Naive MSM cube (Tim Conley's test): center + 6 adjacent points around the
## MSM optimizer solution, delta0 profiled. Same style as 1289 (ELVIS cube):
## axes delta1, delta2, log10(lambda); ball size ~ p-value of J vs chi2_{d_g_eff};
## pink = passes at .95, blue = rejects.
plot_cube <- function(csv, png_file, ttl) {
df <- read.csv(csv)
df$pval <- 1 - pchisq(df$J, df$d_g_eff)
df$logl <- log10(df$lambda)
wong_blue <- "#0072B2"; pink <- "#E6399B"
d1r <- range(df$delta1) + c(-.1, .1); d2r <- range(df$delta2) + c(-.02, .02); lr <- range(df$logl) + c(-.02, .02)
png(png_file, width = 2600, height = 2000, res = 220)
par(family = "Times")
res <- persp(d1r, d2r, matrix(mean(lr), 2, 2), zlim = lr, theta = -50, phi = 20, expand = 0.9,
             col = NA, border = NA, box = TRUE, ticktype = "detailed",
             xlab = "delta1", ylab = "delta2", zlab = "log10(lambda)",
             main = ttl,
             sub = sprintf("Min J = %.0f vs chi2_{%d,.95} = %.2f; passes: %d of %d.",
                           min(df$J), df$d_g_eff[1], df$crit95[1], sum(!df$reject), nrow(df)))
dfo <- df[order(df$pval), ]
pts <- trans3d(dfo$delta1, dfo$delta2, dfo$logl, res)
points(pts, pch = 21, cex = 2.5 + 3 * dfo$pval, bg = adjustcolor(ifelse(dfo$reject, wong_blue, pink), .8), col = "grey20", lwd = .6)
for (i in seq_len(nrow(dfo))) lines(trans3d(rep(dfo$delta1[i], 2), rep(dfo$delta2[i], 2), c(dfo$logl[i], lr[1]), res),
                                    col = adjustcolor("grey60", .5), lty = 3)
legend("topright", legend = c("Passes (.95)", "Rejects"), pt.bg = c(pink, wong_blue), col = "grey20", pch = 21, pt.cex = 1.6, bty = "n")
dev.off()
}
plot_cube("Code/Products/1445-msm-cube-lag_m.csv", "Paper/images/1446-msm-cube-lag_m-3d.png",
          "Naive MSM: 7-pt cube around the MSM optimizer solution (lag_m, delta0 profiled)")
plot_cube("Code/Products/1445-msm-cube-elvis-lag_m.csv", "Paper/images/1446-msm-cube-elvis-lag_m-3d.png",
          "Naive MSM: 7-pt cube around the ELVIS point (lag_m, delta0 profiled)")
