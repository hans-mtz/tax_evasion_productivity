## Stage-2 ELVIS: 3D cube plot of the new 27-point (lambda,delta1,delta2)
## grid (2026-09-11, no-eta mechanism, grid3d mode, deliberately coarse
## +-1/+-0.2 spacing -- an evidentiary grid, not boundary-tracing, see
## CLAUDE.md/Research-log). Same manual persp()+trans3d() construction as
## 1259 (no 3D plotting package installed), axes = delta1, delta2,
## log10(lambda). SIMPLIFIED per the user's request, 2026-09-11: only the
## hard (conservative, Theorem F.1, absolute -- not min-subtracted) test now
## drives color -- more robust for guidance than the soft test, and a
## two-color scheme (blue=fails, pink=passes) reads faster than the earlier
## soft/hard/min three-way split. Ball size ~ p-value = 1-pchisq(TS_hard,9),
## bigger=more significant, so the global min naturally renders as the
## biggest pink ball without needing its own separate color.
## FIXED 2026-09-15: pass_hard now uses the standard .95 level, matching the
## chi2_{9,.95}=16.92 threshold used everywhere else in the write-up -- was
## erroneously using .99 (21.67), which passed 4/27 cells instead of the
## correct 1/27 (the global minimum itself).

df <- read.csv("Code/Products/1288-cube-lag_m-combined.csv")

n <- 32232; dg <- 9
qc95 <- qchisq(0.95, dg)
df$TS_hard <- 2 * n * df$Lhat
df$pval <- 1 - pchisq(df$TS_hard, dg)
df$pass_hard <- df$TS_hard <= qc95
df$logl <- log10(df$lambda)

wong_blue <- "#0072B2"; pink <- "#E6399B"

d1r <- range(df$delta1); d2r <- range(df$delta2); lr <- range(df$logl)
dummy_z <- matrix(mean(lr), 2, 2)

png("Paper/images/1289-cube-lag_m-3d.png", width = 2600, height = 2000, res = 220)
par(family = "Times")
res <- persp(d1r, d2r, dummy_z, zlim = lr, theta = -50, phi = 20, expand = 0.75,
             col = NA, border = NA, box = TRUE, ticktype = "detailed",
             xlab = "delta1", ylab = "delta2", zlab = "log10(lambda)",
             main = "Stage-2 ELVIS: 27-pt cube (lag_m, moment set A, no-eta)",
             sub = sprintf("Ball size ~ p-value of the HARD test (bigger=more significant). Pink=passes, blue=fails. Min at d1=%.2g d2=%.2g lambda=%.3g.",
                           df$delta1[which.min(df$Lhat)], df$delta2[which.min(df$Lhat)], df$lambda[which.min(df$Lhat)]))

## Draw smallest (least significant) balls first, largest (most significant) last, so they sit on top.
ord <- order(df$pval)
dfo <- df[ord, ]
pts <- trans3d(dfo$delta1, dfo$delta2, dfo$logl, res)

## Ball size: linear in p-value, mapped to a readable cex range.
cex_v <- 1.0 + 5.0 * dfo$pval

## Fill: pink = passes the hard test, blue = fails -- the global min passes
## by construction (its own TS_hard is the smallest possible) and is already
## the biggest ball, so no separate highlight color is needed for it.
fill_col <- ifelse(dfo$pass_hard, pink, wong_blue)

points(pts, pch = 21, cex = cex_v,
       bg = adjustcolor(fill_col, alpha.f = 0.8),
       col = "grey20", lwd = 0.6)

## Drop vertical guide lines from each ball down to the delta1-delta2 floor.
floor_z <- lr[1]
for (i in seq_len(nrow(dfo))) {
    seg <- trans3d(rep(dfo$delta1[i], 2), rep(dfo$delta2[i], 2), c(dfo$logl[i], floor_z), res)
    lines(seg, col = adjustcolor("grey60", alpha.f = 0.5), lty = 3)
}

legend("topright",
       legend = c("Passes hard test", "Fails hard test"),
       pt.bg = c(pink, wong_blue),
       col = "grey20", pch = 21, pt.cex = 1.6, bty = "n", cex = 0.9)
dev.off()
cat("Saved: Paper/images/1289-cube-lag_m-3d.png\n")
