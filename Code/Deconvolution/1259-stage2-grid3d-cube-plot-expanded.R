## Stage-2 ELVIS: 3D cube plot of the EXPANDED (lambda,delta1,delta2) grid --
## 65 points (original 27 + 38-point outer-cube expansion, 2026-09-09/10):
## delta1 in {3.5,4.2,5.0,5.8}, delta2 in {0.4..0.7}, lambda in
## {1.3e-7,3e-7,7e-7,1.5e-6} -- not a full regular 4x4x4 cube (the original
## 27 only cover the inner 3x3x3), just the union of tested points. Same
## manual persp()+trans3d() construction as 1257 (no 3D plotting package
## installed), axes = the three parameters, ball size = Lhat (bigger =
## worse), color = pass/fail chi2_9 test-inversion against THIS grid's own
## (lower) minimum.

df <- read.csv("Code/Products/1258-grid3d-lag_m-combined-full.csv")

qc <- qchisq(0.95, 9); n <- 32232
Lmin <- min(df$Lhat); thr <- Lmin + qc / (2 * n)
df$pass <- df$Lhat <= thr
df$logl <- log10(df$lambda)

wong_red <- "#D55E00"; wong_blue <- "#0072B2"

## Dummy flat surface purely to set up the persp() 3D transform/axes -- its
## own z-values are irrelevant (zlim overrides), it's never drawn (border=NA).
d1r <- range(df$delta1); d2r <- range(df$delta2); lr <- range(df$logl)
dummy_z <- matrix(mean(lr), 2, 2)

png("Paper/images/1259-stage2-grid3d-cube-expanded.png", width = 2600, height = 2000, res = 220)
par(family = "Times")
res <- persp(d1r, d2r, dummy_z, zlim = lr, theta = -50, phi = 20, expand = 0.75,
             col = NA, border = NA, box = TRUE, ticktype = "detailed",
             xlab = "delta1", ylab = "delta2", zlab = "log10(lambda)",
             main = "Stage-2 ELVIS: expanded 3D grid (65 pts), lag_m, moment set A, Nelder-Mead",
             sub = sprintf("Ball size ~ Lhat (bigger=worse). Threshold=%.5g (chi2_9,.95). n_keep=3000. Min at d1=%.2g d2=%.2g log10(lam)=%.2g.",
                           thr, df$delta1[which.min(df$Lhat)], df$delta2[which.min(df$Lhat)], df$logl[which.min(df$Lhat)]))

## Sort so smaller (better/passing, typically) balls draw last / on top where
## they overlap larger ones -- draw worst (largest) first, best (smallest) last.
ord <- order(-df$Lhat)
dfo <- df[ord, ]
pts <- trans3d(dfo$delta1, dfo$delta2, dfo$logl, res)

## Ball size: linear in Lhat, mapped to a readable cex range.
cex_v <- 1.0 + 4.5 * (dfo$Lhat - min(df$Lhat)) / (max(df$Lhat) - min(df$Lhat))

points(pts, pch = 21, cex = cex_v,
       bg = adjustcolor(ifelse(dfo$pass, wong_red, wong_blue), alpha.f = 0.75),
       col = "grey20", lwd = 0.6)

## Drop vertical guide lines from each ball down to the delta1-delta2 floor
## (at the min log-lambda), to make the 3D position easier to read.
floor_z <- lr[1]
for (i in seq_len(nrow(dfo))) {
    seg <- trans3d(rep(dfo$delta1[i], 2), rep(dfo$delta2[i], 2), c(dfo$logl[i], floor_z), res)
    lines(seg, col = adjustcolor("grey60", alpha.f = 0.5), lty = 3)
}

legend("topright", legend = c("Inside 95% CI (pass)", "Outside (rejected)"),
       pt.bg = c(wong_red, wong_blue), pch = 21, pt.cex = 1.6, bty = "n", cex = 0.9)
dev.off()
cat("Saved: Paper/images/1259-stage2-grid3d-cube-expanded.png\n")
