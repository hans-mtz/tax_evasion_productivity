# 2026-09-18: theory-coefficient (row9_mode=theory, R_i - t1_i) in its OWN
# native units -- no +mu_c shift back to "real R" terms. This is literally
# -tau_tilde*(M_i+(1-q(e'))e')/pgdp, the (negative) purchases-side credit
# paid out -- should trend toward 0 as Delta -> -1 (tau_tilde -> 0, nothing
# left to credit) and grow more negative as Delta rises, mirroring the
# well-documented revenue-curve shape (just shifted down by mu_c, no other
# change -- a pure vertical translation, so the SHAPE is identical to
# 1307's theory series, only the axis differs).

products <- "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Code/Products"
images   <- "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Paper/images"

chi99 <- 23.209

theory_files <- c(
  "1306-theory-coarse-delta0.csv", "1306-theory-coarse-delta1.csv",
  "1306-theory-coarse-delta2.csv", "1306-theory-coarse-delta3.csv",
  "1306-theory-anchor.csv", "1306-theory-remaining-delta0.csv",
  "1306-theory-remaining-deltam08.csv", "1306-theory-remaining-deltap005.csv",
  "1306-theory-remaining-deltap01.csv", "1306-theory-remaining-deltap02.csv"
)
theory <- do.call(rbind, lapply(theory_files, function(f) read.csv(file.path(products, f))))
theory$Delta <- round(theory$Delta, 4)
theory$TS    <- 2 * theory$n * theory$Lhat
theory$pass99 <- theory$TS < chi99   # R is already in native (R_i - t1_i) units, no shift

delta_levels <- sort(unique(theory$Delta))
delta_labels <- paste0(ifelse(delta_levels >= 0, "+", ""), delta_levels * 100, "%")
theory$xpos <- match(theory$Delta, delta_levels)

bounds <- do.call(rbind, lapply(split(theory, theory$Delta), function(sub) {
  p <- sub[sub$pass99, ]
  if (nrow(p) == 0) return(NULL)
  data.frame(Delta = sub$Delta[1], lo = min(p$R), hi = max(p$R))
}))
bounds$xpos <- match(bounds$Delta, delta_levels)
bounds <- bounds[order(bounds$xpos), ]
bounds$mid <- (bounds$lo + bounds$hi) / 2

png(file.path(images, "1308-theory-raw.png"), width = 2400, height = 1500, res = 220)
par(mar = c(5, 6, 3, 1))
# ylim[1] maps to the BOTTOM of the plot, ylim[2] to the TOP -- we want 0 at
# the top (the Delta->-1 "no credit paid" limit) and the most negative value
# at the bottom, which is just the natural ascending order c(min, 0), NOT a
# reversal (caught and fixed 2026-09-18: an earlier rev() put 0 at the
# bottom instead).
plot(NA, xlim = c(0.5, length(delta_levels) + 0.5), ylim = c(min(theory$R, na.rm = TRUE) * 1.05, 0),
     xaxt = "n", xlab = expression(Delta~"(purchases-tax rate shock)"),
     ylab = expression("Theory-coefficient target, native units:  "*R[i]-t1[i]*"  (COP)"),
     main = expression("Native-units check: "*R[i](Delta)-t1[i]*"  =  "*-tau[P]*"["*M[i]+(1-q(e*minute))*e*minute*"]"))
axis(1, at = seq_along(delta_levels), labels = delta_labels)
abline(h = 0, col = "grey50", lty = 1, lwd = 1.5)
grid(nx = NA, ny = NULL, col = "grey90")
abline(v = seq_along(delta_levels), col = "grey95")

col_pass <- "#c0392b"; col_fail <- "#e3b0a5"
with(theory[theory$pass99, ],  points(xpos, R, pch = 17, col = col_pass, cex = 1.3))
with(theory[!theory$pass99, ], points(xpos, R, pch = 4,  col = col_fail, cex = 1.1, lwd = 2))
segments(bounds$xpos, bounds$lo, bounds$xpos, bounds$hi, col = col_pass, lwd = 2)
lines(bounds$xpos, bounds$mid, col = col_pass, lwd = 1.5, lty = 2)

legend("bottomleft", bty = "n", cex = 0.85,
       legend = c("passes (99%)", "rejects", "passing bound", "0 = no credit paid (Δ→-1 limit)"),
       pch = c(17, 4, NA, NA), lty = c(NA, NA, 1, 1), lwd = c(NA, 2, 2, 1.5),
       col = c(col_pass, col_fail, col_pass, "grey50"))
dev.off()
cat("Saved:", file.path(images, "1308-theory-raw.png"), "\n")
