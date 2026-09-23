# 2026-09-18: theory-coefficient (beta=1, row9_mode=theory) vs CV-R (beta_hat
# fitted) comparison plot -- shows exactly what's being compared: individual
# tested candidate revenue levels (pass/reject) for both systems, at the same
# Deltas, on the same real-revenue y-axis, to make the "nests inside" claim
# visually checkable rather than just a table.
library(tools)

products <- "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Code/Products"
images   <- "/Volumes/SSD Hans/Github/Tax_Evasion_Productivity/Paper/images"

chi95 <- 18.307
chi99 <- 23.209
mu_c  <- 2507.602596

## ---- theory-coefficient points (all in "R - t1/pgdp" units; convert to real R) ----
theory_files <- c(
  "1306-theory-coarse-delta0.csv",  # -0.05
  "1306-theory-coarse-delta1.csv",  # -0.04
  "1306-theory-coarse-delta2.csv",  # -0.03
  "1306-theory-coarse-delta3.csv",  # -0.02
  "1306-theory-anchor.csv",         # 0 (k=0 candidate)
  "1306-theory-remaining-delta0.csv",# 0 (other 4)
  "1306-theory-remaining-deltam08.csv",
  "1306-theory-remaining-deltap005.csv",
  "1306-theory-remaining-deltap01.csv",
  "1306-theory-remaining-deltap02.csv"
)
theory <- do.call(rbind, lapply(theory_files, function(f) read.csv(file.path(products, f))))
theory$Delta <- round(theory$Delta, 4)
theory$TS    <- 2 * theory$n * theory$Lhat
theory$R_real <- theory$R + mu_c   # convert theory-mode target back to real R
theory$pass99 <- theory$TS < chi99
theory$system <- "theory (beta=1, no fitting)"

## ---- CV-R's own points, same Deltas, already has TS_hard/pass_hard ----
cv <- read.csv(file.path(products, "1300-cv-16delta-final.csv"))
target_deltas <- c(-0.08, -0.05, -0.04, -0.03, -0.02, 0, 0.005, 0.01, 0.02)
cv <- cv[round(cv$Delta, 4) %in% target_deltas, ]
cv$R_real <- cv$R
cv$pass99 <- cv$pass_hard == "TRUE" | cv$TS_hard < chi99
cv$system <- "CV-R (beta_hat fitted)"

## ---- combine for plotting ----
pts <- data.frame(
  Delta  = c(theory$Delta, cv$Delta),
  R_real = c(theory$R_real, cv$R_real),
  pass   = c(theory$pass99, cv$pass99),
  system = c(theory$system, cv$system)
)

## discrete x positions, ordered
delta_levels <- sort(unique(pts$Delta))
delta_labels <- paste0(ifelse(delta_levels >= 0, "+", ""), delta_levels * 100, "%")
pts$xpos <- match(pts$Delta, delta_levels)
# small horizontal jitter so the two systems don't overplot
pts$xpos <- pts$xpos + ifelse(pts$system == "CV-R (beta_hat fitted)", 0.15, -0.15)

## bounds per system per Delta (min/max passing point), for connecting lines
bounds <- function(df) {
  do.call(rbind, lapply(split(df, df$Delta), function(sub) {
    p <- sub[sub$pass, ]
    if (nrow(p) == 0) return(NULL)
    data.frame(Delta = sub$Delta[1], lo = min(p$R_real), hi = max(p$R_real))
  }))
}
theory_b <- bounds(data.frame(Delta = theory$Delta, R_real = theory$R_real, pass = theory$pass99))
cv_b     <- bounds(data.frame(Delta = cv$Delta, R_real = cv$R_real, pass = cv$pass99))
theory_b$xpos <- match(theory_b$Delta, delta_levels) - 0.15
cv_b$xpos     <- match(cv_b$Delta, delta_levels) + 0.15

png(file.path(images, "1307-theory-vs-cv.png"), width = 2400, height = 1500, res = 220)
par(mar = c(5, 5.5, 3, 1))
plot(NA, xlim = c(0.5, length(delta_levels) + 0.5), ylim = range(pts$R_real, na.rm = TRUE) * c(0.95, 1.05),
     xaxt = "n", xlab = expression(Delta~"(purchases-tax rate shock)"),
     ylab = "Real mean revenue per firm-period (COP)",
     main = "Theory-coefficient (β=1) vs. CV-R (β̂ fitted): tested candidates and their own bounds")
axis(1, at = seq_along(delta_levels), labels = delta_labels)
grid(nx = NA, ny = NULL, col = "grey90")
abline(v = seq_along(delta_levels), col = "grey95")

col_cv_pass    <- "#1b6ca8"; col_cv_fail    <- "#a9c6de"
col_th_pass    <- "#c0392b"; col_th_fail    <- "#e3b0a5"

with(pts[pts$system == "CV-R (beta_hat fitted)" & pts$pass, ],  points(xpos, R_real, pch = 16, col = col_cv_pass, cex = 1.3))
with(pts[pts$system == "CV-R (beta_hat fitted)" & !pts$pass, ], points(xpos, R_real, pch = 4,  col = col_cv_fail, cex = 1.1, lwd = 2))
with(pts[pts$system != "CV-R (beta_hat fitted)" & pts$pass, ],  points(xpos, R_real, pch = 17, col = col_th_pass, cex = 1.3))
with(pts[pts$system != "CV-R (beta_hat fitted)" & !pts$pass, ], points(xpos, R_real, pch = 4,  col = col_th_fail, cex = 1.1, lwd = 2))

segments(cv_b$xpos, cv_b$lo, cv_b$xpos, cv_b$hi, col = col_cv_pass, lwd = 2)
segments(theory_b$xpos, theory_b$lo, theory_b$xpos, theory_b$hi, col = col_th_pass, lwd = 2, lty = 2)

legend("topright", bty = "n", cex = 0.85,
       legend = c("CV-R candidate: passes (99%)", "CV-R candidate: rejects",
                  "theory candidate: passes (99%)", "theory candidate: rejects",
                  "CV-R's own bound (solid)", "theory's own bound (dashed)"),
       pch = c(16, 4, 17, 4, NA, NA), lty = c(NA, NA, NA, NA, 1, 2),
       col = c(col_cv_pass, col_cv_fail, col_th_pass, col_th_fail, col_cv_pass, col_th_pass),
       pt.cex = c(1.2, 1, 1.2, 1, NA, NA), lwd = c(NA, 2, NA, 2, 2, 2))
dev.off()
cat("Saved:", file.path(images, "1307-theory-vs-cv.png"), "\n")
