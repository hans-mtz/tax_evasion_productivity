## Forward simulation of the evasion FOC at fitted theta (log 2026-09-28): ln tau + ln B(x) = C(omega) + psi,
## B(x) = 1 - l1 + l1*exp(-x)(1-x), x = e/Mbar in [0,1), C = d0 - d1*om + d2*om^2. Fits: 1550 (tax row, row6 = eps*psi).
## omega at M = M* (u = 0): om = W~ + (1-beta)*V. psi = 0, or N(0, s^2) (f_psi is not estimated by ELVIS; illustrative).
## Reports: corner (required ln B >= 0 -> e = 0), interior, saturated (required ln B below ln B(1) = ln(1-l1) -> x -> 1),
## implied u = ln(M*/(M*-e)), and the share of Var(h), h = ln tau + ln B(x*), due to the evasion part ln B.
d <- read.csv("Code/Products/1546-stage2-input-designA-tau-trim0.005.csv"); d <- d[d$corner == 0, ]
fit <- read.csv("Code/Products/1550-s3tau-psi-coarse-designA.csv")
lt <- log(d$sales_tax_rate_purchases); om <- d$tilde_cal_W + (1 - d$beta) * d$cal_V
Bf <- function(x, l) 1 - l + l * exp(-x) * (1 - x)
cat(sprintf("interior firms %d | sd ln tau %.3f | data: mean V %.3f, sd V %.3f | sd omega(M*) %.3f\n",
            nrow(d), sd(lt), mean(d$cal_V), sd(d$cal_V), sd(om)))
set.seed(20260928)
for (l1 in c(0.1, 0.9)) {
  f <- fit[abs(fit$lambda - l1) < 1e-9, ]
  C <- f$delta0_hat - f$delta1_hat * om + f$delta2_hat * om^2
  cat(sprintf("\n== lambda1 = %.1f: delta = (%.3f, %.3f, %.3f); attainable ln B in [%.3f, 0]\n", l1, f$delta0_hat, f$delta1_hat, f$delta2_hat, log(1 - l1)))
  for (s in c(0, 0.5, 1, 2)) {
    req <- C + rnorm(nrow(d), 0, s) - lt                     # required ln B(x*)
    corner <- req >= 0; sat <- req <= log(1 - l1); int <- !corner & !sat
    x <- numeric(nrow(d)); x[sat] <- 1 - 1e-9
    x[int] <- vapply(which(int), \(i) uniroot(\(z) log(Bf(z, l1)) - req[i], c(0, 1 - 1e-12))$root, 0)
    lnB <- log(Bf(x, l1)); h <- lt + lnB
    e <- x * d$Mbar; u <- ifelse(e < d$M_star, log(d$M_star / (d$M_star - e)), NA)
    cat(sprintf("  psi sd %.1f: corner %4.1f%% interior %4.1f%% saturated %4.1f%% | mean u %.3f (NA %4.1f%%) | Var(h) %.3f = Var(ln tau) %.3f + Var(ln B) %.4f + 2Cov %.4f -> evasion share %5.1f%% | cor(ln B*, ln tau) %.2f\n",
                s, 100 * mean(corner), 100 * mean(int), 100 * mean(sat), mean(u, na.rm = TRUE), 100 * mean(is.na(u)),
                var(h), var(lt), var(lnB), 2 * cov(lt, lnB), 100 * (var(lnB) + 2 * cov(lt, lnB)) / var(h), cor(lnB, lt)))
  }
}
