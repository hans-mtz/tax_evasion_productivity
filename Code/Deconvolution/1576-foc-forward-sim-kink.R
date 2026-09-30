## Forward simulation of the evasion FOC under the kinked power form (log 2026-09-29), at fitted theta, psi = 0 or
## N(0, sd^2) (f_psi not estimated by ELVIS; illustrative). q = (e/(kappa*Mbar))^k up to c_k = (1+k)^(-1/k):
## ln tau + ln B(x) = C(omega) + psi, B(x) = 1 - (1+k) x^k; omega at M = M*. Reports: no evasion (required ln B >= 0),
## interior, implied u = ln(M*/(M*-e)) among feasible (e < M*), share needing e > M*, u quantiles vs data V.
d <- read.csv("Code/Products/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv"); d <- d[d$corner == 0, ]
lt <- log(d$sales_tax_rate_purchases); om <- d$tilde_cal_W + (1 - d$beta) * d$cal_V
fits <- list(`k=1 (1574 gm1, TS 83)` = "Code/Products/1574-kgrid-k1-gm1.csv", `k=1 (1574 g0, TS 141)` = "Code/Products/1574-kgrid-k1-g0.csv",
             `k=0.3 11-row (1569 drop 6, TS 521)` = "Code/Products/1569-drop-6.csv")
cat(sprintf("interior firms %d | data V: mean %.3f, p50 %.3f, p90 %.3f, p99 %.3f\n", nrow(d), mean(d$cal_V), quantile(d$cal_V,.5), quantile(d$cal_V,.9), quantile(d$cal_V,.99)))
set.seed(20260929)
for (nm in names(fits)) {
  f <- read.csv(fits[[nm]]); k <- f$k_hat; ka <- f$lambda; ck <- (1 + k)^(-1 / k)
  C <- f$delta0_hat - f$delta1_hat * om + f$delta2_hat * om^2
  cat(sprintf("\n== %s: k=%.2f kappa=%.3f delta=(%.2f, %.2f, %.3f) | sd of C(omega) across firms %.2f vs sd ln tau %.2f\n",
              nm, k, ka, f$delta0_hat, f$delta1_hat, f$delta2_hat, sd(C), sd(lt)))
  for (s in c(0, 1, 3, 10)) {
    req <- C + rnorm(nrow(d), 0, s) - lt
    none <- req >= 0; x <- numeric(nrow(d))
    x[!none] <- (( 1 - exp(req[!none])) / (1 + k))^(1 / k)          # solves ln(1-(1+k)x^k) = req, x < c_k
    e <- x * ka * d$Mbar; ok <- e < d$M_star; u <- rep(NA, nrow(d)); u[ok] <- log(d$M_star[ok] / (d$M_star[ok] - e[ok]))
    uu <- u[!is.na(u) & !none]
    cat(sprintf("  psi sd %4.1f: no evasion %5.1f%% | interior %5.1f%% | e > M* %5.1f%% | u among evaders: mean %.3f p50 %.3f p90 %.3f p99 %.3f\n",
                s, 100*mean(none), 100*mean(!none), 100*mean(!ok), mean(uu), quantile(uu,.5), quantile(uu,.9), quantile(uu,.99)))
  }
}
