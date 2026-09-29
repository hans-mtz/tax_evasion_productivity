## Forward simulation of the evasion FOC under three detection forms (log 2026-09-28), at the S4 center's cost parameters
## (delta0 + year intercepts, delta1, delta2 from 1554-yfe-l0.5), psi = 0. ln tau + ln B(e) = C(omega); omega at M = M*.
## Forms: (L) linear in levels q = lambda*e, B = 1 - 2*lambda*e (old headline lambda and neighbours);
##        (E) exponential with scale Mbar, q = 1 - exp(-x), B = exp(-x)(1-x), x < 1;
##        (P) power with scale Mbar, q = x^k, B = 1 - (1+k) x^k, x < (1+k)^(-1/k).
## Reports corner / interior / saturated shares, evasion share of Var(h), implied mean u among firms with e < M*,
## and the share whose optimum needs e > M* (M < 0). Data: design-A interior, mean V = 0.170.
d <- read.csv("Code/Products/1546-stage2-input-designA-tau-trim0.005.csv"); d <- d[d$corner == 0, ]
f <- read.csv("Code/Products/1554-yfe-l0.5.csv")
lt <- log(d$sales_tax_rate_purchases); om <- d$tilde_cal_W + (1 - d$beta) * d$cal_V
d0 <- f$delta0_hat + c(0, unlist(f[paste0("d0yr", 82:91)]))[d$year - 80]
req <- d0 - f$delta1_hat * om + f$delta2_hat * om^2 - lt        # required ln B at psi = 0
run <- function(lab, lnB, xmax, toE) {                           # lnB(x) decreasing on [0, xmax); e = toE(x)
  lo <- lnB(xmax * (1 - 1e-9)); corner <- req >= 0; sat <- req <= lo; int <- !corner & !sat
  x <- numeric(nrow(d)); x[sat] <- xmax * (1 - 1e-9)
  x[int] <- vapply(which(int), \(i) uniroot(\(z) lnB(z) - req[i], c(0, xmax * (1 - 1e-12)))$root, 0)
  e <- toE(x); ok <- e < d$M_star; u <- log(d$M_star[ok] / (d$M_star[ok] - e[ok])); lb <- lnB(x); h <- lt + lb
  cat(sprintf("%-22s corner %4.1f%% interior %4.1f%% saturated %4.1f%% | evasion share of Var(h) %6.1f%% | mean u %.3f | e > M* %4.1f%%\n",
      lab, 100*mean(corner), 100*mean(int), 100*mean(sat), 100*(var(lb) + 2*cov(lt, lb))/var(h), mean(u), 100*mean(!ok)))
}
cat(sprintf("interior firms %d | data mean V %.3f | sd ln tau %.2f\n", nrow(d), mean(d$cal_V), sd(lt)))
for (lam in c(1e-7, 5.427e-7, 3e-6)) run(sprintf("L lambda=%.3g", lam), \(x) log(1 - 2 * x), 0.5, \(x) x / lam)   # x = lambda*e
run("E scaled (Mbar)", \(x) -x + log(1 - x), 1, \(x) x * d$Mbar)
for (k in c(0.25, 0.5, 0.75, 1)) run(sprintf("P k=%.2f (Mbar)", k), \(x) log(1 - (1 + k) * x^k), (1 + k)^(-1 / k), \(x) x * d$Mbar)
