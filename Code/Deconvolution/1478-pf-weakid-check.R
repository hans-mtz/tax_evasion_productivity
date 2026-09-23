## Check whether gamma (the inner AR(1) step) is weakly identified in industry 313, where the sharp test-inversion
## region (1477) came back EMPTY (min J=9.74 > chi2_3,.95=7.81) but the conservative one didn't. Two diagnostics, at
## 313's own grid minimum and, for context, at 322's (a well-behaved industry throughout this investigation):
##   (1) First-stage F: regress w_{t-1} (the endogenous regressor in eta=w_t-g0-g1*w_{t-1}) on the 4 non-intercept
##       instruments (m*_{it-1}, W~_{it-2}, k, l) -- classic weak-instrument diagnostic, model-free.
##   (2) Condition number of B'WB (the 2x2 matrix actually inverted to solve for gamma in J_at/fit_joint) -- a
##       GMM-native concentration measure; ill-conditioned means gamma is numerically fragile even if (1) looks OK.
## Also checks 313's diagnostics ACROSS its whole conservative-passing region, not just the single grid minimum.
library(tidyverse)
load("Code/Products/931.1-fs-se-het.RData")   # fs_all_ls
grid <- read.csv("Code/Products/1477-pf-joint-testinv-grid.csv")
crit_sharp <- qchisq(.95, 3); crit_cons <- qchisq(.95, 5)
grid <- grid %>% mutate(pass = J <= crit_sharp, pass_cons = J <= crit_cons)

prep <- function(x) {
    d <- fs_all_ls[[x]]$data %>% ungroup() %>% filter(is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m)) %>% arrange(plant, year)
    n <- nrow(d); pl <- d$plant
    lagi <- function(j) { i <- seq_len(n) - j; ok <- i >= 1; i[!ok] <- NA; ok2 <- !is.na(i) & pl[pmax(i, 1)] == pl; i[!ok2] <- NA
                          if (j == 2) { i1 <- seq_len(n) - 1; ok3 <- !is.na(i) & i1 >= 1 & pl[pmax(i1, 1)] == pl; i[!ok3] <- NA }; i }
    list(d = d, p1 = lagi(1), p2 = lagi(2))
}
diag_at <- function(P, aK, aL) {
    d <- P$d; w <- d$cal_W - aK * d$k - aL * d$l
    S <- which(!is.na(P$p1) & !is.na(P$p2)); p1 <- P$p1[S]
    Z1 <- d$m[p1]; Z2 <- w[P$p2[S]]; k <- d$k[S]; l <- d$l[S]; x <- w[p1]; y <- w[S]; n <- length(S)
    M <- cbind(1, Z1, Z2, k, l)
    fs <- lm(x ~ Z1 + Z2 + k + l); fsum <- summary(fs)
    Fstat <- fsum$fstatistic[["value"]]; R2 <- fsum$r.squared
    ## Rebuild the SAME efficient W as J_at's final step, then get B'WB's condition number
    a <- colMeans(M * y); B <- crossprod(M, cbind(1, x)) / n
    gam0 <- solve(B[1:2, ], a[1:2]); pl <- d$plant[S]
    for (it in 1:2) { eta <- y - gam0[1] - gam0[2] * x; G <- M * eta; gb <- colMeans(G)
        Gp <- rowsum(G, pl) - outer(as.vector(table(pl)[rownames(rowsum(G, pl))]), gb); Om <- crossprod(Gp) / n
        W <- solve(Om); gam0 <- drop(solve(t(B) %*% W %*% B, t(B) %*% W %*% a)) }
    BWB <- t(B) %*% W %*% B; ev <- eigen(BWB, symmetric = TRUE, only.values = TRUE)$values
    tibble(aK = aK, aL = aL, n = n, F_stat = Fstat, R2 = R2, cond_BWB = max(ev) / min(ev), gamma0 = gam0[1], gamma1 = gam0[2])
}

cat("== At each industry's own grid minimum (313 = the one that failed sharp; 322 = well-behaved benchmark) ==\n")
p313 <- prep("313"); p322 <- prep("322")
m313 <- grid %>% filter(sic_3 == "313") %>% slice_min(J, n = 1)
m322 <- grid %>% filter(sic_3 == "322") %>% slice_min(J, n = 1)
d313 <- diag_at(p313, m313$aK, m313$aL) %>% mutate(sic_3 = "313", J = m313$J)
d322 <- diag_at(p322, m322$aK, m322$aL) %>% mutate(sic_3 = "322", J = m322$J)
write.table(bind_rows(d313, d322) %>% select(sic_3, aK, aL, n, J, F_stat, R2, cond_BWB, gamma0, gamma1) %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)

cat("\n== Across 313's WHOLE conservative-passing region (not just the minimum) ==\n")
reg313 <- grid %>% filter(sic_3 == "313", pass_cons)
d313_region <- bind_rows(lapply(seq_len(nrow(reg313)), \(i) diag_at(p313, reg313$aK[i], reg313$aL[i])))
cat(sprintf("n grid points in region: %d\n", nrow(d313_region)))
cat(sprintf("F_stat: median %.2f [min %.2f, max %.2f]; R2: median %.3f [min %.3f, max %.3f]; cond(B'WB): median %.1f [min %.1f, max %.1f]\n",
            median(d313_region$F_stat), min(d313_region$F_stat), max(d313_region$F_stat),
            median(d313_region$R2), min(d313_region$R2), max(d313_region$R2),
            median(d313_region$cond_BWB), min(d313_region$cond_BWB), max(d313_region$cond_BWB)))

cat("\n== For comparison, 322's whole SHARP-passing region ==\n")
reg322 <- grid %>% filter(sic_3 == "322", pass)
d322_region <- bind_rows(lapply(seq_len(nrow(reg322)), \(i) diag_at(p322, reg322$aK[i], reg322$aL[i])))
cat(sprintf("n grid points in region: %d\n", nrow(d322_region)))
cat(sprintf("F_stat: median %.2f [min %.2f, max %.2f]; R2: median %.3f [min %.3f, max %.3f]; cond(B'WB): median %.1f [min %.1f, max %.1f]\n",
            median(d322_region$F_stat), min(d322_region$F_stat), max(d322_region$F_stat),
            median(d322_region$R2), min(d322_region$R2), max(d322_region$R2),
            median(d322_region$cond_BWB), min(d322_region$cond_BWB), max(d322_region$cond_BWB)))
save(d313, d322, d313_region, d322_region, file = "Code/Products/1478-pf-weakid-check.RData")

## %% Updated summary table: report BOTH regions, conservative as the headline given 313's sharp-test failure -----
five <- c("331", "322", "369", "313", "321")
smry <- grid %>% group_by(sic_3) %>% summarise(min_J = min(J), aK_min = aK[which.min(J)], aL_min = aL[which.min(J)],
    K_sharp = if (any(pass)) sprintf("[%.2f,%.2f]", min(aK[pass]), max(aK[pass])) else "empty",
    L_sharp = if (any(pass)) sprintf("[%.2f,%.2f]", min(aL[pass]), max(aL[pass])) else "empty",
    K_cons = sprintf("[%.2f,%.2f]", min(aK[pass_cons]), max(aK[pass_cons])),
    L_cons = sprintf("[%.2f,%.2f]", min(aL[pass_cons]), max(aL[pass_cons])),
    share_sharp = mean(pass), share_cons = mean(pass_cons), passes_sharp = any(pass), .groups = "drop") %>%
    mutate(sic_3 = factor(sic_3, five)) %>% arrange(sic_3)
write.csv(smry, "Code/Products/1478-pf-joint-testinv-summary-cons.csv", row.names = FALSE)
cat("\n== Updated table: sharp (chi2_3,.95=7.81) and conservative (chi2_5,.95=11.07) regions ==\n")
write.table(smry %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)
