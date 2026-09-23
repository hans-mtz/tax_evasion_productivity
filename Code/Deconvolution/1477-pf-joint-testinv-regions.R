## Joint test-inversion grid for (alpha_K, alpha_L), BOTH instruments together (2026-09-21): replaces the bootstrap
## CI (1476), which produced nonsensical negative-crossing intervals -- bias-corrected percentile CIs don't respect
## the parameter's natural [0,1] domain, especially with the corner-hugging draws already documented (1472/1473/1474).
## Test inversion only ever EVALUATES the grid, so it can never report an elasticity outside [0,1] -- the direct fix.
## Same 5-moment system as 1474 (1, m*_{it-1}, W~_{it-2}, k, l)*eta, beta FIXED throughout (beta never enters eta --
## it only feeds the reported 'm' coefficient, so there is nothing to additionally fix here). At each grid cell,
## (gamma_0,gamma_1) is profiled via the SAME nested 2-step efficient-GMM as 1473's J_at (preliminary just-identified
## gamma from the first 2 moments, then Omega re-estimated AT THAT CELL -- CUE-style, correct size under H0: theta=
## theta0 at every candidate, not a globally-fixed weight). df: 5 moments - 2 profiled = 3 (sharp), 5 (conservative,
## crediting no profiling, matching the ELVIS/Theorem-F.1 convention used throughout this project).
library(tidyverse); library(parallel)
load("Code/Products/931.1-fs-se-het.RData")   # fs_all_ls
five <- c("331", "322", "369", "313", "321")
gridv <- seq(0, 1, by = 0.02)
crit_sharp <- qchisq(.95, 3); crit_cons <- qchisq(.95, 5)

prep <- function(x) {
    d <- fs_all_ls[[x]]$data %>% ungroup() %>% filter(is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m)) %>% arrange(plant, year)
    n <- nrow(d); pl <- d$plant
    lagi <- function(j) { i <- seq_len(n) - j; ok <- i >= 1; i[!ok] <- NA; ok2 <- !is.na(i) & pl[pmax(i, 1)] == pl; i[!ok2] <- NA
                          if (j == 2) { i1 <- seq_len(n) - 1; ok3 <- !is.na(i) & i1 >= 1 & pl[pmax(i1, 1)] == pl; i[!ok3] <- NA }; i }
    list(d = d, p1 = lagi(1), p2 = lagi(2))
}
J_at <- function(P, aK, aL) {
    d <- P$d; w <- d$cal_W - aK * d$k - aL * d$l
    S <- which(!is.na(P$p1) & !is.na(P$p2))   # needs BOTH lags (joint system)
    p1 <- P$p1[S]
    M <- cbind(1, d$m[p1], w[P$p2[S]], d$k[S], d$l[S])   # (1, m*_{it-1}, W~_{it-2}, k, l)
    y <- w[S]; x <- w[p1]; n <- length(S)
    a <- colMeans(M * y); B <- crossprod(M, cbind(1, x)) / n
    gam <- tryCatch(solve(B[1:2, ], a[1:2]), error = function(e) return(c(NA, NA))); if (anyNA(gam)) return(NA_real_)
    pl <- d$plant[S]
    for (it in 1:2) {
        eta <- y - gam[1] - gam[2] * x; G <- M * eta; gb <- colMeans(G)
        Gp <- rowsum(G, pl) - outer(as.vector(table(pl)[rownames(rowsum(G, pl))]), gb)
        Om <- crossprod(Gp) / n
        W <- tryCatch(solve(Om), error = function(e) NULL); if (is.null(W)) return(NA_real_)
        gam <- drop(solve(t(B) %*% W %*% B, t(B) %*% W %*% a))
    }
    r <- a - drop(B %*% gam); n * drop(t(r) %*% W %*% r)
}
one_ind <- function(x) { P <- prep(x); g <- expand.grid(aK = gridv, aL = gridv)
    g$sic_3 <- x; g$J <- mapply(\(a, b) J_at(P, a, b), g$aK, g$aL); g }
res <- bind_rows(mclapply(five, one_ind, mc.cores = 5))
write.csv(res, "Code/Products/1477-pf-joint-testinv-grid.csv", row.names = FALSE)

res <- res %>% mutate(pass = J <= crit_sharp, pass_cons = J <= crit_cons)
ng <- length(gridv)^2
smry <- res %>% group_by(sic_3) %>% summarise(min_J = min(J, na.rm = TRUE),
    aK_at_min = aK[which.min(J)], aL_at_min = aL[which.min(J)],
    K_range_sharp = if (any(pass, na.rm = TRUE)) sprintf("[%.2f,%.2f]", min(aK[pass], na.rm = TRUE), max(aK[pass], na.rm = TRUE)) else "empty",
    L_range_sharp = if (any(pass, na.rm = TRUE)) sprintf("[%.2f,%.2f]", min(aL[pass], na.rm = TRUE), max(aL[pass], na.rm = TRUE)) else "empty",
    share_sharp = mean(pass, na.rm = TRUE), share_cons = mean(pass_cons, na.rm = TRUE), .groups = "drop")
write.csv(smry, "Code/Products/1477-pf-joint-testinv-summary.csv", row.names = FALSE)
cat(sprintf("Reference: chi2_3,.95=%.2f (sharp), chi2_5,.95=%.2f (conservative)\n", crit_sharp, crit_cons))
cat("== Test-inversion region for (alpha_K,alpha_L), joint 5-moment system, beta fixed ==\n")
write.table(smry %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)

pal <- "#0072B2"
p <- ggplot(res, aes(aK, aL)) +
    geom_tile(data = res %>% filter(pass_cons), fill = pal, alpha = .25) +
    geom_tile(data = res %>% filter(pass), fill = pal, alpha = .6) +
    geom_point(data = smry, aes(aK_at_min, aL_at_min), shape = 4, size = 2.5, stroke = 1.1, colour = "black") +
    facet_wrap(~sic_3, nrow = 2, labeller = labeller(sic_3 = \(z) paste("Industry", z))) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = expression(alpha[K]), y = expression(alpha[L]),
         title = "Joint test-inversion region for (alpha_K, alpha_L), both instruments, beta fixed",
         subtitle = "Dark = chi2_3,.95 (sharp, gamma profiled); light = chi2_5,.95 (conservative); cross = grid minimum") +
    theme_minimal(base_size = 11)
ggsave("Paper/images/1477-pf-joint-testinv-regions.png", p, width = 11, height = 7, dpi = 200)
cat("Saved: Paper/images/1477-pf-joint-testinv-regions.png\n")
