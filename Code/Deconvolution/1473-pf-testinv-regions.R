## Test inversion over (alpha_K, alpha_L) for the PF step, one region per instrument (2026-09-21).
## Instruments: lag_m (m*_{it-1}), lag_2_w_eps (tilded W~_{it-2}), lag_k, lag_l. Paper's own sample (fs_all_ls, 931.1), beta fixed at the stage-1 value.
## At each candidate alpha: w = cal_W - aK*k - aL*l; inner AR(1) w_t = g0 + g1*w_{t-1} + eta; moments g_it = (1, Z, k, l)*eta  [E eta, E Z eta, E k eta, E l eta].
## (g0,g1) are PROFILED (moments are linear in g): efficient two-step GMM with a plant-clustered, centered covariance; J = n * rbar' W rbar.
## Region = {alpha: J <= chi2_{2,.95}=5.99 (sharp, strong id of g)}; conservative alternative chi2_{4,.95}=9.49. Lags are row-based within plant (as in
## obj_fun_ivar1_bounds). Outputs: Code/Products/1473-pf-testinv-{grid,summary}.csv, Paper/images/1473-pf-testinv-regions.png.
library(tidyverse); library(parallel)
load("Code/Products/931.1-fs-se-het.RData")   # fs_all_ls
ins_v <- c("lag_m", "lag_2_w_eps", "lag_k", "lag_l")
five <- c("331", "322", "369", "313", "321")
gridv <- seq(0, 1, by = 0.02)
crit_sharp <- qchisq(.95, 2); crit_cons <- qchisq(.95, 4)

prep <- function(x) {
    d <- fs_all_ls[[x]]$data %>% ungroup() %>% filter(is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m)) %>% arrange(plant, year)
    n <- nrow(d); pl <- d$plant
    lagi <- function(j) { i <- seq_len(n) - j; ok <- i >= 1; i[!ok] <- NA; ok2 <- !is.na(i) & pl[pmax(i, 1)] == pl; i[!ok2] <- NA
                          if (j == 2) { i1 <- seq_len(n) - 1; ok3 <- !is.na(i) & i1 >= 1 & pl[pmax(i1, 1)] == pl; i[!ok3] <- NA }; i }
    list(d = d, p1 = lagi(1), p2 = lagi(2), beta = fs_all_ls[[x]]$beta)
}
J_at <- function(P, ins, aK, aL) {
    d <- P$d; w <- d$cal_W - aK * d$k - aL * d$l
    S <- switch(ins, lag_2_w_eps = which(!is.na(P$p1) & !is.na(P$p2)), which(!is.na(P$p1)))
    p1 <- P$p1[S]
    Z <- switch(ins, lag_m = d$m[p1], lag_k = d$k[p1], lag_l = d$l[p1], lag_2_w_eps = w[P$p2[S]])
    M <- cbind(1, Z, d$k[S], d$l[S]); y <- w[S]; x <- w[p1]; n <- length(S)
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
one_ind <- function(x) {
    P <- prep(x); g <- expand.grid(aK = gridv, aL = gridv)
    bind_rows(lapply(ins_v, \(ii) { g$sic_3 <- x; g$ins <- ii; g$J <- mapply(\(a, b) J_at(P, ii, a, b), g$aK, g$aL); g }))
}
res <- bind_rows(mclapply(five, one_ind, mc.cores = 5))
write.csv(res, "Code/Products/1473-pf-testinv-grid.csv", row.names = FALSE)

est <- read.csv("Code/Products/1472-pf-instrument-comparison.csv") %>% mutate(sic_3 = as.character(sic_3)) %>% filter(sic_3 %in% five, ins %in% ins_v)
res <- res %>% mutate(pass = J <= crit_sharp, pass_cons = J <= crit_cons)
ng <- length(gridv)^2
smry <- res %>% group_by(sic_3, ins) %>% summarise(min_J = min(J, na.rm = TRUE), share_sharp = sum(pass, na.rm = TRUE) / ng, share_cons = sum(pass_cons, na.rm = TRUE) / ng, .groups = "drop")
ov <- res %>% select(sic_3, aK, aL, ins, pass) %>% pivot_wider(names_from = ins, values_from = pass)
ovs <- ov %>% group_by(sic_3) %>% summarise(
    all4 = mean(lag_m & lag_2_w_eps & lag_k & lag_l, na.rm = TRUE), m_W = mean(lag_m & lag_2_w_eps, na.rm = TRUE), m_k = mean(lag_m & lag_k, na.rm = TRUE), m_l = mean(lag_m & lag_l, na.rm = TRUE),
    W_k = mean(lag_2_w_eps & lag_k, na.rm = TRUE), W_l = mean(lag_2_w_eps & lag_l, na.rm = TRUE), k_l = mean(lag_k & lag_l, na.rm = TRUE), any_ = mean(lag_m | lag_2_w_eps | lag_k | lag_l, na.rm = TRUE), .groups = "drop")
write.csv(smry, "Code/Products/1473-pf-testinv-summary.csv", row.names = FALSE); write.csv(ovs, "Code/Products/1473-pf-testinv-overlap.csv", row.names = FALSE)
cat("== Region size (share of [0,1]^2 grid), sharp chi2_2 / conservative chi2_4, and min J ==\n"); write.table(smry %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)
cat("\n== Overlap (share of grid where the listed regions intersect, sharp) ==\n"); write.table(ovs %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)

pal <- c(lag_m = "#0072B2", lag_2_w_eps = "#D55E00", lag_k = "#009E73", lag_l = "#CC79A7")
lbl <- c(lag_m = "m*_{it-1}", lag_2_w_eps = "W~_{it-2}", lag_k = "k_{it-1}", lag_l = "l_{it-1}")
p <- ggplot() +
    geom_tile(data = res %>% filter(pass_cons), aes(aK, aL, fill = ins), alpha = .25) +
    geom_tile(data = res %>% filter(pass), aes(aK, aL, fill = ins), alpha = .55) +
    geom_point(data = est, aes(alpha_K, alpha_L, colour = ins), shape = 4, size = 2.2, stroke = 1) +
    facet_wrap(~sic_3, nrow = 2, labeller = labeller(sic_3 = \(z) paste("Industry", z))) +
    scale_fill_manual(values = pal, labels = lbl, name = "Instrument (region)") + scale_colour_manual(values = pal, labels = lbl, name = "Instrument (point est.)") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + labs(x = expression(alpha[K]), y = expression(alpha[L]),
         title = "Test-inversion regions for (alpha_K, alpha_L) by instrument", subtitle = "Dark = chi2_2 (profiled), light = chi2_4 (conservative); crosses = 2SLS-GMM point estimates") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave("Paper/images/1473-pf-testinv-regions.png", p, width = 11, height = 7, dpi = 200); cat("Saved: Paper/images/1473-pf-testinv-regions.png\n")
