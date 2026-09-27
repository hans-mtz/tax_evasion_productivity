## PRODUCT: Code/Products/1517-pf-systems-all-industries.{csv,RData} := for EVERY industry with a two-tax first stage, the
## three PF systems side by side -- single instrument m*_{it-1} (lag_m), single instrument W~_{it-2} (lag_2_w_eps), joint --
## each with: point estimate (continuous minimum of its test-inversion statistic, Omega at each candidate), statistic at
## the estimate, and the 95% sharp / conservative region projections on a 0.005 grid over [0,1]^2. Purpose (2026-09-26):
## decide between single instruments and the joint system on all industries, not only the five of ch. 6.
## Statistics parsed from 1512 (single) and 1513 (joint), minimizer from 1516, so everything is literally the same code.
## Sharp / conservative: single chi2_2 / chi2_4; joint chi2_3 / chi2_5. Two-tax first stage, codes 6-9 excluded (1501).
library(tidyverse); library(parallel)
load("Code/Products/1501-fs-net.RData"); fs_all_ls <- fs_net_ls

grab <- function(file, from, to) { s <- readLines(file); s[grep(from, s):(grep(to, s) - 1)] }
env_s <- new.env(); env_j <- new.env(); env_m <- new.env()
env_s$fs_all_ls <- env_j$fs_all_ls <- fs_all_ls
eval(parse(text = grab("Code/Deconvolution/1512-pf-testinv-regions.R", "^prep <- function", "^one_ind <- function")), envir = env_s)
eval(parse(text = grab("Code/Deconvolution/1513-pf-joint-testinv-regions.R", "^prep <- function", "^one_ind <- function")), envir = env_j)
eval(parse(text = grab("Code/Deconvolution/1516-pf-points-updated-omega.R", "^minimize <- function", "^five <- ")), envir = env_m)

gridv <- seq(0, 1, by = 0.005)   # 0.005 (was 0.02, 2026-09-26) so region bounds can be reported to 3 digits
crit <- list(single = qchisq(.95, c(2, 4)), joint = qchisq(.95, c(3, 5)))
rng <- \(v) if (!length(v)) c(NA, NA) else range(v)

one <- function(s, system) {
    if (system == "joint") { P <- env_j$prep(s); f <- \(a, b) env_j$J_at(P, a, b); cr <- crit$joint }
    else { P <- env_s$prep(s); f <- \(a, b) env_s$J_at(P, system, a, b); cr <- crit$single }
    g <- expand.grid(aK = gridv, aL = gridv); g$J <- mapply(f, g$aK, g$aL)
    k <- which.min(g$J)
    o <- env_m$minimize(\(p) f(p[1], p[2]), c(list(pmin(pmax(c(g$aK[k], g$aL[k]), .005), .995)), env_m$base_starts))
    sh <- g$J <= cr[1]; co <- g$J <= cr[2]; sh[is.na(sh)] <- FALSE; co[is.na(co)] <- FALSE
    Ks <- rng(g$aK[sh]); Ls <- rng(g$aL[sh]); Kc <- rng(g$aK[co]); Lc <- rng(g$aL[co])
    tibble(sic_3 = s, system = system, n_obs = nrow(P$d), alpha_K = o$par[1], alpha_L = o$par[2], stat = o$value,
           at_bound = any(o$par < 1e-4 | o$par > 1 - 1e-4),
           K_sh_lo = Ks[1], K_sh_hi = Ks[2], L_sh_lo = Ls[1], L_sh_hi = Ls[2],
           K_co_lo = Kc[1], K_co_hi = Kc[2], L_co_lo = Lc[1], L_co_hi = Lc[2],
           share_sharp = mean(sh), share_cons = mean(co))
}
inds <- names(fs_all_ls)[vapply(fs_all_ls, \(z) !is.null(z$data) && nrow(z$data) > 50, TRUE)]
jobs <- expand_grid(sic_3 = inds, system = c("lag_m", "lag_2_w_eps", "joint"))
res <- bind_rows(mcmapply(one, jobs$sic_3, jobs$system, SIMPLIFY = FALSE, mc.cores = max(1, detectCores() - 2))) %>%
    mutate(beta = vapply(sic_3, \(s) fs_all_ls[[s]]$beta, numeric(1))) %>% arrange(sic_3, system)
write.csv(res, "Code/Products/1517-pf-systems-all-industries.csv", row.names = FALSE)
save(res, file = "Code/Products/1517-pf-systems-all-industries.RData")
cat("Saved: Code/Products/1517-pf-systems-all-industries.{csv,RData}\n")
