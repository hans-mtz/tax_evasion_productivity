## PRODUCT: Code/Products/1516-pf-points-updated-omega.{csv,RData} := PF point estimates (alpha_K, alpha_L) as the continuous
## minimum of the SAME test-inversion statistic whose regions ch. 6 reports, i.e. with Omega re-estimated at each candidate
## alpha (the convention of 1512/1513, and of ELVIS). Decided 2026-09-26: one objective for point, statistic and regions.
## Replaces, for the thesis tables and stage 2: (i) the single-instrument points of 1511 (estimate_prod_fn_bounds, a
## different objective; in 369 with m*_{it-1} it stopped at the bound (1, 0.83) while the statistic reaches ~0 elsewhere),
## (ii) the joint grid minimum of 1513 (0.02 resolution), (iii) the joint two-step points of 1502 (Omega fixed at a
## step-1 point, which deflates the statistic when step 1 lands far from the optimum; 313: 1.62 vs 9.96 at the same point).
## Systems: lag_m, lag_2_w_eps (single instrument, statistic of 1512) for the five ch. 6 industries; joint (statistic of
## 1513) for all 28 industries. Two-tax first stage, codes 6-9 excluded (1501). Multi-start on [0,1]^2, including the
## best grid cell (five industries), Nelder-Mead with a box penalty, then an L-BFGS-B polish.
library(tidyverse); library(parallel)
load("Code/Products/1501-fs-net.RData"); fs_all_ls <- fs_net_ls

## Statistics copied by parsing the two region scripts, so point and region use literally the same function
grab <- function(file, from, to) { s <- readLines(file); s[grep(from, s):(grep(to, s) - 1)] }
env_s <- new.env(); env_j <- new.env()
env_s$fs_all_ls <- env_j$fs_all_ls <- fs_all_ls
eval(parse(text = grab("Code/Deconvolution/1512-pf-testinv-regions.R", "^prep <- function", "^one_ind <- function")), envir = env_s)
eval(parse(text = grab("Code/Deconvolution/1513-pf-joint-testinv-regions.R", "^prep <- function", "^one_ind <- function")), envir = env_j)
g_single <- read.csv("Code/Products/1512-pf-testinv-grid.csv") %>% mutate(sic_3 = as.character(sic_3))
g_joint  <- read.csv("Code/Products/1513-pf-joint-testinv-grid.csv") %>% mutate(sic_3 = as.character(sic_3))

minimize <- function(f, starts) {
    pen <- function(p) if (any(p < 0 | p > 1)) 1e6 + 1e6 * sum(pmax(-p, p - 1, 0)) else { v <- f(p); if (is.na(v)) 1e6 else v }
    best <- NULL
    for (s0 in starts) {
        o <- optim(s0, pen, control = list(reltol = 1e-12, maxit = 4000))
        o2 <- tryCatch(optim(pmin(pmax(o$par, 0), 1), pen, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)),
                       error = function(e) o)
        if (o2$value > o$value) o2 <- o
        if (is.null(best) || o2$value < best$value) best <- o2
    }
    best
}
base_starts <- list(c(.3, .3), c(.1, .5), c(.5, .1), c(.2, .2), c(.4, .5))

five <- c("331", "322", "369", "313", "321")
one <- function(s, system) {
    if (system == "joint") {
        P <- env_j$prep(s); f <- \(p) env_j$J_at(P, p[1], p[2]); gg <- g_joint %>% filter(sic_3 == s)
    } else {
        P <- env_s$prep(s); f <- \(p) env_s$J_at(P, system, p[1], p[2]); gg <- g_single %>% filter(sic_3 == s, ins == system)
    }
    starts <- base_starts
    if (nrow(gg)) { k <- which.min(gg$J); starts <- c(list(pmin(pmax(c(gg$aK[k], gg$aL[k]), .005), .995)), starts) }
    o <- minimize(f, starts)
    tibble(sic_3 = s, system = system, alpha_K = o$par[1], alpha_L = o$par[2], stat = o$value,
           at_bound = any(o$par < 1e-4 | o$par > 1 - 1e-4), grid_min = if (nrow(gg)) min(gg$J) else NA_real_)
}
inds_all <- names(fs_all_ls)[vapply(fs_all_ls, \(z) !is.null(z$data) && nrow(z$data) > 50, TRUE)]
jobs <- bind_rows(expand_grid(sic_3 = five, system = c("lag_m", "lag_2_w_eps")),
                  tibble(sic_3 = inds_all, system = "joint"))
pts <- bind_rows(mcmapply(one, jobs$sic_3, jobs$system, SIMPLIFY = FALSE, mc.cores = max(1, detectCores() - 2))) %>%
    mutate(beta = vapply(sic_3, \(s) fs_all_ls[[s]]$beta, numeric(1))) %>% arrange(system, sic_3)
stopifnot(all(is.na(pts$grid_min) | pts$stat <= pts$grid_min + 1e-6))   # continuous minimum never worse than the grid
write.csv(pts, "Code/Products/1516-pf-points-updated-omega.csv", row.names = FALSE)
save(pts, file = "Code/Products/1516-pf-points-updated-omega.RData")
options(width = 200); print(pts %>% mutate(across(where(is.numeric), \(z) round(z, 3))), n = Inf)
cat("Saved: Code/Products/1516-pf-points-updated-omega.{csv,RData}\n")
