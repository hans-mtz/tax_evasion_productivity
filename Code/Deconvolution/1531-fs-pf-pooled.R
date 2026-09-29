## PRODUCT: Code/Products/1531-fs-pf-pooled.{csv,RData} := UNCORRECTED first stage and PF step for every industry: the
## same estimators as the corrected ones (1501 first stage, 1517 PF step with the single instrument W~_{it-2}), except
## that beta comes from ALL firms of the industry (pooled), not from corporations only. Plan: Thesis/PLAN.md §9b, P1.
## Uses: (a) stage 2, design A -- in the 19 industries where the headline test does not reject, all firms are corner
## firms (e = 0) with these uncorrected estimates; (b) Appendix E, corrected vs uncorrected side by side.
## The pooled first stage is GNR's first stage with big E = 1 (1520: ln g0 = mean(s), beta = exp(ln g0)).
## Implementation: first_stage_panel_me estimates beta on the juridical_organization == 3 rows, so it is called on a
## copy of 1501's sample with every row labelled 3 -- exactly the same code and sample filters (incl. the 369 upper
## cut), only the estimation subset changes. Real legal forms are kept in the joined output.
library(tidyverse); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata")
load("Code/Products/1501-fs-net.RData")     # df_net: 1501's sample, net share, corrected cal_V/cal_W/epsilon
mc_cores <- max(1, detectCores() - 2)

## (1) Pooled first stage ---------------------------------------------------------------------------------------
pool_in <- df_net %>% select(-cal_V, -cal_W, -epsilon) %>% mutate(juridical_organization = 3)
fs_pool_ls <- mcmapply(first_stage_panel_me, sic = unique(pool_in$sic_3),
                       MoreArgs = list(var = "log_mats_share_net", r_var = "materials", data = pool_in),
                       SIMPLIFY = FALSE, mc.cores = mc_cores)
names(fs_pool_ls) <- unique(pool_in$sic_3)
df_pool <- df_net %>% select(-cal_V, -cal_W, -epsilon) %>%
    left_join(bind_rows(lapply(fs_pool_ls, \(z) z$data)) %>% select(sic_3, year, plant, cal_V, cal_W, epsilon),
              by = c("sic_3", "year", "plant"))

## (2) PF step, single instrument W~_{it-2}, same code as 1517 ---------------------------------------------------
grab <- function(file, from, to) { s <- readLines(file); s[grep(from, s):(grep(to, s) - 1)] }
env_s <- new.env(); env_m <- new.env(); env_s$fs_all_ls <- fs_pool_ls
eval(parse(text = grab("Code/Deconvolution/1512-pf-testinv-regions.R", "^prep <- function", "^one_ind <- function")), envir = env_s)
eval(parse(text = grab("Code/Deconvolution/1516-pf-points-updated-omega.R", "^minimize <- function", "^five <- ")), envir = env_m)
gridv <- seq(0, 1, by = 0.005); crit <- qchisq(.95, c(2, 4))
rng <- \(v) if (!length(v)) c(NA, NA) else range(v)
one <- function(s, system = "lag_2_w_eps") {
    P <- env_s$prep(s); f <- \(a, b) env_s$J_at(P, system, a, b)
    g <- expand.grid(aK = gridv, aL = gridv); g$J <- mapply(f, g$aK, g$aL)
    k <- which.min(g$J)
    o <- env_m$minimize(\(p) f(p[1], p[2]), c(list(pmin(pmax(c(g$aK[k], g$aL[k]), .005), .995)), env_m$base_starts))
    sh <- g$J <= crit[1]; co <- g$J <= crit[2]; sh[is.na(sh)] <- FALSE; co[is.na(co)] <- FALSE
    Ks <- rng(g$aK[sh]); Ls <- rng(g$aL[sh]); Kc <- rng(g$aK[co]); Lc <- rng(g$aL[co])
    tibble(sic_3 = s, system = system, n_obs = nrow(P$d), alpha_K = o$par[1], alpha_L = o$par[2], stat = o$value,
           at_bound = any(o$par < 1e-4 | o$par > 1 - 1e-4),
           K_sh_lo = Ks[1], K_sh_hi = Ks[2], L_sh_lo = Ls[1], L_sh_hi = Ls[2],
           K_co_lo = Kc[1], K_co_hi = Kc[2], L_co_lo = Lc[1], L_co_hi = Lc[2],
           share_sharp = mean(sh), share_cons = mean(co))
}
inds <- names(fs_pool_ls)[vapply(fs_pool_ls, \(z) !is.null(z$data) && nrow(z$data) > 50, TRUE)]
res_pool <- bind_rows(mclapply(inds, one, mc.cores = mc_cores)) %>%
    mutate(beta = vapply(sic_3, \(s) fs_pool_ls[[s]]$beta, numeric(1)), first_stage = "pooled") %>% arrange(sic_3)

write.csv(res_pool, "Code/Products/1531-fs-pf-pooled.csv", row.names = FALSE)
save(df_pool, fs_pool_ls, res_pool, file = "Code/Products/1531-fs-pf-pooled.RData")
cat("Saved: Code/Products/1531-fs-pf-pooled.{csv,RData}\n")

## Corrected (1517, W~_{it-2}) vs uncorrected, by industry
load("Code/Products/1517-pf-systems-all-industries.RData")
cmp <- res %>% filter(system == "lag_2_w_eps") %>% select(sic_3, beta_c = beta, aK_c = alpha_K, aL_c = alpha_L) %>%
    full_join(res_pool %>% select(sic_3, beta_u = beta, aK_u = alpha_K, aL_u = alpha_L, n_obs), by = "sic_3") %>%
    mutate(d_beta = beta_u - beta_c)
options(width = 200); print(as.data.frame(cmp %>% mutate(across(where(is.numeric), \(x) round(x, 3)))))
