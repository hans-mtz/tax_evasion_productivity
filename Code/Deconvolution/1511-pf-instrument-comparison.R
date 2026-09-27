## TWO-TAX COPY (2026-09-26): generated from 1472-pf-instrument-comparison.R with ONLY the input swapped to the net-of-tax first stage
## (1501-fs-net.RData: fs_net_ls, used under the name fs_all_ls) and the output names renamed 1472 -> 1511. Estimator unchanged.
## Instrument comparison for the PF step (2026-09-21): five instruments on the paper's own (trimmed, full) sample.
##   lag_k, lag_l, lag_m (m*_{it-1}), lag_2_cal_W (W_{it-2}, UNtilded: includes alpha_K k + alpha_L l), lag_2_w_eps (tilded: W - a_K k - a_L l at the candidate alpha).
## Records per (industry, instrument): alpha_K, alpha_L, convergence, at-bound flag, and the ivreg diagnostics that estimate_prod_fn_bounds already returns
## (weak-instrument F, Wu-Hausman, Sargan -- Sargan is NA here: one instrument, one endogenous regressor => exactly identified).
## Then a random-row-drop stability check for the cells that flipped in 1471.
library(tidyverse); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata"); load("Code/Products/1501-fs-net.RData"); fs_all_ls <- fs_net_ls
ins_v <- c("lag_k", "lag_l", "lag_m", "lag_2_cal_W", "lag_2_w_eps")
inds <- names(fs_all_ls)[vapply(fs_all_ls, \(z) !is.null(z$data), TRUE)]
grid <- expand.grid(ind = inds, ins = ins_v, stringsAsFactors = FALSE)
one <- function(i) tryCatch({
    g <- grid[i, ]; r <- estimate_prod_fn_bounds(g$ind, fs_all_ls, obj_fun_ivar1_bounds, g$ins); d <- r$diagnostics
    gv <- \(nm, col) { v <- d[[col]][grepl(nm, rownames(d))]; if (length(v)) v[1] else NA_real_ }
    tibble(sic_3 = g$ind, ins = g$ins, beta = r$coeffs[["m"]], alpha_K = r$coeffs[["k"]], alpha_L = r$coeffs[["l"]], conv = r$convergence,
           F_weak = gv("Weak", "statistic"), p_weak = gv("Weak", "p-value"), p_wu = gv("Wu", "p-value"), p_sargan = gv("Sargan", "p-value"),
           n = nrow(fs_all_ls[[g$ind]]$data))
}, error = function(e) tibble(sic_3 = grid$ind[i], ins = grid$ins[i], beta = NA, alpha_K = NA, alpha_L = NA, conv = NA, F_weak = NA, p_weak = NA, p_wu = NA, p_sargan = NA, n = NA))
res <- bind_rows(mclapply(seq_len(nrow(grid)), one, mc.cores = max(1, detectCores() - 2)))
res <- res %>% mutate(at_bound = alpha_K < 1e-4 | alpha_K > 1 - 1e-4 | alpha_L < 1e-4 | alpha_L > 1 - 1e-4, rts = beta + alpha_K + alpha_L)
write.csv(res, "Code/Products/1511-pf-instrument-comparison.csv", row.names = FALSE)
