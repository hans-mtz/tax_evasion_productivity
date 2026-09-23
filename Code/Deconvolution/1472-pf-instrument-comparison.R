## Instrument comparison for the PF step (2026-09-21): five instruments on the paper's own (trimmed, full) sample.
##   lag_k, lag_l, lag_m (m*_{it-1}), lag_2_cal_W (W_{it-2}, UNtilded: includes alpha_K k + alpha_L l), lag_2_w_eps (tilded: W - a_K k - a_L l at the candidate alpha).
## Records per (industry, instrument): alpha_K, alpha_L, convergence, at-bound flag, and the ivreg diagnostics that estimate_prod_fn_bounds already returns
## (weak-instrument F, Wu-Hausman, Sargan -- Sargan is NA here: one instrument, one endogenous regressor => exactly identified).
## Then a random-row-drop stability check for the cells that flipped in 1471.
library(tidyverse); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata"); load("Code/Products/931.1-fs-se-het.RData")
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
write.csv(res, "Code/Products/1472-pf-instrument-comparison.csv", row.names = FALSE)
cat("== By instrument, 29 industries (paper sample) ==\n")
sm <- res %>% group_by(ins) %>% summarise(n_cells = n(), nonconv = sum(conv != 0, na.rm = TRUE), at_bound = sum(at_bound, na.rm = TRUE),
    med_F = median(F_weak, na.rm = TRUE), share_F_gt10 = mean(F_weak > 10, na.rm = TRUE), share_p_wu_lt05 = mean(p_wu < .05, na.rm = TRUE),
    med_rts = median(rts, na.rm = TRUE), share_rts_in_.8_1.2 = mean(rts > .8 & rts < 1.2, na.rm = TRUE), .groups = "drop")
write.table(sm %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)
cat("\n== Agreement across instruments: sd of alpha_K / alpha_L across the 5 instruments, median over industries ==\n")
ag <- res %>% group_by(sic_3) %>% summarise(sdK = sd(alpha_K), sdL = sd(alpha_L), .groups = "drop"); cat(sprintf("median sd alpha_K %.3f, alpha_L %.3f\n", median(ag$sdK), median(ag$sdL)))
five <- c("331", "322", "369", "313", "321")
cat("\n== Five paper industries: alpha_K / alpha_L / conv / F_weak by instrument ==\n")
write.table(res %>% filter(sic_3 %in% five) %>% mutate(across(where(is.numeric), ~round(.x, 3))) %>% select(sic_3, ins, alpha_K, alpha_L, conv, F_weak, p_wu, p_sargan) %>% arrange(ins, sic_3), quote = FALSE, sep = "\t", row.names = FALSE)

## %% Random-row-drop stability (40 reps; same k rows as 1471 dropped) ---------------------------
set.seed(1); R <- 40
cells <- tribble(~ind, ~k, "321", 12, "322", 6, "369", 7)
st <- map_dfr(seq_len(nrow(cells)), \(i) map_dfr(ins_v, \(ins) {
    c1 <- cells[i, ]; d <- fs_all_ls[[c1$ind]]$data
    base <- estimate_prod_fn_bounds(c1$ind, fs_all_ls, obj_fun_ivar1_bounds, ins)$coeffs
    m <- do.call(rbind, mclapply(seq_len(R), \(r) { f2 <- fs_all_ls; f2[[c1$ind]]$data <- d[-sample(nrow(d), c1$k), ]
        x <- estimate_prod_fn_bounds(c1$ind, f2, obj_fun_ivar1_bounds, ins)$coeffs; c(K = x[["k"]], L = x[["l"]]) }, mc.cores = max(1, detectCores() - 2)))
    tibble(sic_3 = c1$ind, ins = ins, K_base = base[["k"]], K_range = sprintf("[%.2f,%.2f]", min(m[, 1]), max(m[, 1])),
           share_K_moved_gt_0.1 = mean(abs(m[, 1] - base[["k"]]) > .1), share_L_moved_gt_0.1 = mean(abs(m[, 2] - base[["l"]]) > .1))
}))
cat("\n== Stability under random row drops ==\n"); write.table(st %>% mutate(across(where(is.numeric), ~round(.x, 3))), quote = FALSE, sep = "\t", row.names = FALSE)
write.csv(st, "Code/Products/1472-pf-instrument-stability.csv", row.names = FALSE)
