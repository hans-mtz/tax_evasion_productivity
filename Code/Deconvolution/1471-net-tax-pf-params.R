## Phase 2 (2026-09-21): production-function parameters (alpha_K, alpha_L; beta from the first stage) with the NET-of-tax first stage.
## Three specs, same estimator (estimate_prod_fn_bounds / obj_fun_ivar1_bounds, 021-deconv-funs.R), four instruments as in 300-deconv-prod.R:
##   paper : the ORIGINAL stage-1 fits on the FULL sample (fs_all_ls in 931.1-fs-se-het.RData) -- must reproduce the paper's table
##           (Code/Products/deconv_prod_fun_trim.RData, PF_me_tbl) exactly; sanity check below.
##   gross : gross first stage re-run on the COMMON sample (rows where BOTH gross and net shares are defined; 1470-fs-net.RData)
##   net   : net-of-tax first stage on the common sample.
## gross-vs-paper isolates the effect of dropping the ~85 rows (and the broken lag structure they cause: lag() is by row within plant);
## net-vs-gross isolates the effect of the tax netting. Nothing approved is overwritten.
## Outputs: Code/Products/1471-net-pf-params.{RData,csv}, Paper/tbls/1471-net-pf-params-table.png (paper's 5 industries).
library(tidyverse); library(parallel); library(tinytable)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata")
source("Code/Deconvolution/050-render-tbls.R")
load("Code/Products/1470-fs-net.RData")          # fs_specs$gross, $net (common sample)
load("Code/Products/931.1-fs-se-het.RData")      # fs_all_ls (paper's stage-1 fits, full sample)
fs_specs$paper <- fs_all_ls[names(fs_specs$gross)]
mc_cores <- max(1, detectCores() - 2)
inds <- names(fs_specs$gross)
ok_inds <- inds[vapply(inds, \(s) all(vapply(fs_specs, \(z) !is.null(z[[s]]$data), TRUE)), TRUE)]
ins_v <- c("lag_k", "lag_l", "lag_m", "lag_2_cal_W")
grid <- expand.grid(spec = c("paper", "gross", "net"), ins = ins_v, ind = ok_inds, stringsAsFactors = FALSE)
fit_one <- function(i) tryCatch({
    g <- grid[i, ]
    r <- estimate_prod_fn_bounds(g$ind, fs_specs[[g$spec]], obj_fun_ivar1_bounds, g$ins)
    tibble(spec = g$spec, ins = g$ins, sic_3 = g$ind, beta = r$coeffs[["m"]], alpha_K = r$coeffs[["k"]], alpha_L = r$coeffs[["l"]],
           conv = r$convergence, n = nrow(fs_specs[[g$spec]][[g$ind]]$data))
}, error = function(e) tibble(spec = grid$spec[i], ins = grid$ins[i], sic_3 = grid$ind[i], beta = NA, alpha_K = NA, alpha_L = NA, conv = NA, n = NA))
pf_all <- bind_rows(mclapply(seq_len(nrow(grid)), fit_one, mc.cores = mc_cores))
save(pf_all, file = "Code/Products/1471-net-pf-params.RData")
wide <- pf_all %>% pivot_wider(names_from = spec, values_from = c(beta, alpha_K, alpha_L, conv, n)) %>%
    mutate(d_K_rows = alpha_K_gross - alpha_K_paper, d_K_net = alpha_K_net - alpha_K_gross,
           d_L_rows = alpha_L_gross - alpha_L_paper, d_L_net = alpha_L_net - alpha_L_gross,
           d_b_rows = beta_gross - beta_paper, d_b_net = beta_net - beta_gross)
write.csv(wide, "Code/Products/1471-net-pf-params.csv", row.names = FALSE)

## %% Sanity: the 'paper' spec must reproduce the trim table exactly ---------------------
tr <- new.env(); load("Code/Products/deconv_prod_fun_trim.RData", tr)
trl <- as.data.frame(tr$PF_me_tbl); names(trl) <- c("sic_3", "input", ins_v, "GNR", "OLS")
trl <- trl %>% pivot_longer(all_of(ins_v), names_to = "ins", values_to = "paper_table") %>%
    pivot_wider(names_from = input, values_from = paper_table, names_prefix = "tbl_")
chk <- wide %>% mutate(sic_3 = as.numeric(sic_3)) %>% inner_join(trl %>% mutate(sic_3 = as.numeric(sic_3)), by = c("sic_3", "ins")) %>%
    transmute(sic_3, ins, e_b = beta_paper - tbl_m, e_K = alpha_K_paper - tbl_k, e_L = alpha_L_paper - tbl_l)
cat(sprintf("Reproduction of PF_me_tbl (trim) from the 'paper' spec: max|beta err| %.2e, max|alpha_K err| %.2e, max|alpha_L err| %.2e (n cells %d)\n",
            max(abs(chk$e_b)), max(abs(chk$e_K)), max(abs(chk$e_L)), nrow(chk)))

cat("Failed fits:", sum(is.na(pf_all$alpha_K)), " non-converged:", sum(pf_all$conv != 0, na.rm = TRUE), "\n")
for (ins in ins_v) { w <- wide[wide$ins == ins, ]
    cat(sprintf("%-12s mean|d| rows-dropped: K %.3f L %.3f b %.3f | net-vs-gross: K %.3f L %.3f b %.3f | cells with |dK_rows|>0.1: %d, |dK_net|>0.1: %d\n", ins,
        mean(abs(w$d_K_rows), na.rm = TRUE), mean(abs(w$d_L_rows), na.rm = TRUE), mean(abs(w$d_b_rows), na.rm = TRUE),
        mean(abs(w$d_K_net), na.rm = TRUE), mean(abs(w$d_L_net), na.rm = TRUE), mean(abs(w$d_b_net), na.rm = TRUE),
        sum(abs(w$d_K_rows) > 0.1, na.rm = TRUE), sum(abs(w$d_K_net) > 0.1, na.rm = TRUE))) }

## %% PNG table, the paper's 5 industries (the ones in PF_me_tbl) ----------------------------
five <- c("331", "322", "369", "313", "321")
f3 <- \(x) sprintf("%.3f", x)
tb <- wide %>% filter(sic_3 %in% as.numeric(five), ins %in% c("lag_m", "lag_2_cal_W")) %>% mutate(sic_3 = factor(sic_3, as.numeric(five))) %>%
    arrange(desc(ins), sic_3) %>%
    transmute(ins, Industry = as.character(sic_3), bp = f3(beta_paper), bg = f3(beta_gross), bn = f3(beta_net),
              kp = f3(alpha_K_paper), kg = f3(alpha_K_gross), kn = f3(alpha_K_net), lp = f3(alpha_L_paper), lg = f3(alpha_L_gross), ln = f3(alpha_L_net))
idx <- list("Instrument $m^*_{it-1}$" = 1, "Instrument $\\mathcal{W}_{it-2}$" = 6)
tt_obj <- tt(select(tb, -ins), align = "lccccccccc",
             notes = "Paper = original stage 1, full sample (reproduces Table of the paper). Gross/Net = common sample (rows where both shares are defined; 85 rows fewer), gross vs.\\ net-of-tax first stage: $\\ln\\big((M^*-t_2)/(P Y - t_1)\\big)$ replaces $\\ln(M^*/PY)$ ($PY$ nominal gross output). Same estimator (\\texttt{estimate\\_prod\\_fn\\_bounds}).")
colnames(tt_obj) <- c("Industry", rep(c("Paper", "Gross", "Net"), 3))
tt_obj <- tt_obj |> group_tt(j = list("$\\hat\\beta$" = 2:4, "$\\hat\\alpha_K$" = 5:7, "$\\hat\\alpha_L$" = 8:10)) |> group_tt(i = idx) |> style_tt(i = "notes", fontsize = 0.7)
render_png_tt_tbl(tt_obj, "1471-net-pf-params-table"); cat("Saved: Paper/tbls/1471-net-pf-params-table.png\n")
print(tb)
