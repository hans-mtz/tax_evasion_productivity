## PRODUCT: Code/Products/1501-fs-net.RData := two-tax (net-of-tax) first stage for ALL industries, the input to the
## re-estimated stage 2 (plan in Research-log/log.md, 2026-09-26, step A1; model in Paper/sections/9999-tax-wedge.qmd).
## Same estimator as the approved first stage (930.1-fs-se-het.R: first_stage_panel_me, r_var = "materials"), but the
## materials share is built NET of sales taxes on both sides, over the same denominator (nominal gross output):
##   log_mats_share_net = log((nom_mats - t2) / (nom_gross_output - t1)),  t1 = tau_S * nom_sales, t2 = tau_P * nom_mats
## (definitions as in 1470-net-tax-first-stage-diag.R). Sample rule applied to the NET share only (share > threshold_cut;
## 369 also below upper_threshold_cut), so this is the production sample, not 1470's gross-and-net common sample.
## Nothing approved is overwritten: reads 931.1-fs-se-het.RData, writes only 1501-* files.
library(tidyverse); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata")
load("Code/Products/931.1-fs-se-het.RData")   # df (raw panel + gross cal_V/cal_W/epsilon), fs_all_ls (approved gross first stage)
mc_cores <- max(1, detectCores() - 2)

base <- df %>% select(-any_of(c("cal_V", "cal_W", "epsilon"))) %>%
    mutate(t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
           log_mats_share_net = log((nom_mats - t2) / (nom_gross_output - t1)))
lo <- log(threshold_cut)
ok <- is.finite(base$log_mats_share_net) & base$log_mats_share_net > lo &
    ifelse(base$sic_3 == 369, base$log_mats_share_net < log(upper_threshold_cut), TRUE)
wip_net <- base[ok, ]
cat(sprintf("Rows: raw panel %d | approved gross sample %d | net sample %d\n",
            nrow(base), sum(is.finite(df$cal_V)), nrow(wip_net)))

fs_net_ls <- mcmapply(first_stage_panel_me, sic = unique(wip_net$sic_3),
                      MoreArgs = list(var = "log_mats_share_net", r_var = "materials", data = wip_net),
                      SIMPLIFY = FALSE, mc.cores = mc_cores)
names(fs_net_ls) <- unique(wip_net$sic_3)
data_net_ls <- lapply(unique(wip_net$sic_3), \(x) fs_net_ls[[as.character(x)]]$data)
df_net <- wip_net %>%
    left_join(do.call(rbind, data_net_ls) %>% dplyr::select(sic_3, year, plant, cal_V, cal_W, epsilon),
              by = c("sic_3", "year", "plant"))
save(df_net, fs_net_ls, data_net_ls, file = "Code/Products/1501-fs-net.RData")
cat("Saved: Code/Products/1501-fs-net.RData\n")

## Comparison with the approved gross first stage, by industry
cmp <- map_dfr(names(fs_net_ls), function(s) {
    n <- fs_net_ls[[s]]; g <- fs_all_ls[[s]]
    if (is.null(n$beta)) return(tibble(sic_3 = s))
    tibble(sic_3 = s, beta_gross = if (is.null(g)) NA else g$beta, beta_net = n$beta,
           sd_eps_gross = if (is.null(g)) NA else g$epsilon_sigma, sd_eps_net = n$epsilon_sigma,
           n_net = nrow(n$data))
}) %>% mutate(d_beta = beta_net - beta_gross)
options(width = 200); print(cmp %>% mutate(across(where(is.numeric), \(x) round(x, 3))), n = 40)
cat(sprintf("\nd_beta (net - gross): median %.3f, range [%.3f, %.3f]; industries: %d\n",
            median(cmp$d_beta, na.rm = TRUE), min(cmp$d_beta, na.rm = TRUE), max(cmp$d_beta, na.rm = TRUE), sum(!is.na(cmp$d_beta))))
