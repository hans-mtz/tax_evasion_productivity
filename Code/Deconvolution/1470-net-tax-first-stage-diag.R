## Phase 1 diagnostic (2026-09-21): how much does the first stage change if the log materials share is built
## NET of sales taxes? (Paper/sections/9999-tax-wedge.qmd: eq-two-tax-eps-net). Nothing approved is overwritten:
## reads 931.1-fs-se-het.RData, writes only 1470-* files.
##   gross : log(nom_mats / nom_gross_output)                       -- current stage 1 (baseline, re-run on the common sample)
##   net   : log((nom_mats - t2) / (nom_gross_output - t1))         -- net of tax; SAME denominator (nominal gross output) as stage 1.
##           t1 = tau_S*nom_sales (sales tax paid on sales), t2 = tau_P*nom_mats (sales tax paid on purchases), by the definitions
##           tau_S = t1/nom_sales, tau_P = t2/s10 (10_data_wrangling.R:130-131); df carries the rates, not t1/t2.
## (An earlier version also ran gross-materials/nom_sales to isolate a denominator change; dropped 2026-09-21 -- nominal gross output is the correct denominator.)
## Same rows in both specs (common sample: both shares finite and > log(threshold_cut), 369 also < upper cut).
library(tidyverse); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata")
load("Code/Products/931.1-fs-se-het.RData")   # df (raw panel + current cal_V/cal_W/epsilon), fs_all_ls (current stage 1)
mc_cores <- max(1, detectCores() - 2)

base <- df %>% select(-any_of(c("cal_V", "cal_W", "epsilon"))) %>%
    mutate(t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
           log_mats_share_net = log((nom_mats - t2) / (nom_gross_output - t1)),
           wedge = log_mats_share - log_mats_share_net)   # gross = net + wedge, wedge = -ln(1-tau_P) + ln(1 - t1/gross_output)
vars <- c("log_mats_share", "log_mats_share_net")
lo <- log(threshold_cut)
ok <- Reduce(`&`, lapply(vars, \(v) is.finite(base[[v]]) & base[[v]] > lo &
                           ifelse(base$sic_3 == 369, base[[v]] < log(upper_threshold_cut), TRUE)))
cat(sprintf("Rows in raw panel: %d | current-rule rows (gross): %d | common sample: %d | dropped by net domain (net<=0, NA tau_S, share<=cut): %d\n",
            nrow(base), sum(is.finite(base$log_mats_share) & base$log_mats_share > lo), sum(ok),
            sum(is.finite(base$log_mats_share) & base$log_mats_share > lo) - sum(ok)))
common <- base[ok, ]

run_spec <- function(var) {
    ls <- mcmapply(first_stage_panel_me, sic = unique(common$sic_3),
                   MoreArgs = list(var = var, r_var = "materials", data = common), SIMPLIFY = FALSE, mc.cores = mc_cores)
    names(ls) <- unique(common$sic_3); ls
}
fs_specs <- setNames(lapply(vars, run_spec), c("gross", "net"))
save(fs_specs, file = "Code/Products/1470-fs-net.RData")

sk <- function(x) { x <- x[is.finite(x)]; mean((x - mean(x))^3) / sd(x)^3 }
tab <- map_dfr(names(fs_specs$gross), function(s) {
    g <- fs_specs$gross[[s]]; n <- fs_specs$net[[s]]
    if (is.null(g$data) || is.null(n$data)) return(NULL)
    cd <- common %>% filter(sic_3 == as.numeric(s), juridical_organization == 3)
    tibble(sic_3 = s, n_corp = nrow(cd),
           beta_gross = g$beta, beta_net = n$beta, d_beta_net_vs_gross = n$beta - g$beta,
           sd_eps_gross = g$epsilon_sigma, sd_eps_net = n$epsilon_sigma,
           skew_eps_gross = sk(g$data$epsilon), skew_eps_net = sk(n$data$epsilon),
           sd_wedge_corp = sd(cd$wedge, na.rm = TRUE),
           corr_eps_wedge = cor(g$data$epsilon, common$wedge[match(paste(g$data$plant, g$data$year), paste(common$plant, common$year))], use = "complete.obs"),
           median_V_unincorp_gross = median(g$data$cal_V[is.na(g$data$epsilon)]),
           median_V_unincorp_net   = median(n$data$cal_V[is.na(n$data$epsilon)]))
})
write.csv(tab, "Code/Products/1470-net-first-stage-diag.csv", row.names = FALSE)
options(width = 200); print(tab %>% mutate(across(where(is.numeric), \(x) round(x, 3))), n = 40)
cat("\nSummary across industries (median [min, max]):\n")
for (v in c("d_beta_net_vs_gross", "sd_eps_gross", "sd_eps_net", "sd_wedge_corp", "corr_eps_wedge"))
    cat(sprintf("  %-22s %.3f [%.3f, %.3f]\n", v, median(tab[[v]]), min(tab[[v]]), max(tab[[v]])))
