## PRODUCT: Code/Products/1518-deconv-369-check.RData := diagnostic (2026-09-26): deconvolution of u for industry 369 on the
## FULL sample (no 75% upper share cut) with juridical organization codes 6-9 dropped, two-tax (net-of-tax) share.
## Why: 369's deconvolved mean u (0.059 in 1515, skewness 22) is far below its test mean of V (~0.19). The top trim
## (13 unincorporated obs, share >= 75%) and codes 6-9 (14 code-6 obs) do not overlap and move mean V by <= 0.02, so this
## checks whether the fit, not the sample, is the problem. Same estimator as 292/1515; first stage as 1501 minus the cut.
library(tidyverse); library(splines); library(statmod); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata")   # first_stage_panel_me
load("Code/Products/colombia_data.RData")   # RAW panel. NOT 931.1's df: that is the approved production sample and
                                            # already carries 369's 75% top cut (first attempt used it and just reproduced 1515)
fenv <- new.env(); load("Code/Products/np-deconv-funs.RData", envir = fenv)          # np_pdf, estimate_np_theta, gl, lambda
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv   # helpers resolve in fenv (as in ch05-overreporting-ratio.R)
## first_stage_panel_me (021-deconv-funs.R) hard-codes the 75% cut for 369 and reads it from its OWN saved environment
## ("env-deconv"), so a global assignment has no effect: lift it there, for this diagnostic only.
assign("upper_threshold_cut", Inf, envir = environment(first_stage_panel_me))
set.seed(557788)

base <- colombia_data_frame %>% ungroup() %>%
    filter(sic_3 == 369, !juridical_organization %in% 6:9, is.finite(y), is.finite(k), is.finite(l), is.finite(m)) %>%
    mutate(t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
           log_mats_share_net = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1)))) %>%
    filter(is.finite(log_mats_share_net), log_mats_share_net > log(threshold_cut))     # NO upper cut
fs <- first_stage_panel_me(sic = 369, var = "log_mats_share_net", r_var = "materials", data = base)
jo <- base %>% transmute(plant = as.character(plant), year = as.character(year), juridical_organization) %>% distinct()
fs_u <- fs; fs_u$inter <- "log_mats_share_net"
fs_u$data <- fs$data %>% select(-any_of("juridical_organization")) %>%
    mutate(plant = as.character(plant), year = as.character(year)) %>%
    left_join(jo, by = c("plant", "year")) %>% filter(juridical_organization != 3)
cat("369 full sample, codes 6-9 dropped: rows", nrow(fs$data), "| unincorporated", nrow(fs_u$data),
    "| beta", round(fs$beta, 3), "| sd eps (corp)", round(fs$epsilon_sigma, 3),
    "| mean V (uninc)", round(mean(fs_u$data$cal_V), 3), "| sd V (uninc)", round(sd(fs_u$data$cal_V), 3), "\n")
eps_pdf <- fenv$np_pdf(fs)
fit_369 <- fenv$estimate_np_theta(fs_u, eps_pdf, fenv$gl, lambda = fenv$lambda, parallel = FALSE)
stats_369 <- fenv$get_stats.list(list(`369 log_mats_share_net full` = fit_369))
print(stats_369)
save(fs, fit_369, stats_369, file = "Code/Products/1518-deconv-369-check.RData")
cat("Saved: Code/Products/1518-deconv-369-check.RData\n")
