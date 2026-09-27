## PRODUCT: Code/Products/1521-np-deconv-selected.RData := deconvolution of u = ln(M*/M) on UNINCORPORATED firms for the
## industries selected by an ex-ante rule, on EXACTLY the test's sample. Decided 2026-09-26:
##   RULE: deconvolve every industry whose 99% sharp test-inversion region for mu = E[V] (1510) is non-empty and excludes 0
##         (TS(0) > chi2_{1,0.99} = 6.63 with mu_hat > 0). Selects 322, 321, 331, 313, 324, 369, 342; next are 352 and 351
##         at TS(0) = 4.1 and 3.9, and the conservative test at 1% selects the same set. Results reported whatever they are.
##   SAMPLE: the test's (1510): raw panel, finite y, k, l, m; net-of-tax share > 5%; juridical organization codes 6-9 dropped;
##           NO 75% top cut for 369 (first_stage_panel_me hard-codes it and reads it from its own saved environment, so it
##           is lifted there -- see 1518).
## Estimator unchanged (292/1515): f_eps from corporations' residuals, penalized B-spline logspline, same lambda/knots
## rule, same seed. Two-tax (net-of-tax) log materials share.
library(tidyverse); library(splines); library(statmod); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata")   # first_stage_panel_me
load("Code/Products/colombia_data.RData")                                          # raw panel
fenv <- new.env(); load("Code/Products/np-deconv-funs.RData", envir = fenv)
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv
assign("upper_threshold_cut", Inf, envir = environment(first_stage_panel_me))
threshold_cut <- 0.05

selected <- c("322", "321", "331", "313", "324", "369", "342")
run_one <- function(s) {
    set.seed(557788)
    base <- colombia_data_frame %>% ungroup() %>%
        filter(sic_3 == as.numeric(s), !juridical_organization %in% 6:9,
               is.finite(y), is.finite(k), is.finite(l), is.finite(m)) %>%
        mutate(t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
               log_mats_share_net = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1)))) %>%
        filter(is.finite(log_mats_share_net), log_mats_share_net > log(threshold_cut))
    fs <- first_stage_panel_me(sic = as.numeric(s), var = "log_mats_share_net", r_var = "materials", data = base)
    jo <- base %>% transmute(plant = as.character(plant), year = as.character(year), juridical_organization) %>% distinct()
    fs_u <- fs; fs_u$inter <- "log_mats_share_net"
    fs_u$data <- fs$data %>% select(-any_of("juridical_organization")) %>%
        mutate(plant = as.character(plant), year = as.character(year)) %>%
        left_join(jo, by = c("plant", "year")) %>% filter(juridical_organization != 3)
    cat(s, ": rows", nrow(fs$data), "| unincorporated", nrow(fs_u$data), "| beta", round(fs$beta, 3),
        "| mean V (uninc)", round(mean(fs_u$data$cal_V), 3), "\n")
    fit <- fenv$estimate_np_theta(fs_u, fenv$np_pdf(fs), fenv$gl, lambda = fenv$lambda, parallel = FALSE)
    list(fit = fit, fs = fs, n_uninc = nrow(fs_u$data), mean_V = mean(fs_u$data$cal_V), sd_V = sd(fs_u$data$cal_V))
}
res_sel <- mclapply(selected, run_one, mc.cores = length(selected))
names(res_sel) <- selected
deconv_sel_list <- lapply(res_sel, `[[`, "fit"); names(deconv_sel_list) <- paste(selected, "log_mats_share_net")
deconv_sel_stats <- fenv$get_stats.list(deconv_sel_list)
print(deconv_sel_stats)
save(res_sel, deconv_sel_list, deconv_sel_stats, selected, file = "Code/Products/1521-np-deconv-selected.RData")
cat("Saved: Code/Products/1521-np-deconv-selected.RData\n")
