## PRODUCT: Code/Products/1515-np-deconv-unincorp-net.RData := non-parametric deconvolution of u = ln(M*/M) on
## UNINCORPORATED firms, two-tax (net-of-tax) log materials share. TWO-TAX COPY of 292-np-deconv-unincorp.R (2026-09-26):
## same estimator (f_eps from corporations' residuals, penalized B-spline logspline, same lambda/knots rule, same seed),
## only the first-stage objects swapped: fs_net_ls from 1501-fs-net.RData (first_stage_panel_me on the net share)
## instead of fs_list (gross). Sample: 1501's (test-sample rules; 369 also has the 0.75 upper share cut, 17 rows).
## Output: unincorp_np_deconv_list_net, unincorp_np_stats_df_net
library(splines); library(statmod); library(parallel); library(dplyr)

load("Code/Products/np-deconv-funs.RData")   # functions, gl, lambda, n_knots, pspline_degree (fs_list unused)
load("Code/Products/1501-fs-net.RData")      # fs_net_ls
load("Code/Products/test_data.RData")        # juridical_organization, to split corps / unincorporated
set.seed(557788)

five <- c("331", "322", "369", "313", "321")
jo <- test_data %>% ungroup() %>% distinct(plant, year, juridical_organization) %>%
    mutate(plant = as.character(plant), year = as.character(year))   # 1501 stores plant/year as factors

run_one <- function(s) {
    fs <- fs_net_ls[[s]]; fs$inter <- "log_mats_share_net"
    eps_pdf <- np_pdf(fs)   # f_eps from corporations' residuals (non-NA epsilon rows), as in 292
    fs_u <- fs
    fs_u$data <- fs$data %>% select(-any_of("juridical_organization")) %>%
        mutate(plant = as.character(plant), year = as.character(year)) %>%
        left_join(jo, by = c("plant", "year")) %>% filter(juridical_organization != 3)
    cat("Estimating", s, "on", nrow(fs_u$data), "unincorporated obs\n")
    estimate_np_theta(fs_u, eps_pdf, gl, lambda = lambda, parallel = FALSE)
}

unincorp_np_deconv_list_net <- mclapply(five, run_one, mc.cores = length(five))
names(unincorp_np_deconv_list_net) <- paste0(five, " log_mats_share_net")
unincorp_np_stats_df_net <- get_stats.list(unincorp_np_deconv_list_net)
print(unincorp_np_stats_df_net)
save(unincorp_np_deconv_list_net, unincorp_np_stats_df_net, file = "Code/Products/1515-np-deconv-unincorp-net.RData")
cat("Saved: Code/Products/1515-np-deconv-unincorp-net.RData\n")
