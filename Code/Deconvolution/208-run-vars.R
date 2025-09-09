## %% Loading libraries ----------------------------
library(parallel)
library(dplyr)
load("Code/Products/intermediates.RData")
load("Code/Products/boot_test_comp_tbl.RData")

## %% Getting vars ----------------------------

top_5_boot_test <- pref_tax_ev_test_tbl %>%
    filter(
        type == "coeff"
    ) %>%
    arrange(desc(mean_V_log_mats_share)) %>%
    pull(sic_3)

## %% Declaring variables ----------------------------
J <- 5
mc_cores <- detectCores()-2
gnr_inds <- c( 311, 321, 322, 331, 381)
# inds <- union(top_5_ev_inds_mag[1:J], gnr_inds)
inds <- top_5_boot_test[1:5]
# inputs <- c("log_mats_share","log_deductible_intermediates_share"))
inputs <- "log_mats_share"
run_vars<-expand.grid(inds=inds,input=inputs, stringsAsFactors = FALSE)



## %% Saving variables ---------------------

save(
    run_vars, mc_cores, top_5_boot_test,
    file = "Code/Products/run-vars.RData"
)