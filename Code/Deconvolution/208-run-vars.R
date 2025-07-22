## %% Loading libraries ----------------------------
library(parallel)
load("Code/Products/intermediates.RData")

## %% Declaring variables ----------------------------
J <- 5
mc_cores <- detectCores()-2
gnr_inds <- c( 311, 321, 322, 331, 381)
inds <- union(top_5_ev_inds_mag[1:J], gnr_inds)
inputs <- c("log_mats_share","log_deductible_intermediates_share")
run_vars<-expand.grid(inds=inds,input=inputs, stringsAsFactors = FALSE)

## %% Saving variables ---------------------

save(
    run_vars, mc_cores,
    file = "Code/Products/run-vars.RData"
)