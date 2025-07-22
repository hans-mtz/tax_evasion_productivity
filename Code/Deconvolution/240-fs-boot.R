## %% load packages and data --------------------------

library(tidyverse)
library(parallel)
load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/run-vars.RData")
load("Code/Products/fs.RData") # run_vars
# load("Code/Products/intermediates.RData") #Top evading industries

## %% Set seed and data --------------------------

set.seed(66636)
R = 250 # Number of Bootstrap Replicates
mc_cores <- detectCores()-2
col_df <- test_data %>%
    filter(
        log_mats_share > log(threshold_cut)
    )
## %% Bootstrap GNR's First Stage --------------------------

boot_fs_list<-mclapply(
    1:R,
    function(i){
    resampled_data<-resample_by_group(col_df,sic_3)
    ### First Stage -------------------------
    fs_list_temp<-mapply(
        first_stage_panel, #sic_3,log_mats_share, juridical_organization, gross_output, year, plant, k, l
        run_vars[,"inds"],
        run_vars[,"input"],
        run_vars[,"r_input"],
        MoreArgs = list(data=resampled_data),
        SIMPLIFY = FALSE#,
        # mc.cores = mc_cores
    )

    names(fs_list_temp)<-paste(run_vars[,"inds"],run_vars[,"input"])

    if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}

    return(fs_list_temp)
    },
    mc.cores = mc_cores
)

## %% Save results ---------------------

save(
    boot_fs_list,
    file = "Code/Products/boot_fs.RData"
)

## %% Bootstrap GNR's First Stage --------------------------

boot_fs_me_list<-mclapply(
    1:R,
    function(i){
    resampled_data<-resample_by_group(col_df,sic_3)
    ### First Stage -------------------------
    fs_list_temp<-mapply(
        first_stage_panel_me, #sic_3,log_mats_share, juridical_organization, gross_output, year, plant, k, l
        run_vars[,"inds"],
        run_vars[,"input"],
        run_vars[,"r_input"],
        MoreArgs = list(data=resampled_data),
        SIMPLIFY = FALSE#,
        # mc.cores = mc_cores
    )

    names(fs_list_temp)<-paste(run_vars[,"inds"],run_vars[,"input"])

    if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}

    return(fs_list_temp)
    },
    mc.cores = mc_cores
)

## %% Save results ---------------------

save(
    boot_fs_list, boot_fs_me_list,
    file = "Code/Products/boot_fs.RData"
)

load("Code/Products/boot_fs.RData")

boot_fs_list
