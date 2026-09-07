## %% load packages and data ---------------
library(tidyverse)
library(parallel)
load("Code/Products/test_data.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/np-deconv-funs.RData")
load("Code/Products/global_vars.RData")

## %% Setting up vars and seed for reproducibility --------------

set.seed(66636)
B <- 250 #Number of bootstrap replicates
mc_cores <- detectCores()-1 #Number of cores for parallel processing

## %% Testing for the presence of Tax Evasion ---------------------

test_data %>%
    filter(
        # log_mats_share < log(0.75),
        log_mats_share > log(threshold_cut)
    ) %>%
    test_ev_2t_2smpl_cond(369, "log_mats_share", quote(juridical_organization!=3),data=.)

test_data %>%
    filter(
        # log_mats_share < log(0.75),
        log_mats_share > log(threshold_cut)
    ) %>%
    test_ev_2t_2smpl(369, "log_mats_share", data=.)


(tax_ev_tst_369 <-test_data %>%
    filter(
        log_mats_share < log(0.75),
        log_mats_share > log(threshold_cut)
    ) %>%
    test_ev_2t_2smpl(369, "log_mats_share", data=.)
)

boot_tax_ev_369 <- mclapply(
    1:B,
    function(i){
        resampled_data <- resample_by_group(test_data,sic_3)

        tbl_out<-resampled_data %>%
            filter(
                log_mats_share < log(0.75),
                log_mats_share > log(threshold_cut)
            ) %>%
            test_ev_2t_2smpl(369, "log_mats_share", data=.)
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(tbl_out)
    },
    mc.cores = mc_cores
)

(tax_ev_369_tbl <- render_tbl.tbl(boot_tax_ev_369, tax_ev_tst_369))
## %% First Stage Estimation ---------------------

conditions <- list(
    all=quote(TRUE),
    corps=quote(juridical_organization == 3),
    others=quote(juridical_organization != 3)
    # exporters=quote(share_exports > 0.1),
    # importers=quote(share_imports > 0.1),
    # importers_mats=quote(share_imports_materials > 0.1)
)

v369 <- expand.grid(
    # sic_3 = union(top_5_ev_inds_mag[1:5], c(311, 321, 322, 331, 381)),
    sic_3 = 369, #c(311,312,313,321,322,323,324,331,332),
    # inter = c("log_share","log_mats_share","log_deductible_intermediates_share"),
    inter = "log_mats_share",
    condition = conditions,
    stringsAsFactors = FALSE
)

inter_named <- c(
    "log_mats_share" = "materials",
    "log_deductible_intermediates_share" = "deductible_intermediates",
    "log_share" = "intermediates" 
)

v369$r_input <- inter_named[v369$inter]
v369

### %% Estimation -------------------------------

do_fs_cond(369, "log_mats_share", "materials", quote(juridical_organization!=3), data=test_data %>%
    filter(
        log_mats_share < log(0.75),
        log_mats_share > log(threshold_cut)
    ))

fs_369_list<-mcmapply(
    do_fs_cond, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    # sic=ev_me_v$sic_3,
    # var=ev_me_v$inter,
    # r_var=ev_me_v$r_input,
    cond=v369$condition,
    MoreArgs = list(
        sic=369,
        var="log_mats_share",
        r_var="materials",
        data=test_data%>%
            filter(
                log_mats_share < log(0.75),
                log_mats_share > log(threshold_cut)
    )),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

fs_369_tbl <- get_table(fs_369_list)

fs_369_tbl$good_guys <- names(v369$condition)
fs_369_tbl<-fs_369_tbl %>%
    pivot_wider(
        names_from = good_guys,
        values_from = c(m, `err sd`),
        names_sep = " - "
    ) %>%
    mutate(
        intermediate = factor(intermediate, 
            levels = c("intermediates", "materials", "deductible_intermediates"),
            labels = c("Intermediates", "Materials", "Deductibles")
        ),
        # sic_3 = factor(sic_3, 
        #     levels = union(top_5_ev_inds_mag[1:5], c(311, 321, 322, 331, 381))
        # )
    ) %>% arrange(sic_3, intermediate)

fs_369_tbl

# (fs_369 <- first_stage_panel_me(369, "log_mats_share", "materials", data=test_data %>%
#     filter(
#         log_mats_share < log(0.75),
#         log_mats_share > log(threshold_cut)
#     )))
### %% Bootstrap FS -------------------------

boot_fs_369_list<-mclapply(
    1:R,
    function(i){

        temp_data <- resample_by_group(test_data, sic_3)

        tmp_list<-mcmapply(
            do_fs_cond, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
            # sic=ev_me_v$sic_3,
            # var=ev_me_v$inter,
            # r_var=ev_me_v$r_input,
            cond=v369$condition,
            MoreArgs = list(
                sic=369,
                var="log_mats_share",
                r_var="materials",
                data=temp_data%>%
                    filter(
                        log_mats_share < log(0.75),
                        log_mats_share > log(threshold_cut)
            )),
            SIMPLIFY = FALSE,
            mc.cores = mc_cores
        )

        tmp_tbl <- get_table(tmp_list)

        tmp_tbl$good_guys <- names(v369$condition)
        tmp_tbl<-tmp_tbl %>%
            pivot_wider(
                names_from = good_guys,
                values_from = c(m, `err sd`),
                names_sep = " - "
            ) %>%
            mutate(
                intermediate = factor(intermediate, 
                    levels = c("intermediates", "materials", "deductible_intermediates"),
                    labels = c("Intermediates", "Materials", "Deductibles")
                ),
                # sic_3 = factor(sic_3, 
                #     levels = union(top_5_ev_inds_mag[1:5], c(311, 321, 322, 331, 381))
                # )
            ) %>% arrange(sic_3, intermediate)

        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(tmp_tbl)
    },
    mc.cores = mc_cores
)

ielas_369_tbl <-render_boot_elas_tbl(boot_fs_369_list, fs_369_tbl)

main_369_tbl <- ielas_369_tbl %>%
    mutate(
        type = replace(type, type == "m", "coeff"),
        sic_3 = as.numeric(sic_3)
    ) %>%
    left_join(
        tax_ev_369_tbl,
        by = c("sic_3", "type")
    )

main_369_tbl

## %% Save preliminary results ---------------------

save(
    main_369_tbl, ielas_369_tbl, fs_369_list, tax_ev_369_tbl,
    file = "Code/Products/900-369-trim.RData"
)

## %% Tax Evasion Density Estimation ---------------------
load("Code/Products/900-369-trim.RData")

fs_369_2trim_ls <- first_stage_panel_me(369, "log_mats_share", "materials", data=test_data %>%
    filter(
        log_mats_share < log(0.75),
        log_mats_share > log(threshold_cut)
    ))

np_decov_369 <- estimate_np_theta(fs_369_2trim_ls, np_pdf(fs_369_2trim_ls), gl, lambda = lambda, parallel = FALSE)

stats_369_ev_density <- get_stats(np_decov_369$theta, np_decov_369$params)
stats_369_ev_density_6 <- get_stats.m(np_decov_369$theta, np_decov_369$params)

## %% Save preliminary results ---------------------

save(
    main_369_tbl, ielas_369_tbl, fs_369_list, tax_ev_369_tbl,
    fs_369_2trim_ls, np_decov_369, stats_369_ev_density,
    stats_369_ev_density_6,
    file = "Code/Products/900-369-trim.RData"
)

## %% Omega Density Estimation ---------------------

## %% Productivity Density Estimation ---------------------


## %% Saving Results ---------------------

