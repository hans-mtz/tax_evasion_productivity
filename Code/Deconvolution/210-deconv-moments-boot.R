## Load Packages and Data ----------------------
library(tidyverse)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/intermediates.RData")

## Setting seed for Reproducibility --------------
set.seed(66636)

## Testing ----------------------------------

# sampled_plants_by_crop <-colombia_data_frame %>%
#     ungroup() %>%
#     mutate(
#         Corp = ifelse(juridical_organization==3,"Corp","Other")
#     ) %>%
#     filter(
#         # sic_3 == 322,
#         is.finite(log_mats_share)
#         # jurical_organization == 3
#     ) %>%
#     group_by(sic_3,Corp) %>%
#     reframe(
#         plant = sample(unique(plant), replace = TRUE)#,
#         # .by = Corp
#     ) #%>%

# print(sampled_plants_by_crop) |> View()
#     # select(plant,year,Corp) 
#     #|> expand.grid(year = 81:91, plant=_ ) %>%
# sampled_plants_by_crop %>% left_join(
#         colombia_data_frame,
#         by = c("plant","sic_3"),
#         relationship = "many-to-many"
#     ) %>% View()

# colombia_data_frame %>%
#     ungroup() %>%
#     mutate(
#         Corp = ifelse(juridical_organization==3,"Corp","Other")
#     ) %>%
#     filter(
#         # sic_3 == 322,
#         is.finite(log_mats_share)
#         # jurical_organization == 3
#     ) %>%
#     group_by(sic_3,Corp) %>%
#     reframe(
#         plant = sample(unique(plant), replace = TRUE)#,
#         # .by = Corp
#     ) %>%
#     pull(plant) |> expand.grid(year = 81:91, plant=_ ) %>%
#     left_join(
#         colombia_data_frame,
#         relationship = "many-to-one"
#     ) %>% View()

# colombia_data_frame %>%
#     ungroup() %>%
#     filter(
#         # sales > 0,
#         is.finite(log_mats_share)
#         # !is.na(k),!is.na(l),!is.na(m)
#         # n_sic > 500
#     ) %>%
#     mutate(
#         Corp = ifelse(juridical_organization==3,"Corp","Other")
#     ) %>%
#     filter(
#         sic_3 == 322#,
#         # jurical_organization == 3
#     ) %>%
#     ungroup() %>%
#     group_by(Corp) %>%
#     summarise(
#         plant = length(unique(plant)),
#         N = n()
#     )

## Function -----------------------------

# resample_by_group<-function(data,...){

#     sampled_plants_by_corp <- data %>%
#         ungroup() %>%
#         mutate(
#             Corp = ifelse(juridical_organization==3,"Corp","Other")
#         ) %>%
#         group_by(...,Corp) %>%
#         reframe(
#             plant = sample(unique(plant), replace = TRUE)
#         ) 

#     resampled_data <-sampled_plants_by_corp %>% 
#         left_join(
#             data,
#             by = c("plant","sic_3"),
#             relationship = "many-to-many"
#         )
#     return(resampled_data)
# }

# resample_by_group(colombia_data_frame,sic_3) |> View()

## Declaring variables ----------------------------

mc_cores <- detectCores()-2
top_evading_inds <- tax_ev_test_tbl %>% arrange(desc(log_mats_share)) %>% pull(sic_3)
run_vars<-expand.grid(inds=top_evading_inds[1:5],input="log_mats_share", stringsAsFactors = FALSE)


## Estimating ------------------------------

col_df <- colombia_data_frame %>%
    select(
        sic_3, year, plant, juridical_organization, log_mats_share
    )

tax_ev_mmt_deconv<-mapply(
    mmt_deconv,
    run_vars$inds,
    run_vars$input,
    MoreArgs = list(data=col_df),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)
tax_ev_mmt_deconv_tbl<-do.call(rbind,tax_ev_mmt_deconv)

## Bootstrapping ---------------------------

boot_tax_ev_mmt <- mclapply(
    1:200,
    function(i){
        resampled_data <- resample_by_group(col_df,sic_3)
        temp<-mapply(
            mmt_deconv,
            run_vars$inds,
            run_vars$input,
            MoreArgs = list(data=resampled_data),
            SIMPLIFY = FALSE
        )
        tbl_out<-do.call(rbind,temp)
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(tbl_out)
    },
    mc.cores = mc_cores
)

## Collecting Bootstrap Results ----------------

# load("Code/Products/boot_tax_ev_mmt.RData")

tax_ev_boot_tbl <- do.call(rbind,boot_tax_ev_mmt) %>%
    left_join(
        tax_ev_mmt_deconv_tbl[,c("sic_3","mean_evasion")],
        by="sic_3"#,
        # relantionship="many-to-one"
    ) %>% 
    mutate(
        bc_boot = mean_evasion.x-mean_evasion.y
    ) %>%
    group_by(sic_3) %>%
    reframe(
        value = quantile(bc_boot, probs=c(0.975,0.025)),
        probs = c(0.975,0.25),
        ci_name = c("LCI","UCI"),
        CI = mean(mean_evasion.y)-value,
        mu = max(mean_evasion.y)
    ) %>%
    select( sic_3, mu, ci_name, CI) %>%
    pivot_wider(
        names_from = ci_name,
        values_from = CI
    ) %>%
    arrange(desc(mu))


## Save ----------------------------------------

save(
    boot_tax_ev_mmt, tax_ev_mmt_deconv_tbl,tax_ev_boot_tbl,top_evading_inds,run_vars,
    file="Code/Products/boot_tax_ev_mmt.RData"
)
