# %% Load data and packages ---------------
library(tidyverse)
library(parallel)

# library(fixest)
# library(modelsummary)
# library(tinytable)
# load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/intermediates.RData")

## %% Setting up vars and seed for reproducibility --------------

set.seed(66636)
B <- 250 #Number of bootstrap replicates
mc_cores <- detectCores()-1 #Number of cores for parallel processing

## %% Bootstrap test for deconvolution ----------------


col_df <- test_data %>%
    filter(
        log_mats_share > log(threshold_cut)
    )

boot_tax_ev_2t2s <- mclapply(
    1:B,
    function(i){
        resampled_data <- resample_by_group(test_data,sic_3)
        temp<-mapply(
            test_ev_2t_2smpl,
            sic=rv_trim$inds,
            var=rv_trim$inter,
            # cond=rv_trim$condition,
            MoreArgs = list(
                data = resampled_data#,
                # cond = quote(juridical_organization != 3)
            ),
            SIMPLIFY = FALSE
        )
        tbl_out<-do.call(rbind,temp)
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(tbl_out)
    },
    mc.cores = mc_cores
)


bayboot_tax_ev_2ttst <- mclapply(
    1:B,
    function(i){
        resampled_data <- bayesian_sampling(col_df,sic_3)
        temp<-mapply(
            test_ev_2t,
            sic=rv_cond$inds,
            var=rv_cond$inter,
            # cond=rv_trim$condition,
            MoreArgs = list(data = resampled_data),
            SIMPLIFY = FALSE
        )
        tbl_out<-do.call(rbind,temp)
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(tbl_out)
    },
    mc.cores = mc_cores
)

## %% Saving bootstrap results ----------------

save(
    bayboot_tax_ev_2ttst, boot_tax_ev_2t2s,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)

## %% two sample sampling bootstrap test for deconvolution ----------------

boot_tax_ev_2ttst <- mclapply(
    1:B,
    function(i){
        resampled_data <- resample_by_group(col_df,sic_3)
        temp<-mapply(
            test_ev_2t,
            sic=rv_trim$inds,
            var=rv_trim$inter,
            # cond=rv_trim$condition,
            MoreArgs = list(data = resampled_data),
            SIMPLIFY = FALSE
        )
        tbl_out<-do.call(rbind,temp)
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(tbl_out)
    },
    mc.cores = mc_cores
)
## %% Saving bootstrap results ----------------

save(
    boot_tax_ev_2ttst, bayboot_tax_ev_2ttst, boot_tax_ev_2t2s,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)

## %% Load the saved bootstrap results ----------------

load("Code/Products/boot_tax_ev_2ttst.RData")

fmt_tbl <- function(boot_l, smpl_l){
    tbl <- boot_l |> do.call(rbind,args=_) %>%
    select(-c("rej_rule", "test_result")) %>%
    as_tibble() %>%
    left_join(
        do.call(rbind, smpl_l) %>% select(-c("rej_rule", "test_result")),
        by = c("sic_3", "intermediates"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        bc_mean_V = mean_V - mean_V.t0
    ) %>%
    group_by(sic_3, intermediates) %>%
    reframe(
        # val_mean_V = quantile(bc_mean_V, c(0.975, 0.025)),
        val_mean_V = quantile(bc_mean_V, c(0.95, 0.05)),
        probs = c(0.975, 0.025),
        CI = c("LCI", "UCI"),
        CI_mean_V = max(mean_V.t0)- val_mean_V,
        coeff_mean_V = max(mean_V.t0),
        coeff_se = 2*max(se.t0)-mean(se),
        prob = ecdf(bc_mean_V)(max(mean_V.t0)),
        p_val = 2*min(
            # prob >= 0.5,
            1 - prob,
            prob
        )
    ) %>%
    pivot_wider(
        id_cols = c(sic_3, intermediates, coeff_mean_V, p_val),
        names_from = CI,
        values_from = c(CI_mean_V),
        names_prefix = "CI_"
    ) %>%
    mutate(
        stars = case_when(
            p_val <= 0.01 ~ "***",
            p_val <= 0.05 ~ "**",
            p_val <= 0.1 ~ "*",
            TRUE ~ ""
        ),
        CI_mean_V = glue::glue("[{round(CI_LCI, 2)}, {round(CI_UCI, 2)}]"),
        coeff_mean_V= glue::glue("{round(coeff_mean_V, 2)}{stars}")
    ) %>%
    select(
        sic_3, intermediates, coeff_mean_V, CI_mean_V
    ) %>%
    pivot_longer(
        cols = starts_with(c("CI","coeff")),
        names_to = c("type","var"),
        values_to = c("value"),
        names_pattern = "^(CI|coeff)_(.*)"
    ) %>%
    pivot_wider(
        names_from = var,
        values_from = value
    ) %>%
    arrange(
        intermediates, sic_3, desc(type)
    )
    return(tbl)
}

boot_tax_ev_2ttst |> do.call(rbind,args=_) %>%
    select(-c("rej_rule", "test_result")) %>%
    as_tibble() %>%
    left_join(
        do.call(rbind, tax_ev_test_2t) %>% select(-c("rej_rule", "test_result")),
        by = c("sic_3", "intermediates"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        bc_mean_V = mean_V - mean_V.t0
    ) %>%
    group_by(sic_3, intermediates) %>%
    reframe(
        # val_mean_V = quantile(bc_mean_V, c(0.975, 0.025)),
        val_mean_V = quantile(bc_mean_V, c(0.95, 0.05)),
        probs = c(0.975, 0.025),
        CI = c("LCI", "UCI"),
        CI_mean_V = max(mean_V.t0)- val_mean_V,
        coeff_mean_V = max(mean_V.t0),
        coeff_se = 2*max(se.t0)-mean(se),
        prob = ecdf(bc_mean_V)(max(mean_V.t0)),
        p_val = 2*ifelse(
            prob > 0.5,
            1 - prob,
            prob
        ) # Equal tail booststrap p-value 
    ) %>%
    pivot_wider(
        id_cols = c(sic_3, intermediates, coeff_mean_V, p_val),
        names_from = CI,
        values_from = c(CI_mean_V),
        names_prefix = "CI_"
    ) %>%
    mutate(
        stars = case_when(
            p_val < 0.01 ~ "***",
            p_val < 0.05 ~ "**",
            p_val < 0.1 ~ "*",
            TRUE ~ ""
        ),
        CI_mean_V = glue::glue("[{round(CI_LCI, 2)}, {round(CI_UCI, 2)}]"),
        coeff_mean_V= glue::glue("{round(coeff_mean_V, 2)}{stars}")
    ) %>%
    select(
        sic_3, intermediates, coeff_mean_V, CI_mean_V
    ) %>%
    pivot_longer(
        cols = starts_with(c("CI","coeff")),
        names_to = c("type","var"),
        values_to = c("value"),
        names_pattern = "^(CI|coeff)_(.*)"
    ) %>%
    pivot_wider(
        names_from = var,
        values_from = value
    ) %>%
    arrange(
        intermediates, sic_3, desc(type)
    )

boot_2ttst_tbl<-fmt_tbl(
    boot_tax_ev_2ttst,
    tax_ev_test_2t
)

bayboot_2ttst_tbl<-fmt_tbl(
    bayboot_tax_ev_2ttst,
    tax_ev_test_2t
)

fmt_tbl(
    boot_tax_ev_2ttst,
    tax_ev_test_2t
) |> View()

fmt_tbl(
    bayboot_tax_ev_2ttst,
    tax_ev_test_2t
) |> View()

## %% Saving bootstrap results ----------------

save(
    boot_tax_ev_2ttst, bayboot_tax_ev_2ttst,
    boot_2ttst_tbl, bayboot_2ttst_tbl,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)


