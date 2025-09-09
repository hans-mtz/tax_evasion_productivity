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

### bayesian bootstrap, two samples --------------------

bayboot_tax_ev_2t2s <- mclapply(
    1:B,
    function(i){
        resampled_data <- bayesian_sampling(col_df,sic_3)
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

## %% Saving bootstrap results ----------------

save(
    bayboot_tax_ev_2t2s,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)

### Fixed number of corps bootstrap, two samples --------------------

boot_tax_ev_2t2s <- mclapply(
    1:B,
    function(i){
        resampled_data <- resample_by_group(col_df,sic_3)
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

## %% Saving bootstrap results ----------------

save(
    boot_tax_ev_2t2s, bayboot_tax_ev_2t2s,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)

## %% Basyeian bootstrap, one sample ----------------

bayboot_tax_ev_2ttst <- mclapply(
    1:B,
    function(i){
        resampled_data <- bayesian_sampling(col_df,sic_3)
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
    bayboot_tax_ev_2ttst, boot_tax_ev_2t2s,
    bayboot_tax_ev_2t2s,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)

## %% Fixed number of corps, one sample ----------------

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
    boot_tax_ev_2ttst, bayboot_tax_ev_2ttst, 
    boot_tax_ev_2t2s, bayboot_tax_ev_2t2s,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)

## %% Load the saved bootstrap results ----------------

# load("Code/Products/boot_tax_ev_2ttst.RData")

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
    boot_tax_ev_2t2s, bayboot_tax_ev_2t2s,
    boot_2ttst_tbl, bayboot_2ttst_tbl,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)


## %% Get tables --------------------------

load("Code/Products/boot_tax_ev_2ttst.RData")

boot_tax_ev_2t2s |> do.call(rbind, args=_) |> names()

tax_tst_2t2s_tbl <- do.call(rbind, tax_ev_test_2t_2smpl) %>% 
            filter( 
                cond == "juridical_organization != 3"
            ) %>%
            select(-c("rej_rule", "test_result","cond"))

tst_2t2s_tbl <- boot_tax_ev_2t2s |> do.call(rbind,args=_) %>%
    select(-c("rej_rule", "test_result")) %>%
    as_tibble() %>%
    left_join(
        tax_tst_2t2s_tbl,
        by = c("sic_3", "intermediates"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        bc_mean_V = mean_V - mean_V.t0
    ) %>%
    group_by(sic_3, intermediates) %>%
    reframe(
        # val_mean_V = quantile(bc_mean_V, c(0.975, 0.025)),
        val_mean_V = quantile(bc_mean_V, c(0.975, 0.025)),
        probs = c(0.975, 0.025),
        CI = c("LCI", "UCI"),
        CI_mean_V = max(mean_V.t0)- val_mean_V,
        coeff_mean_V = max(mean_V.t0),
        coeff_se = 2*max(se.t0)-mean(se),
        prob = ecdf(bc_mean_V)(max(mean_V.t0)),
        p_val = 1 - prob#2*min(1 - prob, prob) # Equal tail booststrap p-value 
    ) %>% #filter(sic_3 == 331, intermediates == "log_mats_share")%>%
    # select(sic_3, p_val, CI_mean_V, coeff_mean_V)
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
        names_from = c(var, intermediates),
        values_from = value
    ) %>%
    arrange(
        sic_3, desc(type)
    )# |> View()
tst_2t2s_tbl |> View()
boot_tax_ev_2t2s |> do.call(rbind,args=_) %>%
    select(-c("rej_rule", "test_result")) %>%
    as_tibble() %>%
    left_join(
        tax_tst_2t2s_tbl,
        by = c("sic_3", "intermediates"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        bc_mean_V = mean_V - mean_V.t0
    ) %>%
    group_by(sic_3, intermediates) %>%
    summarise(
        # val_mean_V = quantile(bc_mean_V, c(0.975, 0.025)),
        # val_mean_V = quantile(bc_mean_V, c(0.975, 0.025)),
        # probs = c(0.975, 0.025),
        # CI = c("LCI", "UCI"),
        # CI_mean_V = max(mean_V.t0)- val_mean_V,
        coeff_mean_V = max(mean_V.t0),
        coeff_se = 2*max(se.t0)-mean(se),
        prob = ecdf(bc_mean_V)(max(mean_V.t0)),
        p_val = 1 - prob,
            #prob
        #) # Upper tail booststrap p-value 
    ) %>%#filter(sic_3 == 331, intermediates == "log_mats_share")%>%
    # select(sic_3, p_val, CI_mean_V, coeff_mean_V)
    # pivot_wider(
    #     id_cols = c(sic_3, intermediates, coeff_mean_V, p_val),
    #     names_from = CI,
    #     values_from = c(CI_mean_V),
    #     names_prefix = "CI_"
    # ) %>%
    mutate(
        stars = case_when(
            p_val < 0.01 ~ "***",
            p_val < 0.05 ~ "**",
            p_val < 0.1 ~ "*",
            TRUE ~ ""
        ),
        se_V = glue::glue("({round(coeff_se, 2)})"),
        mean_V= glue::glue("{round(coeff_mean_V, 2)}{stars}")
    ) %>%   
    select(
        sic_3, intermediates, mean_V, se_V
    ) %>%
    pivot_longer(
        cols = c("mean_V","se_V"),
        names_to = "var",
        values_to = c("value"),
        names_pattern = "^(mean|se)_V"
    ) %>%
    pivot_wider(
        names_from = c(intermediates),
        values_from = value
    ) %>%
    arrange(
        sic_3#, desc(type)
    ) |> View()


## %% Saving bootstrap results ----------------

save(
    boot_tax_ev_2ttst, bayboot_tax_ev_2ttst,
    boot_tax_ev_2t2s, bayboot_tax_ev_2t2s,
    boot_2ttst_tbl, bayboot_2ttst_tbl,
    tst_2t2s_tbl,
    file = "Code/Products/boot_tax_ev_2ttst.RData"
)

## %% 4 ways to test ----------------

# 1: Fix # corps, evaluate on all
# 2: Fix # corps, evaluate on others
# (Bayesian bootstrap to ensure at least one corp is sampled)
# 3: Don't fix # corps, evaluate on all 
# 4: Don't fix # corps, evaluate on others

# During estimation with full sample (no boostrap), we use the full sample
# of corporations to estimate beta, but we can use either all the sample or
# onl the non-corporations to evaluate calgraphic V. 
# Therefore, full sample estimates only vary by evaluate on all or others.
# When bootstrppping, we can either fix the number of corporations or not, 
# and we can evaluate on all or on others.

# Full sample estimates
# all: tax_ev_test_trim (or tax_ev_test_2t)
# others: tax_ev_test_2t_2smpl 
#       (remember to filter selecting condition "juridical_organization != 3")

# Bootstrap estimates
# Fix corps, all: boot_tax_ev_2ttst
# Fix corps, others: boot_tax_ev_2t2s
# No fix, all: bayboot_tax_ev_2ttst
# No fix, others: bayboot_tax_ev_2t2s

# Combining to get tables 

# Fix corps, all: boot_tax_ev_2ttst with tax_ev_test_trim (or tax_ev_test_2t)
# Fix corps, others: boot_tax_ev_2t2s with tax_ev_test_2t_2smpl
# No fix, all: bayboot_tax_ev_2ttst with tax_ev_test_trim (or tax_ev_test_2t)
# No fix, others: bayboot_tax_ev_2t2s with tax_ev_test_2t_2smpl

# %% Get tables --------------------------

load("Code/Products/boot_tax_ev_2ttst.RData")

## %% function to get tables ----------------

render_tbl <- function(boot_l, smpl_l, cond=FALSE){
    if(cond){
        tmp_smpl_l <- do.call(rbind, smpl_l) %>% 
            filter( 
                cond == "juridical_organization != 3"
            ) %>%
            select(-c("rej_rule", "test_result","cond"))
    } else {
        tmp_smpl_l <- do.call(rbind, smpl_l) %>% 
                    select(-c("rej_rule", "test_result"))
    }

    tmp_tbl <- boot_l |> do.call(rbind,args=_) %>%
        select(-c("rej_rule", "test_result")) %>%
        as_tibble() %>%
        left_join(
            tmp_smpl_l,
            by = c("sic_3", "intermediates"),
            suffix = c("", ".t0"),
            relationship = "many-to-one"
        ) %>%
        mutate(
            bc_mean_V = mean_V - mean_V.t0
        ) %>%
        group_by(sic_3, intermediates) %>%
        reframe(
            # val_mean_V = quantile(bc_mean_V, c(0.975, 0.025)),
            val_mean_V = quantile(bc_mean_V, c(0.975, 0.025)),
            probs = c(0.975, 0.025),
            CI = c("LCI", "UCI"),
            CI_mean_V = max(mean_V.t0)- val_mean_V,
            coeff_mean_V = max(mean_V.t0),
            coeff_se = 2*max(se.t0)-mean(se),
            prob = ecdf(bc_mean_V)(max(mean_V.t0)),
            p_val = 1 - prob#2*min(1 - prob, prob) # Equal tail booststrap p-value 
        ) %>% #filter(sic_3 == 331, intermediates == "log_mats_share")%>%
        # select(sic_3, p_val, CI_mean_V, coeff_mean_V)
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
            names_from = c(var, intermediates),
            values_from = value
        ) %>%
        arrange(
            sic_3, desc(type)
        )# |> View()

    return(tmp_tbl)
}

## %% Get tables for bootstrap estimates ----------------

# Fix corps, all: boot_tax_ev_2ttst with tax_ev_test_trim (or tax_ev_test_2t)
render_tbl(
    boot_tax_ev_2ttst,
    tax_ev_test_2t
) |> View()

# Fix corps, others: boot_tax_ev_2t2s with tax_ev_test_2t_2smpl
pref_tax_ev_test_tbl <- render_tbl(
    boot_tax_ev_2t2s,
    tax_ev_test_2t_2smpl,
    cond = TRUE
) 
pref_tax_ev_test_tbl|> View()

# No fix, all: bayboot_tax_ev_2ttst with tax_ev_test_trim (or tax_ev_test_2t)
render_tbl(
    bayboot_tax_ev_2ttst,
    tax_ev_test_2t
) |> View()

# No fix, others: bayboot_tax_ev_2t2s with tax_ev_test_2t_2smpl
render_tbl(
    bayboot_tax_ev_2t2s,
    tax_ev_test_2t_2smpl,
    cond = TRUE
) |> View()

## %% Combining into one table ----------------

# leave_out  <- "mean_V_log_deductible_intermediates_share"
test_comp_tbl <- render_tbl(
        boot_tax_ev_2ttst,
        tax_ev_test_2t
    )[-3] %>%
    left_join(
        render_tbl(
            boot_tax_ev_2t2s,
            tax_ev_test_2t_2smpl,
            cond = TRUE
            )[-3], 
        by = c("sic_3", "type")) %>%
    left_join(
        render_tbl(
            bayboot_tax_ev_2ttst,
            tax_ev_test_2t
            )[-3], 
        by = c("sic_3", "type")) %>%
    left_join(
        render_tbl(
            bayboot_tax_ev_2t2s,
            tax_ev_test_2t_2smpl,
            cond = TRUE
        )[-3], by = c("sic_3", "type"))

test_comp_tbl |> View()

test_comp_ded_tbl <- render_tbl(
        boot_tax_ev_2ttst,
        tax_ev_test_2t
    )[-4] %>%
    left_join(
        render_tbl(
            boot_tax_ev_2t2s,
            tax_ev_test_2t_2smpl,
            cond = TRUE
            )[-4], 
        by = c("sic_3", "type")) %>%
    left_join(
        render_tbl(
            bayboot_tax_ev_2ttst,
            tax_ev_test_2t
            )[-4], 
        by = c("sic_3", "type")) %>%
    left_join(
        render_tbl(
            bayboot_tax_ev_2t2s,
            tax_ev_test_2t_2smpl,
            cond = TRUE
        )[-4], by = c("sic_3", "type"))
        
test_comp_ded_tbl |> View()

## %% Saving the combined table ----------------

save(
    test_comp_tbl, test_comp_ded_tbl, pref_tax_ev_test_tbl,
    file = "Code/Products/boot_test_comp_tbl.RData"
)
