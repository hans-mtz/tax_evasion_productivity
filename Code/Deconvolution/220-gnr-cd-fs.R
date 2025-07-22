## %% Load Data and Libraries ------------------------

library(tidyverse)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/intermediates.RData")
load("Code/Products/boot_tax_ev_mmt.RData")

mc_cores <- detectCores() - 2
set.seed(77777)
R <- 250
### %% Stata Data ------------------------
data_folder <- "/Volumes/SSD Hans 1/Dropbox/Dropbox hmarti33/COLOMBIA/"
gnr_stata_data <- haven::read_dta(paste0(data_folder, "gnr-colombia-stata-data.dta")) 

names(gnr_stata_data)

gnr_stata_data <- gnr_stata_data %>%
    rename(
        l_man_hour = l,
        y = l_rgo,
        k = l_rk,
        l = l_l,
        m = l_rii,
        juridical_organization = j_org
    ) 

gnr_stata_data <- gnr_stata_data %>%
    mutate(
        log_ded_i_share = log(ded_i_share),
        log_si = log(si)
    )

## %% data moments ------------------------

# colombia_data_frame %>%
#     # group_by(sic_3, juridical_organization==3) %>%
#     filter(
#         is.finite(y),
#         is.finite(k),
#         is.finite(l),
#         is.finite(m),
#         sic_3 == 311,
#         is.finite(log_mats_share)
#         # juridical_organization == 3,
#         log_mats_share > log(0.05)
#     ) %>%
#     lm(
#         log_mats_share ~ 1,
#         data = .    
#     ) -> fs_reg

# log_D <- coefficients(fs_reg)[["(Intercept)"]]
# epsilon <- residuals(fs_reg)
# big_E <- -epsilon |> exp() |> mean()
# beta <- exp(log_D - log(big_E))
# mean_epsilon <- mean(-epsilon)
# sd_epsilon <- sd(-epsilon)

colombia_data_frame %>%
    # group_by(sic_3, juridical_organization==3) %>%
    mutate(
        mats_share = nom_mats / nom_gross_output,
        ded_i_share = nom_deductible_intermediates / nom_gross_output,
        si = nom_intermediates / nom_gross_output
    ) %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        sic_3 == 311,
        # is.finite(materials_share),
        # is.finite(log_mats_share),
        # juridical_organization == 3,
        # log_share > log(0.05)
        ded_i_share > 0.05
        # mats_share > 0.05
    ) %>%
    summarise(
        across(
            c(materials, 
            deductible_intermediates, 
            intermediates,
            materials_share, mats_share,
            deductible_intermediates_share,
            ded_i_share, gnr_int_share, si),
            ~ mean(.x, na.rm = TRUE)
        ),
        n = n()
    ) |> as.data.frame()


gnr_stata_data %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        sic_3 == 311,
        # is.finite(materials_share),
        # is.finite(log_mats_share),
        # juridical_organization == 3,
        # log_share > log(0.05)
        ded_i_share > 0.05
        # mats_share > 0.05
    ) %>%
    summarise(
        across(
            c(
            rmats, 
            rded_i, 
            rii,
            mats_share,
            ded_i_share, 
            si),
            ~ mean(.x, na.rm = TRUE)
        ),
        n = n()
    ) |> as.data.frame()
inter <- "log_ded_i_share"
fml <- paste0(inter,"~1") |> as.formula()
gnr_stata_data %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        sic_3 == 311,
        # is.finite(materials_share),
        # is.finite(log_mats_share),
        # juridical_organization == 3,
        # log_share > log(0.05)
        # ded_i_share > 0.05
        .data[[inter]] > log(0.05)
    ) %>%
    lm(fml, data = .) -> fs_reg_311_deds

log_D <- coefficients(fs_reg_311_deds)[[1]]
epsilon <- residuals(fs_reg_311_deds)
big_E <- -epsilon |> exp() |> mean()
beta <- exp(log_D - log(big_E))
mean_epsilon <- mean(-epsilon)
variance_epsilon <- var(-epsilon)
beta
## %% Compare Trimming All Inds ------------------------

### %% Stata Results ------------------------

read.csv(
    "Code/Products/gnr_fs_trim.csv",
    skip = 1
) -> gnr_fs_trim

gnr_fs_trim <- gnr_fs_trim %>% 
    mutate(
        sic_3 = str_extract(X, "\\d{3}"),
        inter = str_extract(X, "\\w+$"),
        .before = "m"
    )


gnr_fs_trim


### %% R results ------------------------

first_stage_panel_all <- function(sic_in, share, r_var, df) {
    fml <- paste0(share,"~1") |> as.formula()

    regdata <- df %>%
        filter(
            sic_3 == sic_in,
            # juridical_organization == 3,
            # # is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[share]] > log(0.05)
        )

    cat("ragdata dims:", dim(regdata), "\n")
    fs_reg <-lm(fml, regdata)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- -epsilon |> exp() |> mean()
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- df %>%
        filter(
            sic_3 == sic_in,
            # is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[share]] > log(0.05)
        ) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[share]] - log_D,
            log_inter  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(log_inter-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, m, k, l, y, log_inter
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E,
        sic_3 = sic_in,
        inter = r_var
    )
    return(result_list)
}

first_stage_panel_all(311, "log_ded_i_share", "rded_i", gnr_stata_data)
### %% Run First Stage R Code and Stata Data ------------------------

run_vars_gnr <- expand.grid(
    sic_3 = c(311, 321, 322, 331, 381),
    inter = c("log_si","lmats_share","log_ded_i_share"),
    stringsAsFactors = FALSE
)

inter_named <- c(
    "log_si" = "rii",
    "lmats_share" = "rmats",
    "log_ded_i_share" = "rded_i"
)

run_vars_gnr$r_input <- inter_named[run_vars_gnr$inter]
run_vars_gnr

fs_list_gnr_all<-mapply(
    first_stage_panel_all, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    run_vars_gnr[,"sic_3"],
    run_vars_gnr[,"inter"],
    run_vars_gnr[,"r_input"],
    MoreArgs = list(df=gnr_stata_data),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)

names(fs_list_gnr_all)<-paste(run_vars_gnr[,"sic_3"],run_vars_gnr[,"inter"])
fs_list_gnr_all 

### %% Generate Table ------------------------

get_table <- function(l){
    tbl <- sapply(
        seq_along(l),
        \(x){
            c(
                sic_3 = l[[x]]$sic_3,
                intermediate = l[[x]]$inter,
                m = l[[x]]$beta |> round(2),
                `$\\mathcal{E}$` = l[[x]]$big_E |> round(2),
                `err sd` = l[[x]]$epsilon_sigma |> round(2)
            )
        }
    ) |> t() |> as.data.frame()
    return(tbl)
}

get_table(fs_list_gnr_all)
sapply(
    seq_along(fs_list_gnr_all),
    \(x){
        c(
            sic_3 = fs_list_gnr_all[[x]]$sic_3,
            intermediate = fs_list_gnr_all[[x]]$inter,
            m = fs_list_gnr_all[[x]]$beta |> round(2),
            `$\\mathcal{E}$` = fs_list_gnr_all[[x]]$big_E |> round(2),
            `err sd` = fs_list_gnr_all[[x]]$epsilon_sigma |> round(2)
        )
    }
) |> t() |> as.data.frame() -> gnr_fs_tbl_all

gnr_fs_tbl_all %>% arrange(sic_3)

### %% R Data & R Code - CD GNR First Stage ------------------------

run_vars_gnr <- expand.grid(
    sic_3 = c(311, 321, 322, 331, 381),
    inter = c("log_share","log_mats_share","log_deductible_intermediates_share"),
    stringsAsFactors = FALSE
)

inter_named <- c(
    "log_mats_share" = "materials",
    "log_deductible_intermediates_share" = "deductible_intermediates",
    "log_share" = "intermediates" 
)

run_vars_gnr$r_input <- inter_named[run_vars_gnr$inter]
run_vars_gnr

first_stage_panel_trim <- function(sic, var, r_var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    fs_reg <- data %>%
        select(
            !c(log_mats_share, log_deductible_intermediates_share,log_share)
        ) %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
            log_mats_share = log(nom_mats / nom_gross_output),
            log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
            log_share = log(nom_intermediates / nom_gross_output)
        ) %>%
        filter(
            sic_3 == sic,
            # juridical_organization == 3,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        lm(fml, data = .) # %>%
        # fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- -epsilon |> exp() |> mean()
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- data %>%
        select(
            !c(log_mats_share, log_deductible_intermediates_share,log_share)
        ) %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
            log_mats_share = log(nom_mats / nom_gross_output),
            log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
            log_share = log(nom_intermediates / nom_gross_output)
        ) %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[var]] - log_D,
            m  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(m-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, m, k, l, y
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E,
        sic_3 = sic,
        inter = r_var
    )
    return(result_list)
}
## %% Run GNR First Stage ------------------------

fs_list_gnr<-mapply(
    first_stage_panel_trim, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    run_vars_gnr[,"sic_3"],
    run_vars_gnr[,"inter"],
    run_vars_gnr[,"r_input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)

names(fs_list_gnr)<-paste(run_vars_gnr[,"sic_3"],run_vars_gnr[,"inter"])

## %% Generate Table ------------------------

sapply(
    seq_along(fs_list_gnr),
    \(x){
        c(
            sic_3 = fs_list_gnr[[x]]$sic_3,
            intermediate = fs_list_gnr[[x]]$inter,
            m = fs_list_gnr[[x]]$beta |> round(2),
            `$\\mathcal{E}$` = fs_list_gnr[[x]]$big_E |> round(2),
            `err sd` = fs_list_gnr[[x]]$epsilon_sigma |> round(2)
        )
    }
) |> t() |> as.data.frame() -> gnr_fs_tbl

gnr_fs_tbl %>% arrange(sic_3)

### %% Merge and create table ------------------------

aux <- c(
    "gnr_ii"="intermediates",
    "materials"="materials",
    "dedudctibles" = "deductible_intermediates"
)
gnr_fs_trim$intermediate <- aux[gnr_fs_trim$inter]

gnr_fs_trim %>%
    select(
        sic_3, intermediate, m, bigE, err_sd
    ) %>%
    left_join(
        gnr_fs_tbl,
        by = c("sic_3", "intermediate"),
        suffix = c(".stata", ".R")
    ) %>%
    mutate(
        intermediate = factor(intermediate, 
            levels = c("intermediates", "materials", "deductible_intermediates"),
            labels = c("Intermediates", "Materials", "Deductibles")
        )
    )-> gnr_fs_trim_tbl

gnr_fs_trim_tbl

## %% Save Results ------------------------

save(
    fs_list_gnr,
    gnr_fs_tbl,
    gnr_fs_trim_tbl,
    gnr_fs_trim,
    gnr_fs_tbl_all,
    file = "Code/Products/gnr_fs.RData"
)

## %% Compare Trimming Corporations ------------------------

### Stata Results ------------------------

read.csv(
    "Code/Products/gnr_fs_trim_corps.csv",
    skip = 1
) -> gnr_fs_trim_corps

gnr_fs_trim_corps <- gnr_fs_trim_corps %>% 
    mutate(
        sic_3 = str_extract(X, "\\d{3}"),
        inter = str_extract(X, "\\w+$"),
        .before = "m"
    )
gnr_fs_trim_corps

### R Results ------------------------

first_stage_panel <- function(sic, var, r_var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    fs_reg <- data %>%
        select(
            !c(log_mats_share, log_deductible_intermediates_share,log_share)
        ) %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
            log_mats_share = log(nom_mats / nom_gross_output),
            log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
            log_share = log(nom_intermediates / nom_gross_output)
        ) %>%
        filter(
            sic_3 == sic,
            juridical_organization == 3,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        lm(fml, data = .) # %>%
        # fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- -epsilon |> exp() |> mean()
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- data %>%
        select(
            !c(log_mats_share, log_deductible_intermediates_share,log_share)
        ) %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
            log_mats_share = log(nom_mats / nom_gross_output),
            log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
            log_share = log(nom_intermediates / nom_gross_output)
        ) %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[var]] - log_D,
            m  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(m-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, m, k, l, y
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E,
        sic_3 = sic,
        inter = r_var
    )
    return(result_list)
}

## %% Run Tax-GNR First Stage ------------------------

fs_list<-mapply(
    first_stage_panel, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    run_vars_gnr[,"sic_3"],
    run_vars_gnr[,"inter"],
    run_vars_gnr[,"r_input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)

names(fs_list)<-paste(run_vars_gnr[,"sic_3"],run_vars_gnr[,"inter"])

## %% Generate Table ------------------------

sapply(
    seq_along(fs_list),
    \(x){
        c(
            sic_3 = fs_list[[x]]$sic_3,
            intermediate = fs_list[[x]]$inter,
            m = fs_list[[x]]$beta |> round(2),
            `$\\mathcal{E}$` = fs_list[[x]]$big_E |> round(2),
            `err sd` = fs_list[[x]]$epsilon_sigma |> round(2)
        )
    }
) |> t() |> as.data.frame() -> fs_tbl

fs_tbl %>% arrange(sic_3)

### %% Merge and create table ------------------------

gnr_fs_trim_corps$intermediate <- aux[gnr_fs_trim_corps$inter]

gnr_fs_trim_corps %>%
    select(
        sic_3, intermediate, m, bigE, err_sd
    ) %>%
    left_join(
        fs_tbl,
        by = c("sic_3", "intermediate"),
        suffix = c(".stata", ".R")
    ) %>%
    mutate(
        intermediate = factor(intermediate, 
            levels = c("intermediates", "materials", "deductible_intermediates"),
            labels = c("Intermediates", "Materials", "Deductibles")
        )
    )-> fs_compare_tbl

fs_compare_tbl

## %% Save Results ------------------------

save(
    fs_list_gnr,
    gnr_fs_tbl,
    gnr_fs_trim_tbl,
    gnr_fs_trim,
    gnr_fs_tbl_all,
    fs_list,
    fs_tbl,
    fs_compare_tbl,
    gnr_fs_trim_corps,
    file = "Code/Products/gnr_fs.RData"
)

## %% Measurement Error ------------------------

first_stage_panel_me <- function(sic, var, r_var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    fs_reg <- data %>%
        select(
            !c(log_mats_share, log_deductible_intermediates_share,log_share)
        ) %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
            log_mats_share = log(nom_mats / nom_gross_output),
            log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
            log_share = log(nom_intermediates / nom_gross_output)
        ) %>%
        filter(
            sic_3 == sic,
            juridical_organization == 3,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        lm(fml, data = .) # %>%
        # fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- 1
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- data %>%
        select(
            !c(log_mats_share, log_deductible_intermediates_share,log_share)
        ) %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
            log_mats_share = log(nom_mats / nom_gross_output),
            log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
            log_share = log(nom_intermediates / nom_gross_output)
        ) %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[var]] - log_D,
            m  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(m-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, m, k, l, y
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E,
        sic_3 = sic,
        inter = r_var
    )
    return(result_list)
}

## %% Run Tax-GNR First Stage ME ------------------------

fs_list_me<-mapply(
    first_stage_panel_me, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    run_vars_gnr[,"sic_3"],
    run_vars_gnr[,"inter"],
    run_vars_gnr[,"r_input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)


## %% Generate Table ------------------------

sapply(
    seq_along(fs_list_me),
    \(x){
        c(
            sic_3 = fs_list_me[[x]]$sic_3,
            intermediate = fs_list_me[[x]]$inter,
            m = fs_list_me[[x]]$beta |> round(2),
            `$\\mathcal{E}$` = fs_list_me[[x]]$big_E |> round(2),
            `err sd` = fs_list_me[[x]]$epsilon_sigma |> round(2)
        )
    }
) |> t() |> as.data.frame() -> fs_tbl_me

fs_tbl_me %>% arrange(sic_3)


### %% ME - all industries ------------------------


first_stage_panel_me_all <- function(sic, var, r_var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    fs_reg <- data %>%
        select(
            !c(log_mats_share, log_deductible_intermediates_share,log_share)
        ) %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
            log_mats_share = log(nom_mats / nom_gross_output),
            log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
            log_share = log(nom_intermediates / nom_gross_output)
        ) %>%
        filter(
            sic_3 == sic,
            # juridical_organization == 3,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        lm(fml, data = .) # %>%
        # fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- 1
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- data %>%
        select(
            !c(log_mats_share, log_deductible_intermediates_share,log_share)
        ) %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
            log_mats_share = log(nom_mats / nom_gross_output),
            log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
            log_share = log(nom_intermediates / nom_gross_output)
        ) %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[var]] - log_D,
            m  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(m-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, m, k, l, y
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E,
        sic_3 = sic,
        inter = r_var
    )
    return(result_list)
}

## %% Run Tax-GNR First Stage ME ------------------------

fs_list_me_all<-mapply(
    first_stage_panel_me_all, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    run_vars_gnr[,"sic_3"],
    run_vars_gnr[,"inter"],
    run_vars_gnr[,"r_input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)


## %% Generate Table ------------------------

sapply(
    seq_along(fs_list_me_all),
    \(x){
        c(
            sic_3 = fs_list_me_all[[x]]$sic_3,
            intermediate = fs_list_me_all[[x]]$inter,
            m = fs_list_me_all[[x]]$beta |> round(2),
            `$\\mathcal{E}$` = fs_list_me_all[[x]]$big_E |> round(2),
            `err sd` = fs_list_me_all[[x]]$epsilon_sigma |> round(2)
        )
    }
) |> t() |> as.data.frame() -> fs_tbl_me_all

fs_tbl_me_all %>% arrange(sic_3)

### %% ME - all industries - compare ------------------------

fs_tbl_me %>% arrange(sic_3) %>%
    left_join(
        fs_tbl_me_all %>% arrange(sic_3),
        by = c("sic_3", "intermediate"),
        suffix = c(".co", ".all")
    ) %>%
    mutate(
        intermediate = factor(intermediate, 
            levels = c("intermediates", "materials", "deductible_intermediates"),
            labels = c("Intermediates", "Materials", "Deductibles")
        )
    )-> fs_compare_me_tbl

fs_compare_me_tbl

## %% Save Results ------------------------

save(
    fs_list_gnr,
    gnr_fs_tbl,
    gnr_fs_trim_tbl,
    gnr_fs_trim,
    gnr_fs_tbl_all,
    fs_list,
    fs_tbl,
    fs_compare_tbl,
    gnr_fs_trim_corps,
    fs_list_me,
    fs_tbl_me,
    fs_list_me_all,
    fs_tbl_me_all,
    fs_compare_me_tbl,
    file = "Code/Products/gnr_fs.RData" 
)

## %% Top 5 Evading Industries ------------------------

ev_vars <- expand.grid(
    sic_3 = top_5_ev_inds_mag[1:5],
    inter = c("log_share","log_mats_share","log_deductible_intermediates_share"),
    stringsAsFactors = FALSE
)

inter_named <- c(
    "log_mats_share" = "materials",
    "log_deductible_intermediates_share" = "deductible_intermediates",
    "log_share" = "intermediates" 
)

ev_vars$r_input <- inter_named[ev_vars$inter]
ev_vars

fs_ev_me_co_l<-mapply(
    first_stage_panel_me, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    ev_vars[,"sic_3"],
    ev_vars[,"inter"],
    ev_vars[,"r_input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)

fs_ev_me_all_l<-mapply(
    first_stage_panel_me_all, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    ev_vars[,"sic_3"],
    ev_vars[,"inter"],
    ev_vars[,"r_input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)

## %% Generate Table ------------------------

sapply(
    seq_along(fs_ev_me_co_l),
    \(x){
        c(
            sic_3 = fs_ev_me_co_l[[x]]$sic_3,
            intermediate = fs_ev_me_co_l[[x]]$inter,
            m = fs_ev_me_co_l[[x]]$beta |> round(2),
            `$\\mathcal{E}$` = fs_ev_me_co_l[[x]]$big_E |> round(2),
            `err sd` = fs_ev_me_co_l[[x]]$epsilon_sigma |> round(2)
        )
    }
) |> t() |> as.data.frame() -> fs_tbl_ev_me_co

sapply(
    seq_along(fs_ev_me_all_l),
    \(x){
        c(
            sic_3 = fs_ev_me_all_l[[x]]$sic_3,
            intermediate = fs_ev_me_all_l[[x]]$inter,
            m = fs_ev_me_all_l[[x]]$beta |> round(2),
            `$\\mathcal{E}$` = fs_ev_me_all_l[[x]]$big_E |> round(2),
            `err sd` = fs_ev_me_all_l[[x]]$epsilon_sigma |> round(2)
        )
    }
) |> t() |> as.data.frame() -> fs_tbl_ev_me_all

fs_tbl_ev_me_co %>%
    left_join(
        fs_tbl_ev_me_all %>% arrange(sic_3),
        by = c("sic_3", "intermediate"),
        suffix = c(".co", ".all")
    ) %>%
    mutate(
        intermediate = factor(intermediate, 
            levels = c("intermediates", "materials", "deductible_intermediates"),
            labels = c("Intermediates", "Materials", "Deductibles")
        )
    )-> fs_compare_ev_me_tbl

fs_compare_ev_me_tbl %>% 
    mutate(
        sic_3 = factor(sic_3, 
            levels = top_5_ev_inds_mag[1:5],
            # labels = c("311", "321", "322", "331", "381")
        )
    ) %>% arrange(sic_3) -> fs_compare_ev_me_tbl
fs_compare_ev_me_tbl

## %% Save Results ------------------------

# load("Code/Products/gnr_fs.RData")

save(
    fs_list_gnr,
    gnr_fs_tbl,
    gnr_fs_trim_tbl,
    gnr_fs_trim,
    gnr_fs_tbl_all,
    fs_list,
    fs_tbl,
    fs_compare_tbl,
    gnr_fs_trim_corps,
    fs_list_me,
    fs_tbl_me,
    fs_list_me_all,
    fs_tbl_me_all,
    fs_compare_me_tbl,
    fs_compare_ev_me_tbl,
    file = "Code/Products/gnr_fs.RData" 
)

## %% conditions ------------------------



do_fs_cond <- function(sic, var, r_var, data, cond) {
    fml <- paste0(var,"~1") |> as.formula()
    fs_reg <- data %>%
        # select(
        #     !c(log_mats_share, log_deductible_intermediates_share,log_share)
        # ) %>%
        # mutate(
        #     treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
        #     log_mats_share = log(nom_mats / nom_gross_output),
        #     log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
        #     log_share = log(nom_intermediates / nom_gross_output)
        # ) %>%
        filter(
            sic_3 == sic,
            # juridical_organization == 3,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut),
            eval(cond, envir = .)
        ) %>%
        lm(fml, data = .) # %>%
        # fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- 1
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- data %>%
        # select(
        #     !c(log_mats_share, log_deductible_intermediates_share,log_share)
        # ) %>%
        # mutate(
        #     treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp"),
        #     log_mats_share = log(nom_mats / nom_gross_output),
        #     log_deductible_intermediates_share = log(nom_deductible_intermediates / nom_gross_output),
        #     log_share = log(nom_intermediates / nom_gross_output)
        # ) %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[var]] - log_D,
            m  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(m-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, m, k, l, y
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E,
        sic_3 = sic,
        inter = r_var
    )
    return(result_list)
}

conditions <- list(
    all=quote(TRUE),
    corps=quote(juridical_organization == 3),
    others=quote(juridical_organization != 3)
    # exporters=quote(share_exports > 0.1),
    # importers=quote(share_imports > 0.1),
    # importers_mats=quote(share_imports_materials > 0.1)
)

ev_me_v <- expand.grid(
    # sic_3 = union(top_5_ev_inds_mag[1:5], c(311, 321, 322, 331, 381)),
    sic_3 = c(311,312,313,321,322,323,324,331,332),
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

ev_me_v$r_input <- inter_named[ev_me_v$inter]
ev_me_v

do_fs_cond(
    sic = 311,
    var = "log_mats_share",
    r_var = "materials",
    data = test_data,
    cond = conditions$corps
)

## %% Run First Stage with Conditions ------------------------

fe_me_list<-mcmapply(
    do_fs_cond, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    sic=ev_me_v$sic_3,
    var=ev_me_v$inter,
    r_var=ev_me_v$r_input,
    cond=ev_me_v$condition,
    MoreArgs = list(data=test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)
# fe_me_list[1:5]

fs_me_tbl <- get_table(fe_me_list)

# good_guys <- c(
#     "all" = "All",
#     "corps" = "Corporations",
#     "exporters" = "Exporters",
#     "importers" = "Importers",
#     "importers_mats" = "Importers of Materials"
# )

fs_me_tbl$good_guys <- names(ev_me_v$condition)
fs_me_tbl<-fs_me_tbl %>%
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

## %% Generate Table ------------------------

# load("Code/Products/gnr_fs.RData")

## %% bootstrap ------------------------

# set.seed(1234)

boot_fs_me_cust_list<-mclapply(
    1:R,
    function(i){

        temp_data <- resample_by_group(test_data, sic_3)

        temp_list<-mapply(
            do_fs_cond, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
            sic=ev_me_v$sic_3,
            var=ev_me_v$inter,
            r_var=ev_me_v$r_input,
            cond=ev_me_v$condition,
            MoreArgs = list(data=temp_data),
            SIMPLIFY = FALSE#,
            # mc.cores = mc_cores
        )

        temp_tbl <- get_table(temp_list)


        temp_tbl$good_guys <- names(ev_me_v$condition)
        temp_tbl<-temp_tbl %>%
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
        return(temp_tbl)
    },
    mc.cores = mc_cores
)



## %% Save Results ------------------------

save(
    fs_list_gnr,
    gnr_fs_tbl,
    gnr_fs_trim_tbl,
    gnr_fs_trim,
    gnr_fs_tbl_all,
    fs_list,
    fs_tbl,
    fs_compare_tbl,
    gnr_fs_trim_corps,
    fs_list_me,
    fs_tbl_me,
    fs_list_me_all,
    fs_tbl_me_all,
    fs_compare_me_tbl,
    fs_compare_ev_me_tbl,
    fe_me_list, fs_me_tbl,
    file = "Code/Products/gnr_fs.RData" 
)

