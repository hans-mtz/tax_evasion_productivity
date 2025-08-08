# %% Load packages and data ---------------

library(tidyverse)
library(parallel)
# load("Code/Products/fs.RData")
# load("Code/Products/boot_fs.RData")
load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/global_vars.RData") # top_20_inds
load("Code/Products/intermediates.RData") #rv_cond, test_cont_tbl

## %% Set seed and data --------------------------

mc_cores <- detectCores() - 2
set.seed(77777)
R <- 250
## %% FUNs -----------------------


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


# %% Set up variables ------------------------

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
    sic_3 = top_20_inds$sic_3, #c(311,312,313,321,322,323,324,331,332),
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

# %% Run First Stage with Conditions ------------------------

fs_me_cust_list<-mcmapply(
    do_fs_cond, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    sic=ev_me_v$sic_3,
    var=ev_me_v$inter,
    r_var=ev_me_v$r_input,
    cond=ev_me_v$condition,
    MoreArgs = list(data=test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)
# fs_me_cust_list[1:5]

fs_me_cust_tbl <- get_table(fs_me_cust_list)

# good_guys <- c(
#     "all" = "All",
#     "corps" = "Corporations",
#     "exporters" = "Exporters",
#     "importers" = "Importers",
#     "importers_mats" = "Importers of Materials"
# )

fs_me_cust_tbl$good_guys <- names(ev_me_v$condition)
fs_me_cust_tbl<-fs_me_cust_tbl %>%
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

fs_me_cust_tbl
# %% bootstrap ------------------------

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

## %% Generate Table ------------------------

i_elas_tbl <- do.call(rbind, boot_fs_me_cust_list) %>%
    as.data.frame() %>%
    left_join(
        fs_me_cust_tbl,
        by = c("sic_3", "intermediate"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        across(
            !c(sic_3,intermediate),
            as.numeric
        )
    ) %>%
    mutate(
        bc_boot_m_all = `m - all` - `m - all.t0`,
        bc_boot_m_corps = `m - corps` - `m - corps.t0`,
        bc_boot_m_others = `m - others` - `m - others.t0`
    ) %>%
    group_by(
        sic_3, intermediate
    ) %>%
    reframe(
        val_m_all = quantile(bc_boot_m_all, c(0.975, 0.025)),
        val_m_corps = quantile(bc_boot_m_corps, c(0.975, 0.025)),
        val_m_others = quantile(bc_boot_m_others, c(0.975, 0.025)),
        probs = c(0.975, 0.025),
        CI = c("LCI", "UCI"),
        CI_all = max(`m - all.t0`) - val_m_all, # Bias Corrected Bootstrap CI
        CI_corps = max(`m - corps.t0`) - val_m_corps, # Bias Corrected Bootstrap CI
        CI_others = max(`m - others.t0`) - val_m_others, # Bias Corrected Bootstrap CI
        m_all = max(`m - all.t0`), # point estimate
        m_corps = max(`m - corps.t0`), # point estimate
        m_others = max(`m - others.t0`), # point estimate
    ) %>%
    mutate(
        across(
            where(is.numeric),
            ~ round(.x, 2)
        )
    ) %>%
    select(
        sic_3, intermediate,
        CI:m_others
    ) %>%
    pivot_wider(
        names_from = CI, 
        values_from = c(CI_all, CI_corps, CI_others),
        names_sep = "_"
    ) %>%
    mutate(
        CI_all = glue::glue("[{CI_all_LCI}, {CI_all_UCI}]"),
        CI_corps = glue::glue("[{CI_corps_LCI}, {CI_corps_UCI}]"),
        CI_others = glue::glue("[{CI_others_LCI}, {CI_others_UCI}]"),
        across(
            m_all:m_others,
            ~ glue::glue("{x}", x=.x)#,
            # .names = "coef_{col}"
        ),
    )%>%
    select(
        sic_3,
        m_all, CI_all,
        m_corps, CI_corps,
        m_others, CI_others
    ) %>%
    pivot_longer(
        cols = -sic_3,
        names_to = c("type", "group"),
        names_pattern = "(.*)_(.*)",
        values_to = "value"
    ) %>%
    pivot_wider(
        names_from = group,
        values_from = value
    ) %>%
    select(
        sic_3, type, corps, others
    )

i_elas_tbl

# %% Save results ---------------------

save(
    fs_me_cust_tbl, fs_me_cust_list,
    boot_fs_me_cust_list, i_elas_tbl,
    file = "Code/Products/i_elas.RData"
)

# %% Join with Test Table ------------------------

load("Code/Products/intermediates.RData")

ielas_cont_tbl <- i_elas_tbl %>%
    mutate(
        sic_3 = as.numeric(sic_3)
    ) %>%
    left_join(
        test_cont_tbl,
        by = "sic_3"
    ) %>%
    select(
        sic_3, #sic_3_lab,
        corps, others,
        log_mats_share, n:exporters
    )
ielas_cont_tbl

save(
    fs_me_cust_tbl, fs_me_cust_list,
    boot_fs_me_cust_list, i_elas_tbl,
    ielas_cont_tbl,
    file = "Code/Products/i_elas.RData"
)

## %% Join with Booststrap Test -------------------------
load("Code/Products/i_elas.RData")
load("Code/Products/boot_tax_ev_2ttst.RData")

elas_tst_tbl <- i_elas_tbl %>% 
    mutate(
        type = replace(
            type, type == "m", "coeff"
        ),
        sic_3 = as.numeric(sic_3)
    ) %>%
    left_join(
        boot_2ttst_tbl %>% 
                filter(
                    intermediates == "log_mats_share"
                ),
        by = c('sic_3', 'type')
    ) %>%
    select(
        -intermediates
    )

save(
    fs_me_cust_tbl, fs_me_cust_list,
    boot_fs_me_cust_list, i_elas_tbl,
    ielas_cont_tbl, elas_tst_tbl,
    file = "Code/Products/i_elas.RData"
)

# %% Industry Characteristics Table ------------------------

inds_char_tbl_corp <- colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    mutate(
        # total_gross_output = sum(gross_output, na.rm = TRUE),
        total_sales = sum(sales, na.rm = TRUE),
        total_deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE),
        total_materials = sum(materials, na.rm = TRUE),
        total_share_exports = mean(share_exports, na.rm = TRUE),
        total_share_imports = mean(share_imports, na.rm = TRUE),
        total_share_imports_materials = mean(share_imports_materials, na.rm = TRUE),
        total_share_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        importer = ifelse(share_imports > 0, 1, 0),
        exporter = ifelse(share_exports > 0, 1, 0),
        importer_mats = ifelse(share_imports_materials > 0, 1, 0),
        Corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        
    ) %>%
    mutate(
        avg_age_plant = mean(age, na.rm = TRUE),
        .by = c(plant)
    ) %>%
    group_by(sic_3, Corp) %>%
    summarise(
        n = unique(plant) |> length(),
        avg_age = mean(age, na.rm = TRUE),
        # n_Corp = unique(plant*as.numeric(juridical_organization==3)) |> length()-1,
        sales_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        purchases_sales_tax = mean(sales_tax_purchases, na.rm = TRUE),
        # gross_output = sum(gross_output, na.rm = TRUE)/max(total_gross_output, na.rm = TRUE),
        sales = sum(sales, na.rm = TRUE)/max(total_sales, na.rm = TRUE),
        # deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE)/max(total_deductible_intermediates, na.rm = TRUE),
        # materials = sum(materials, na.rm = TRUE)/max(total_materials, na.rm = TRUE),
        share_exports = mean(share_exports, na.rm = TRUE),
        # share_imports = mean(share_imports, na.rm = TRUE),
        share_imports_materials = mean(share_imports_materials, na.rm = TRUE),
        exporters = mean(exporter, na.rm = TRUE),
        # importers = mean(importer, na.rm = TRUE),
        importers_mats = mean(importer_mats, na.rm = TRUE)
    ) %>%
    mutate(
        across(sales_sales_tax:importers_mats, ~ round(.x*100, 1)),
        avg_age = round(avg_age, 0),
    ) %>%
    pivot_wider(
        names_from = Corp,
        values_from = n:importers_mats,
        names_sep = "_"
    )# |> View()

inds_char_tbl_corp

inds_char_tbl <- colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    mutate(
        # total_gross_output = sum(gross_output, na.rm = TRUE),
        total_sales = sum(sales, na.rm = TRUE),
        total_deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE),
        total_materials = sum(materials, na.rm = TRUE),
        total_share_exports = mean(share_exports, na.rm = TRUE),
        total_share_imports = mean(share_imports, na.rm = TRUE),
        total_share_imports_materials = mean(share_imports_materials, na.rm = TRUE),
        total_share_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        importer = ifelse(share_imports > 0, 1, 0),
        exporter = ifelse(share_exports > 0, 1, 0),
        importer_mats = ifelse(share_imports_materials > 0, 1, 0),
        Corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        
    ) %>%
    mutate(
        avg_age_plant = mean(age, na.rm = TRUE),
        .by = c(plant)
    ) %>%
    group_by(sic_3) %>%
    summarise(
        n = unique(plant) |> length(),
        n_Corp = unique(plant*as.numeric(juridical_organization==3)) |> length()-1,
        avg_age = mean(age, na.rm = TRUE),
        sales_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        purchases_sales_tax = mean(sales_tax_purchases, na.rm = TRUE),
        # gross_output = sum(gross_output, na.rm = TRUE)/max(total_gross_output, na.rm = TRUE),
        sales = sum(sales, na.rm = TRUE)/max(total_sales, na.rm = TRUE),
        # deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE)/max(total_deductible_intermediates, na.rm = TRUE),
        # materials = sum(materials, na.rm = TRUE)/max(total_materials, na.rm = TRUE),
        exporters = mean(exporter, na.rm = TRUE),
        share_exports = mean(share_exports, na.rm = TRUE),
        # share_imports = mean(share_imports, na.rm = TRUE),
        importers_mats = mean(importer_mats, na.rm = TRUE),
        share_imports_materials = mean(share_imports_materials, na.rm = TRUE)
        # importers = mean(importer, na.rm = TRUE),
    ) %>%
    mutate(
        across(sales_sales_tax:share_imports_materials, ~ round(.x*100, 1)),
        avg_age = round(avg_age, 0),
    ) #%>%
    # pivot_wider(
    #     names_from = Corp,
    #     values_from = n:importers_mats,
    #     names_sep = "_"
    # )# |> View()
inds_char_tbl

## %% Combining tbls ------------------------

load("Code/Products/i_elas.RData")
load("Code/Products/boot_tax_ev_2ttst.RData")

i_elas_tbl
tst_2t2s_tbl


comparison_tbl <- i_elas_tbl %>%
    mutate(
        type = replace(
            type, type == "m", "coeff"
        ),
        sic_3 = as.numeric(sic_3)
    ) %>%
    left_join(
        tst_2t2s_tbl %>% select(-mean_V_log_deductible_intermediates_share),
        by = c('sic_3', 'type')
    ) %>%
    left_join(
        inds_char_tbl_corp,
        by = "sic_3",
    ) #|> View()


main_tbl <- i_elas_tbl %>%
    mutate(
        type = replace(
            type, type == "m", "coeff"
        ),
        sic_3 = as.numeric(sic_3)
    ) %>%
    left_join(
        tst_2t2s_tbl %>% select(-mean_V_log_deductible_intermediates_share),
        by = c('sic_3', 'type')
    ) %>%
    left_join(
        inds_char_tbl,
        by = "sic_3",
    ) %>% 
    mutate(
        effective_tax_rate = ifelse(
            type == "coeff",
            round((sales_sales_tax-purchases_sales_tax*as.numeric(corps)),1),
            NaN
        ),
        .before = sales_sales_tax
    )#|> View()

## %% Saving the main table ------------------------

save(
    fs_me_cust_tbl, fs_me_cust_list,
    boot_fs_me_cust_list, i_elas_tbl,
    ielas_cont_tbl, elas_tst_tbl,
    inds_char_tbl, inds_char_tbl_corp,
    comparison_tbl, main_tbl,
    file = "Code/Products/i_elas.RData"
)

## %% stuff -------------------------

vrs<-gsub("^(\\w_*\\w*)_(Corp|Non-Corp)$","\\1",names(inds_char_tbl_corp)[-1]) |> unique()
vrs
col_vec <- rep(2, length(vrs))
names(col_vec) <- vrs
col_vec |> View()
gsub("(_)", " ", names(col_vec))
gsub("\\b(\\w)", "\\U\\1", names(col_vec), perl=TRUE) 
