# %% Load data and packages ---------------
library(tidyverse)
library(parallel)
library(fixest)
library(modelsummary)
library(tinytable)
load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")

## Function summing rows across columns -----------------
## ignoring NAs unless all values are NAs

# sum_rows <- function(data) {
#   apply(data, 1, function(row) {
#     if (all(is.na(row))) {
#       return(NA)
#     } else {
#       return(sum(row, na.rm = TRUE))
#     }
#   })
# }


# Ejemplo de uso
# df <- data.frame(a = c(1, 2, NA), b = c(NA, 3, NA), c = c(4, NA, NA))
# sum_rows(df)

# df %>%
#     mutate(
#         sum_abc = sum_rows(cbind(a,-b,c))
#     )

## %% Definitions of Intermediates -------------
### GNR: materials, electricity, and services (log_share)
###    services: Gen Exp-Machinery rental-Interest payments)
###    Deductible: materials + deductible services (log_gnr_ded)
###    Non-Deductible: electricity + non deductivle services (log_gnr_nded)
### Levinsohn & Petrin (LP): materials, electricity, and fuels 
###    proxies are materials and electricity, non-zero obs
###    Deductible: materials + fuels
###    Non Deductible: electricity
### Alternative: materials, electricity, and industrial services
###    Deductible: materials + deductible industrial services
###    Non Deductible: electricity + non deductible industrial services

# intermediates<-c(
#     "log_share","log_gnr_ded","log_gnr_nded",
#     "log_lp","log_lp_ded","log_lp_nded",
#     "log_inds_int", "log_inds_ded","log_inds_nded"
# )

intermediates<-c(
    "log_mats_share", "log_energy_share","log_fuels_share",
    "log_repair_maint_share","log_deductible_intermediates_share",
    "log_non_deductible_intermediates_share"
)

# intermediates_gnr<-c(
#     "log_share","log_mats_share", "log_energy_share", "log_services_share"
# )

intermediates %in% names(test_data) |> all()

# intermediates_gnr %in% names(colombia_data_frame) |> all()

# test_data <- colombia_data_frame %>%
#     ungroup() %>%
#     filter(
#         is.finite(y),
#         is.finite(k),
#         is.finite(l),
#         is.finite(m)
#     ) %>%
#     select(
#         !intermediates
#     ) %>%
#     mutate(
#         log_mats_share = log(nom_mats/nom_gross_output),
#         log_energy_share = log(nom_energy/nom_gross_output),
#         log_fuels_share = log(nom_fuels/nom_gross_output),
#         log_repair_maint_share = log(nom_repair_maint/nom_gross_output),
#         log_deductible_intermediates_share = log(nom_deductible_intermediates/nom_gross_output),
#         log_non_deductible_intermediates_share = log(nom_non_deductible_intermediates/nom_gross_output)
#     ) %>%
#     select(
#         sic_3, plant, year, juridical_organization, 
#         y, k, l, m, log_mats_share:log_non_deductible_intermediates_share,
#         materials, energy, fuels, repair_maintenance, deductible_intermediates,
#         share_imports, share_exports, share_imports_materials,
#         share_sales_tax, sales_taxes, sales
#     )

# save(
#     test_data,
#     file = "Code/Products/test_data.RData"      
# )
## Labels ----------------------------

# labels <- c(
#     # 'capital' = 'Capital',
#     # 'mats_serv' = 'Materials & Services',
#     # 'skilled_labor' = 'Labor (Skilled)',
#     # 'unskilled_labor' = 'Labor (Unskilled)',
#     # 'sales' = 'Revenue',
#     # 'sales_taxes' = 'Sales Taxes',
#     # 'JO_class' = 'J. Org.',
#     'skilled_wage_bill_share' = 'Skilled Labor (Wages)',
#     'unskilled_wage_bill_share' = 'Unskilled Labor (Wages)',
#     'share_sales_tax' = 'Sales Taxes',
#     'intermediates_share' = 'GNR (M.+G.S.)',
#     # 'mats_serv_share' = 'Materials + Services',
#     'mats_deduct_share' = 'Materials + Deductible T.S.',
#     'materials_share' = 'Materials (M)',
#     'energy_share' = 'Electricity (E)',
#     'capital_share' = 'Capital',
#     'gnr_ded_share' = 'Deductible GNR',
#     'lp_share' = 'LP (M+E+F)',
#     'lp_ded_share' = 'Deductible LP',
#     'inds_int_share' = 'EAM (M+E+I.S.)',
#     'inds_ded_share' = 'Deductible EAM',
#     'total_expenses_share' = 'Total Services (G.S.+I.S.)',
#     'services_exp_share' = 'General Services (G.S.)',
#     'industrial_exp_share' = 'Industrial Services (I.S.)',
#     'deductible_exp_share' = 'Deductible T.S.',
#     'energy_nondeductibles_share' = 'Electricity + \nNon-Deductibles T.S.',
#     'fuels_share' = 'Fuels (F)',
#     'deductible_services_share' = 'Deductible G.S.',
#     'industrial_ded_exp_share' = 'Deductible I.S.'
# )


# colombia_data_frame %>%
#     # mutate(
#     #     inds_exp_deductible_share = deductible_inds_exp/total_expenditure
#     # ) %>%
#     select(
#         starts_with("log_") #& !contains("bill")
#     ) %>%
#     datasummary_skim(
#     fun_numeric = list(
#         Missing = PercentMissing, 
#         Mean = Mean, 
#         SD = SD, 
#         Q1 = P25,
#         Median = Median,
#         Q3 = P75
#     ),
#     fmt = 3,
#     # output = 'kableExtra',
#     # table.attr = 'data-quarto-disable-processing="true"'
#     )

## %% Adding labels ----------------------------

for (var in names(labels)) {
    print(var)
    colombia_data_frame[[var]] <- haven::labelled(
        colombia_data_frame[[var]], label = labels[var]
        )
}

colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m)
    ) %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        sic_3 = factor(sic_3),
        metro_area_code = factor(metro_area_code),
        JO_class = factor(JO_class, levels = c("Proprietorship", "Ltd. Co.", "Corporation", "Partnership"))
    ) %>%
    select(
        `J. Org.`= JO_class,
        share_sales_tax,
        ends_with("share") & !starts_with("log_") & !contains("nded")
        # all_of() %in% names(labels)
    ) %>%
    datasummary_skim(
    fun_numeric = list(
        `Missing (%)` = PercentMissing, #(x)sum(!is.na(x))/length(x), 
        Mean = Mean, 
        SD = SD, 
        # Min = Min,
        Q1 = P25,
        Median = Median,
        Q3 = P75#,
        # Max = Max
    ),
    fmt = 3,
    # output = 'kableExtra',
    # table.attr = 'data-quarto-disable-processing="true"'
    )|> 
    group_tt(
        i = list(
            "Share of Revenues" = 1,
            "Intermediates" = 5#,
            # "Share of Total Services" = 15
        )
    ) |>
    style_tt(
        i = c(1,6,15),
        bold = TRUE,
        line = "b"
    ) |>
    style_tt(
        i = 10,
        line = "b",
        line_color = "white"
    )

##  Testing ----------------------------------------

### %% Declaring variables ----------------------------


mc_cores <- detectCores()-2

conditions <- list(
    non_corp=quote(juridical_organization != 3),
    exporter=quote(share_exports > 0),
    importer=quote(share_imports > 0),
    importer_mats=quote(share_imports_materials > 0)
)

run_vars<-expand.grid(top_20_inds$sic_3,intermediates, stringsAsFactors = FALSE)
# run_vars_gnr<-expand.grid(top_20_inds$sic_3,intermediates_gnr, stringsAsFactors = FALSE)

rv_cond <- expand.grid(
    inds = top_20_inds$sic_3,
    inter = c("log_mats_share", "log_deductible_intermediates_share"),
    cond= conditions,
    stringsAsFactors = FALSE
)

rv_trim <- expand.grid(
    inds = top_20_inds$sic_3,
    inter = c("log_mats_share", "log_deductible_intermediates_share"),#"log_mats_share",
    # condition = list(trim=quote(log_mats_share > threshold_cut)),
    stringsAsFactors = FALSE
)

# test_ev_cond(311, "log_mats_share", data = colombia_data_frame, juridical_organization == 3)
# test_ev_cond(311, "log_mats_share", data = colombia_data_frame, share_exports > 0.1)
# test_ev_cond(311, "log_mats_share", data = colombia_data_frame, share_imports > 0)
# test_ev_cond(311, "log_mats_share", data = colombia_data_frame, share_imports_materials > 0)
## %% Testing ------------------------------------------

tax_ev_test_CD<-mcmapply(
    test_ev_cd,
    run_vars$Var1,
    run_vars$Var2,
    MoreArgs = list(data=test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

tax_ev_test_2t_2smpl<-mcmapply(
    test_ev_2t_2smpl_cond,
    sic=rv_cond$inds,
    var=rv_cond$inter,
    cond=rv_cond$cond,
    MoreArgs = list(data = test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)


tax_ev_test_trim<-mcmapply(
    test_ev_trim,
    sic=rv_trim$inds,
    var=rv_trim$inter,
    # cond=rv_trim$condition,
    MoreArgs = list(data = test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

tax_ev_test_2t<-mcmapply(
    test_ev_2t,
    sic=rv_trim$inds,
    var=rv_trim$inter,
    # cond=rv_trim$condition,
    MoreArgs = list(data = test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

tax_ev_test_exp<-mcmapply(
    \(x,y,data) test_ev_cond(x,y,data,share_exports > 0.1),
    rv_trim$inds,
    rv_trim$inter,
    # share_exports > 0.1,
    MoreArgs = list(
        data = test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

tax_ev_test_exp

tax_ev_test_imp<-mcmapply(
    \(x,y,data) test_ev_cond(x,y,data,share_imports > 0.1),
    rv_trim$inds,
    rv_trim$inter,
    # share_exports > 0.1,
    MoreArgs = list(
        data = test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

tax_ev_test_imp

tax_ev_test_imp_mats<-mcmapply(
    \(x,y,data) test_ev_cond(x,y,data,share_imports_materials > 0.1),
    rv_trim$inds,
    rv_trim$inter,
    # share_exports > 0.1,
    MoreArgs = list(
        data = test_data),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

tax_ev_test_imp_mats
# tax_ev_test_CD_gnr<-mcmapply(
#     test_ev_cd,
#     run_vars_gnr$Var1,
#     run_vars_gnr$Var2,
#     MoreArgs = list(data=colombia_data_frame),
#     SIMPLIFY = FALSE,
#     mc.cores = mc_cores
# )

# mapply(
#     test_ev_cd,
#     run_vars$Var1,
#     run_vars$Var2,
#     MoreArgs = list(data=colombia_data_frame),
#     SIMPLIFY = FALSE#,
#     # mc.cores = mc_cores
# )
# names(tax_ev_test_CD)<-paste(run_vars$Var1, run_vars$Var2)

## %% Recollecting results ------------------------

# tax_ev_test_CD_df<-do.call(rbind,tax_ev_test_CD)
# # tax_ev_test_CD_df$intermediates<-run_vars$Var2

# tax_ev_test_CD_df |> View()

# tax_ev_test_CD_df_gnr<-do.call(rbind,tax_ev_test_CD_gnr)
# # tax_ev_test_CD_df_gnr$intermediates<-run_vars_gnr$Var2

# tax_ev_test_CD_df_gnr |> View()
# tax_ev_test_tbl<-tax_ev_test_CD_df %>%
#     mutate(
#         `p-val` = 1-pnorm(t_stat),
#         stars = case_when(
#                 `p-val` <= 0.001 ~ "***",
#                 `p-val` <= 0.05 ~ "**",
#                 `p-val` <= 0.1 ~ "*",
#                 .default = ""
#             ),
#         tbl_res = glue::glue("{round(mean_V,4)} ({round(se,4)}){stars}"),
#         .before = 1
#     ) %>%
#     select(sic_3,intermediates,tbl_res) %>%
#     group_by(sic_3) %>%
#     pivot_wider(
#         names_from = intermediates,
#         values_from = tbl_res
#     ) %>%
#     select(
#         sic_3,starts_with("log")
#     )# |> View()
## %% Generating table results ----------------------

get_my_table <- function(tst_list){
    tbl<-do.call(rbind,tst_list)%>%
    mutate(
        `p-val` = 1-pnorm(t_stat),
        stars = case_when(
                `p-val` <= 0.001 ~ "***",
                `p-val` <= 0.05 ~ "**",
                `p-val` <= 0.1 ~ "*",
                .default = ""
            ),
        tbl_res = glue::glue("{round(mean_V,4)} ({round(se,4)}){stars}"),
        .before = 1
    ) %>%
    select(sic_3,intermediates,tbl_res) %>%
    group_by(sic_3) %>%
    pivot_wider(
        names_from = intermediates,
        values_from = tbl_res
    ) %>%
    select(
        sic_3,starts_with("log")
    )
    return(tbl)
}

get_my_table_2t <- function(tst_list, group_vars = "sic_3"){
    tbl<-do.call(rbind,tst_list)%>%
    mutate(
        `p-val` = 1-pnorm(abs(t_stat)),
        stars = case_when(
                `p-val` <= 0.001 ~ "***",
                `p-val` <= 0.05 ~ "**",
                `p-val` <= 0.1 ~ "*",
                .default = ""
            ),
        tbl_res = glue::glue("{round(mean_V,4)} ({round(se,4)}){stars}"),
        .before = 1
    ) %>%
    group_by(all_of(group_vars)) %>%
    select(sic_3,intermediates,tbl_res) %>%
    pivot_wider(
        names_from = intermediates,
        values_from = tbl_res
    ) %>%
    select(
        sic_3,starts_with("log")
    )
    return(tbl)
}

tax_ev_test_tbl<-get_my_table(tax_ev_test_CD)

test_tbl<-get_my_table(tax_ev_test_CD)[c("sic_3", "log_mats_share", "log_deductible_intermediates_share")] %>%
    left_join(
        get_my_table(tax_ev_test_exp),
        by = "sic_3",
        suffix = c("", "_exp")
    ) %>%
    left_join(
        get_my_table(tax_ev_test_imp),
        by = "sic_3",
        suffix = c("", "_imp")
    ) %>%
    left_join(
        get_my_table(tax_ev_test_imp_mats),
        by = "sic_3",
        suffix = c("", "_imp_mats")
    )

test_tbl

# tax_ev_test_tbl_gnr<-tax_ev_test_CD_df_gnr %>%
#     mutate(
#         `p-val` = 1-pnorm(t_stat),
#         stars = case_when(
#                 `p-val` <= 0.001 ~ "***",
#                 `p-val` <= 0.05 ~ "**",
#                 `p-val` <= 0.1 ~ "*",
#                 .default = ""
#             ),
#         tbl_res = glue::glue("{round(mean_V,4)} ({round(se,4)}){stars}"),
#         .before = 1
#     ) %>%
#     select(sic_3,intermediates,tbl_res) %>%
#     group_by(sic_3) %>%
#     pivot_wider(
#         names_from = intermediates,
#         values_from = tbl_res
#     ) %>%
#     select(
#         sic_3,starts_with("log")
#     )# |> View()

tax_ev_test_tbl |> View()

tax_ev_trim_tbl<-get_my_table(tax_ev_test_trim)
tax_ev_2t_tbl<-get_my_table_2t(tax_ev_test_2t)


## %% Save ----------------------------------------

save(
    tax_ev_test_tbl,#tax_ev_test_tbl_gnr,
    test_tbl,
    tax_ev_trim_tbl, tax_ev_2t_tbl,
    tax_ev_test_2t_2smpl, tax_ev_test_CD,
    tax_ev_test_trim, tax_ev_test_2t,
    rv_cond, rv_trim, run_vars,
    file="Code/Products/intermediates.RData"
)


## %% Review ----------------------------------------

# load("Code/Products/intermediates-1p.RData")
# load("Code/Products/intermediates.RData")

# load("Code/Products/global_vars.RData")

back_env_ev_tbl<- colombia_data_frame %>%
    ungroup() %>%
    filter(
        # sales > 0,
        # # !is.na(capital),
        # !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m)
    ) %>%
    left_join(
        tax_ev_test_tbl,
        by = "sic_3",
        suffix = c("", "_tax")
    ) %>%
    mutate(
        # gross_output = exp(y),
        ev_mean_rate = as.numeric(str_extract(log_deductible_intermediates_share_tax, "^\\d+\\.\\d+")),
        # ev_mean_rate = as.numeric(str_extract(log_mats_share_tax, "^\\d+\\.\\d+")),
        corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        corp_num = ifelse(juridical_organization==3,1,0),
        market_value = sum(gross_output, na.rm = TRUE),
        # gross_output_tax_purchases_rate = meandeductible_intermediates/gross_output_tax_purchases,
        ded_int_spending_total = sum(deductible_intermediates, na.rm = TRUE),
        gov_rev_loss = 0.06*deductible_intermediates*(1-exp(-ev_mean_rate)),
        # gov_rev_loss_perc_pot = gov_rev_loss/rowSums(cbind(gross_output,-deductible_intermediates), na.rm = FALSE),
        gov_rev_loss_total = sum(gov_rev_loss, na.rm = TRUE)
        # n = unique(plant) |> length()
    ) %>%
    group_by(sic_3) %>%
    summarise(
        n_sic = unique(plant) |> length(),
        n_Corp = unique(plant*corp_num) |> length()-1,
        gross_output_tax_purchases_rate_sic = mean(sales_tax_purchases/deductible_intermediates, na.rm = TRUE),
        revenues_sic = sum(sales, na.rm = TRUE),
        # ded_int_spending_total = sum(deductible_intermediates, na.rm = TRUE),
        sic_market_share = revenues_sic/min(market_value),
        sic_spending_share = sum(deductible_intermediates, na.rm = TRUE)/min(ded_int_spending_total),
        sic_rev_per_firm = mean(sales, na.rm = TRUE)/n_sic,
        sic_spending_per_firm = mean(deductible_intermediates, na.rm = TRUE)/n_sic,
        gov_rev_loss_sic = sum(gov_rev_loss, na.rm = TRUE),
        gov_rev_loss_perc_pot_sic = gov_rev_loss_sic/(sum(gross_output,-deductible_intermediates, na.rm = TRUE)*0.06),
        sic_gov_rev_loss_share = gov_rev_loss_sic/min(gov_rev_loss_total),
            # corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    filter( n_sic >= 100) %>%
    ungroup() %>%
    arrange(desc(sic_gov_rev_loss_share)) %>%
    select(sic_3, n_sic, n_Corp, ends_with("share"), ends_with("per_firm"), ends_with("sic")) %>%
    left_join(
        ciiu_3[,2:3],
        by = join_by(sic_3 == `Código`)
    ) %>%
    mutate(
        description = str_remove(`Descripción`,"Manufacture of |Manufacture ot "),
        description = str_replace_all(
            str_to_title(description),
            c(" And " = " and ", " Of " = " of ", " Or | Ord " = " or ")
        ),
        .before = sic_3
    ) %>%
    select(!`Descripción`)

back_env_ev_tbl |> View()

top_5_ev_inds <- back_env_ev_tbl$sic_3[2:6] #322 is not in the top 10 rev inds
top_5_ev_inds

top_10_ev_inds<- back_env_ev_tbl$sic_3[1:10] #322 is not in the top 10 rev inds
top_10_ev_inds

top_5_ev_inds_mag <- tax_ev_test_tbl %>% arrange(desc(log_mats_share)) %>% pull(sic_3)#back_env_ev_tbl$sic_3[2:6] #322 is not in the top 10 rev inds
top_5_ev_inds_mag




## %% Charactersitics by industry ------------------------
# load("Code/Products/intermediates.RData")
test_cont_tbl <- colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    mutate(
        total_gross_output = sum(gross_output, na.rm = TRUE),
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
    group_by(sic_3) %>%
    summarise(
        n = unique(plant) |> length(),
        n_Corp = unique(plant*as.numeric(juridical_organization==3)) |> length()-1,
        share_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        gross_output = sum(gross_output, na.rm = TRUE)/max(total_gross_output, na.rm = TRUE),
        sales = sum(sales, na.rm = TRUE)/max(total_sales, na.rm = TRUE),
        deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE)/max(total_deductible_intermediates, na.rm = TRUE),
        materials = sum(materials, na.rm = TRUE)/max(total_materials, na.rm = TRUE),
        share_exports = mean(share_exports, na.rm = TRUE),
        share_imports = mean(share_imports, na.rm = TRUE),
        share_imports_materials = mean(share_imports_materials, na.rm = TRUE),
        importers = mean(importer, na.rm = TRUE),
        exporters = mean(exporter, na.rm = TRUE),
        importers_mats = mean(importer_mats, na.rm = TRUE)
    ) %>%
    right_join(
        tax_ev_test_tbl[c("sic_3", "log_mats_share", "log_deductible_intermediates_share")],
        by = "sic_3"
    )

colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    mutate(
        total_gross_output = sum(gross_output, na.rm = TRUE),
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
    group_by(sic_3, Corp) %>%
    summarise(
        n = unique(plant) |> length(),
        age = mean(age, na.rm = TRUE),
        # n_Corp = unique(plant*as.numeric(juridical_organization==3)) |> length()-1,
        share_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        # gross_output = sum(gross_output, na.rm = TRUE)/max(total_gross_output, na.rm = TRUE),
        sales = sum(sales, na.rm = TRUE)/max(total_sales, na.rm = TRUE),
        # deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE)/max(total_deductible_intermediates, na.rm = TRUE),
        materials = sum(materials, na.rm = TRUE)/max(total_materials, na.rm = TRUE),
        share_exports = mean(share_exports, na.rm = TRUE),
        # share_imports = mean(share_imports, na.rm = TRUE),
        share_imports_materials = mean(share_imports_materials, na.rm = TRUE),
        exporters = mean(exporter, na.rm = TRUE),
        # importers = mean(importer, na.rm = TRUE),
        importers_mats = mean(importer_mats, na.rm = TRUE)
    ) %>%
    mutate(
        across(share_sales_tax:importers_mats, ~ round(.x*100, 1))
    ) %>%
    pivot_wider(
        names_from = Corp,
        values_from = n:importers_mats,
        names_sep = "_"
    ) |> View()

# TODO
# - Left join with tax_ev_test_tbl

## %% Save ----------------------------------------

save(
    tax_ev_test_tbl,#tax_ev_test_tbl_gnr,
    top_5_ev_inds, top_10_ev_inds,
    back_env_ev_tbl, top_5_ev_inds_mag,
    test_cont_tbl, test_tbl,
    tax_ev_trim_tbl, tax_ev_2t_tbl,
    tax_ev_test_trim, tax_ev_test_2t, tax_ev_test_CD,
    rv_cond, rv_trim, run_vars, tax_ev_test_2t_2smpl,
    file="Code/Products/intermediates.RData"
)

## %% Checking ----------------------------------------

load("Code/Products/intermediates.RData")

test_cont_tbl <- test_cont_tbl %>%
    mutate(
        across( 
            share_sales_tax:importers_mats,
            ~ round(.x*100, 1)
        ),
        sic_3 = factor(sic_3, levels = top_10_revenue$sic_3),# labels = top_10_revenue$description)
        sic_3_lab = factor(sic_3, levels = top_10_revenue$sic_3, labels = top_10_revenue$description)
    ) %>% select(
        sic_3, sic_3_lab, log_mats_share,# log_deductible_intermediates_share, 
        n, n_Corp, sales, share_sales_tax, share_exports, exporters
    ) %>%
    arrange(sic_3) #|> View()

test_tbl |> View()
test_cont_tbl |> View()

tax_ev_2t_tbl |> View()
tax_ev_trim_tbl |> View()
