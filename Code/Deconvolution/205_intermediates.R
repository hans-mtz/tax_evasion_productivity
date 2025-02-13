# Load data and packages ---------------
library(tidyverse)
library(parallel)
library(fixest)
library(modelsummary)
library(tinytable)
load("Code/Products/colombia_data.RData")
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

## Definitions of Intermediates -------------
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
    "log_repair_maint_share","log_inds_nded_share"
)

intermediates_gnr<-c(
    "log_share","log_mats_share", "log_energy_share", "log_services_share"
)

intermediates %in% names(colombia_data_frame) |> all()

intermediates_gnr %in% names(colombia_data_frame) |> all()
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
#     summarise(
#         across(
#             ends_with("share")&starts_with("log"),
#             list(
#                 mean= ~mean(.x, na.rm = TRUE),
#                 n = ~sum(!is.na(.x))
#             )
#         )
#     ) |> View()

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

for (var in names(labels)) {
    print(var)
    colombia_data_frame[[var]] <- haven::labelled(
        colombia_data_frame[[var]], label = labels[var]
        )
}

colombia_data_frame %>%
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
        i = c(1,6,13),
        bold = TRUE,
        line = "b"
    ) |>
    style_tt(
        i = 10,
        line = "b",
        line_color = "white"
    )

## Testing ----------------------------------------

## Declaring variables ----------------------------


mc_cores <- detectCores()-2

run_vars<-expand.grid(top_20_inds$sic_3,intermediates, stringsAsFactors = FALSE)
run_vars_gnr<-expand.grid(top_20_inds$sic_3,intermediates_gnr, stringsAsFactors = FALSE)


## Testing ------------------------------------------

tax_ev_test_CD<-mcmapply(
    test_ev_cd,
    run_vars$Var1,
    run_vars$Var2,
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

tax_ev_test_CD_gnr<-mcmapply(
    test_ev_cd,
    run_vars_gnr$Var1,
    run_vars_gnr$Var2,
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

# mapply(
#     test_ev_cd,
#     run_vars$Var1,
#     run_vars$Var2,
#     MoreArgs = list(data=colombia_data_frame),
#     SIMPLIFY = FALSE#,
#     # mc.cores = mc_cores
# )
# names(tax_ev_test_CD)<-paste(run_vars$Var1, run_vars$Var2)

## Recollecting results ------------------------

tax_ev_test_CD_df<-do.call(rbind,tax_ev_test_CD)
tax_ev_test_CD_df$intermediates<-run_vars$Var2

tax_ev_test_CD_df |> View()

tax_ev_test_CD_df_gnr<-do.call(rbind,tax_ev_test_CD_gnr)
tax_ev_test_CD_df_gnr$intermediates<-run_vars_gnr$Var2

tax_ev_test_CD_df_gnr |> View()

## Generating table results ----------------------

tax_ev_test_tbl<-tax_ev_test_CD_df %>%
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
    )# |> View()

tax_ev_test_tbl_gnr<-tax_ev_test_CD_df_gnr %>%
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
    )# |> View()




## Save ----------------------------------------

save(
    tax_ev_test_tbl, tax_ev_test_tbl_gnr,
    file="Code/Products/intermediates.RData"
)
