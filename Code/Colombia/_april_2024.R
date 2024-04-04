library(tidyverse)
library(dplyr)
# library(data.table)
load("Code/Products/col_df.RData")
## Load packages and data ####
library(tidyverse)
library(ggplot2)
library(modeest)
library(fixest)
library(vtable)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
# load("Code/Products/functions.RData")
jo_class <- tibble(
    JO_code = 0:9,
    JO_class = c(
        "Proprietorship",
        "Ltd. Co.",
        "Partnership",
        "Corporation",
        "Partnership",
        "Partnership",
        "Corporation",
        # "Stock Co. (Corp.)",
        "Other",
        "Other",
        "Other"
    )
)
###
colombia_data_frame %>%
    group_by(sic_3) %>%
    summarise(
        n_sic = n(),
        revenues = sum(sales, na.rm = TRUE)
    ) %>%
    arrange(desc(revenues)) %>%
    mutate(
        n_cum = cumsum(n_sic),
        perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
        market_share = revenues/ sum(revenues)*100
    ) %>%
    select(sic_3, n_sic, market_share, perc, perc_acc) %>%
    arrange(desc(market_share)) %>%
    unique() %>%
    View()

###
industries <- col_df %>%
    group_by(sic) %>%
    summarise(
        n_sic = n(),
        revenues = sum(s5, na.rm = TRUE),
        corps_n = sum(==3, na.rm = TRUE)
    ) %>%
    arrange(desc(revenues)) %>%
    mutate(
        n_cum = cumsum(n_sic),
        perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
        market_share = revenues/ sum(revenues)*100,
        sic_3 = as.numeric(str_sub(as.character(sic), 1, 3))
    ) %>%
    select(sic, sic_3, n_sic, corps_n, market_share, perc, perc_acc) %>%
    arrange(sic_3, desc(market_share)) %>%
    unique()
col_df %>%
    group_by(sic) %>%
    summarise(
        n_sic = n(),
        revenues = sum(s5, na.rm = TRUE),
        corps_n = sum(x3==3, na.rm = TRUE)
    ) %>%
    arrange(desc(revenues)) %>%
    mutate(
        n_cum = cumsum(n_sic),
        n_perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
        market_share = revenues/ sum(revenues)*100,
        sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        corps_share = corps_n/n_sic * 100
    ) %>%
    select(sic, sic_3, n_sic, corps_n, corps_share, market_share, n_perc) %>%
    arrange(desc(corps_share))

candidate_inds<-colombia_data_frame %>%
    group_by(sic_3) %>%
    summarise(
        n_sic = n(),
        revenues = sum(sales, na.rm = TRUE),
        corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    arrange(desc(revenues)) %>%
    mutate(
        n_cum = cumsum(n_sic),
        n_perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
        market_share = revenues/ sum(revenues)*100,
        # sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        corps_share = corps_n/n_sic * 100
    ) %>%
    select(sic_3, n_sic, corps_n, corps_share, market_share, n_perc) %>%
    arrange(desc(corps_share)) %>%
    filter(n_perc>1.2)
##

ciiu_data <- read.csv("Data/Colombia/ciiu-rev2-en.csv", skip = 3)

ciiu_3 <- ciiu_data %>%
    filter(
        Nivel == "Agrupaciones"
    )
ciiu_4 <- ciiu_data %>%
    filter(
        Nivel == "Grupos"
    )

candidate_inds %>%
    left_join(
        ciiu_3[,2:3],
        by = join_by(sic_3 == `Código`)
    ) %>%
    View()
industries %>%
    left_join(
        ciiu_4[, 2:3],
        by = join_by(sic == `Código`)
    ) %>%
    left_join(
        ciiu_3[,2:3], 
        by = join_by(sic_3 == `Código`)
    ) %>%
    View()

###

colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    ungroup() %>%
    filter(
        sales>0,
        capital>0
    ) %>%
    mutate(
        JO = factor(
            juridical_organization, 
            levels = 0:9, 
            labels = c(
                "Proprietorships",
                "Ld. Partnership",
                "Collective",
                "Corporation",
                "De Facto Corp.",
                "Joint Partnership",
                "Joint Stock Co.",
                "Cooperative",
                "Official Entity",
                "Religious Community")
        ),
        Corp = ifelse(juridical_organization==3,"Corp","Other"),
        capital_share = capital/sales,
        consumed_energy_share = consumed_energy/sales,
        mats_serv_share = mats_serv/sales,
        materials_share = materials/sales,
        labor_employee_years_share = labor_employee_years/sales,
        gen_exp_share = general_expenditure/sales,
        inds_exp_share_exp = industrial_expenditure/total_expenditure,
        non_deductible_expenses_share_exp = non_deductible_expenses/total_expenditure,
        deductible_expenses_share_exp = deductible_expenses/total_expenditure,
        inds_exp_non_deductible_share = inds_exp_non_deductible/industrial_expenditure,
        inds_exp_deductible_share = inds_exp_deductible/industrial_expenditure,
        total_expenses_share = total_expenditure/sales,
        serv_share_exp = services/total_expenditure,
        energy_serv_share = (consumed_energy+industrial_expenditure)/sales
    ) %>%
    feols(
        sw(
            capital_share,
            consumed_energy_share,
            materials_share,
            skilled_wage_bill_share,
            unskilled_wage_bill_share,
            total_expenses_share
            ) ~ JO_class|
        sic_3+metro_area_code+year,
        data = ., cluster = ~sic_3+year
    ) %>%
    etable(
        # div.class = "table"
    )

colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    ungroup() %>%
    filter(
        sales>0,
        capital>0
    ) %>%
    mutate(
        JO = factor(
            juridical_organization, 
            levels = 0:9, 
            labels = c(
                "Proprietorships",
                "Ld. Partnership",
                "Collective",
                "Corporation",
                "De Facto Corp.",
                "Joint Partnership",
                "Joint Stock Co.",
                "Cooperative",
                "Official Entity",
                "Religious Community")
        ),
        Corp = ifelse(juridical_organization==3,"Corp","Other"),
        capital_share = capital/sales,
        consumed_energy_share = consumed_energy/sales,
        mats_serv_share = mats_serv/sales,
        materials_share = materials/sales,
        labor_employee_years_share = labor_employee_years/sales,
        gen_exp_share = general_expenditure/sales,
        inds_exp_share_exp = industrial_expenditure/total_expenditure,
        non_deductible_expenses_share_exp = non_deductible_expenses/total_expenditure,
        deductible_expenses_share_exp = deductible_expenses/total_expenditure,
        inds_exp_non_deductible_share = inds_exp_non_deductible/industrial_expenditure,
        inds_exp_deductible_share = inds_exp_deductible/industrial_expenditure,
        total_expenses_share = total_expenditure/sales,
        serv_share_exp = services/total_expenditure,
        energy_serv_share = (consumed_energy+industrial_expenditure)/sales
    ) %>%
    feols(
        sw(
            log(capital),
            log(sales),
            log(capital/sales)
            ) ~ JO_class|
        sic_3+metro_area_code+year,
        data = ., cluster = ~sic_3+year
    ) %>%
    etable(
        div.class = "table"
    )


## What predicts switching to Corp

colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    ungroup() %>%
    filter(
        sales>0,
        capital>0
    ) %>%
    mutate(
        JO = factor(
            juridical_organization, 
            levels = 0:9, 
            labels = c(
                "Proprietorships",
                "Ld. Partnership",
                "Collective",
                "Corporation",
                "De Facto Corp.",
                "Joint Partnership",
                "Joint Stock Co.",
                "Cooperative",
                "Official Entity",
                "Religious Community")
        ),
        Corp = ifelse(juridical_organization==3,1,0),
        capital_share = capital/sales,
        consumed_energy_share = consumed_energy/sales,
        mats_serv_share = mats_serv/sales,
        materials_share = materials/sales,
        labor_employee_years_share = labor_employee_years/sales,
        gen_exp_share = general_expenditure/sales,
        inds_exp_share_exp = industrial_expenditure/total_expenditure,
        non_deductible_expenses_share_exp = non_deductible_expenses/total_expenditure,
        deductible_expenses_share_exp = deductible_expenses/total_expenditure,
        inds_exp_non_deductible_share = inds_exp_non_deductible/industrial_expenditure,
        inds_exp_deductible_share = inds_exp_deductible/industrial_expenditure,
        total_expenses_share = total_expenditure/sales,
        serv_share_exp = services/total_expenditure,
        energy_serv_share = (consumed_energy+industrial_expenditure)/sales
    ) %>%
    group_by(plant) %>%
    mutate(
        switch_to_corp = ifelse(lag(Corp, order_by = year)-Corp<0,1,0),
        reform_1986 = ifelse(year>=87,1,0)
    ) %>%
    feols(
        sw(
            Corp#,
            # switch_to_corp
            ) ~ sw(lag(log(capital), k=1), lag(log(sales)), lag(log(capital/sales))) + reform_1986+ lag(share_sales_tax)+ age + total_owners + lag(log(generated_energy))|
        sic_3+metro_area_code+year,
        data = ., panel.id =~plant+year , cluster = ~sic_3+year
    ) %>%
    etable(
        # div.class = "table"
    )


colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    ungroup() %>%
    filter(
        sales>0,
        capital>0
    ) %>%
    mutate(
        JO = factor(
            juridical_organization, 
            levels = 0:9, 
            labels = c(
                "Proprietorships",
                "Ld. Partnership",
                "Collective",
                "Corporation",
                "De Facto Corp.",
                "Joint Partnership",
                "Joint Stock Co.",
                "Cooperative",
                "Official Entity",
                "Religious Community")
        ),
        Corp = ifelse(juridical_organization==3,1,0),
        capital_share = capital/sales,
        consumed_energy_share = consumed_energy/sales,
        mats_serv_share = mats_serv/sales,
        materials_share = materials/sales,
        labor_employee_years_share = labor_employee_years/sales,
        gen_exp_share = general_expenditure/sales,
        inds_exp_share_exp = industrial_expenditure/total_expenditure,
        non_deductible_expenses_share_exp = non_deductible_expenses/total_expenditure,
        deductible_expenses_share_exp = deductible_expenses/total_expenditure,
        inds_exp_non_deductible_share = inds_exp_non_deductible/industrial_expenditure,
        inds_exp_deductible_share = inds_exp_deductible/industrial_expenditure,
        total_expenses_share = total_expenditure/sales,
        serv_share_exp = services/total_expenditure,
        energy_serv_share = (consumed_energy+industrial_expenditure)/sales
    ) %>%
    group_by(plant) %>%
    mutate(
        switch_to_corp = ifelse(lag(Corp, order_by = year)-Corp<0,1,0),
        reform_1986 = ifelse(year>=87,1,0)
    ) %>%
    ungroup() %>%
    summarise(
        total_switchs = sum(switch_to_corp, na.rm = TRUE)
    )
    

## Sumary stats


# Use dfSummary for a detailed summary of each variable
# summary_stats <- 
colombia_data_frame %>%
    ungroup() %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        sic_3 = factor(sic_3),
        metro_area_code = factor(metro_area_code),
    ) %>%
    filter(
        sales > 0 ,
        capital > 0
    ) %>%
    select(
        capital,
        mats_serv,
        labor_employee_years,
        sales,
        share_sales_tax,
        # age,
        consumed_energy,
        deductible_expenses,
        # year,
        # juridical_organization,
        # sic_3,
        # metro_area_code
    ) %>%
    st()

print(summary_stats)
