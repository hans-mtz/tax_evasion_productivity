## Load packages and data ####
library(tidyverse)
library(ggplot2)
library(modeest)
library(fixest)


load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
# load("Code/Products/functions.RData")

setFixest_dict(c(
    share_sales_tax = "Sales Tax Rate",
    `relevel(factor(juridical_organization),ref="3")0` = "Proprietorships",
    `relevel(factor(juridical_organization),ref="3")1` = "Ld. Partnership",
    `relevel(factor(juridical_organization),ref="3")2` = "Collective",
    `relevel(factor(juridical_organization),ref="3")3` = "Corporation",
    `relevel(factor(juridical_organization),ref="3")4` = "De Facto Corp.",
    `relevel(factor(juridical_organization),ref="3")5` = "Joint Partnership",
    `relevel(factor(juridical_organization),ref="3")6` = "Joint Stock Co.",
    `relevel(factor(juridical_organization),ref="3")7` = "Cooperative",
    `relevel(factor(juridical_organization),ref="3")8` = "Official Entity",
    `relevel(factor(juridical_organization),ref="3")9` = "Religious Community",
    `polym(m,k,l,degree=2,raw=TRUE)1.0.0` = "\\(m\\)",
    `polym(m,k,l,degree=2,raw=TRUE)2.0.0` = "\\(m^2\\)",
    `polym(m,k,l,degree=2,raw=TRUE)0.1.0` = "\\(k\\)",
    `polym(m,k,l,degree=2,raw=TRUE)0.2.0` = "\\(k^2\\)",
    `polym(m,k,l,degree=2,raw=TRUE)0.0.1` = "\\(l\\)",
    `polym(m,k,l,degree=2,raw=TRUE)0.0.2` = "\\(l^2\\)",
    `polym(m,k,l,degree=2,raw=TRUE)1.1.0` = "\\(mk\\)",
    `polym(m,k,l,degree=2,raw=TRUE)1.0.1` = "\\(ml\\)",
    `polym(m,k,l,degree=2,raw=TRUE)0.1.1` = "\\(kl\\)",
    log_share = "\\(log(s)\\)",
    sic_3 = "Industry",
    year = "Year",
    `log(energy/sales)` = "\\(log(Energy/Revenue)\\)",
    `log(mats_serv/sales)` = "\\(log(Mats+Serv/Revenue)\\)",
    metro_area_code = "Metro Area",
    `factor(fiscal_period)83` = "84-86 F.P.",
    `factor(fiscal_period)86` = "87-91 F.P."#,
    # "note 1" = "Reference group is J.O. (9) in 1981. Production Function Polynomial and interactions not displayed.",
    # "note 2" = "Reference group is J.O. (9) in Fiscal Period 1981-1983. Only interactions displayed."
))

## Firms' income taxes are defined by the type of juridical
# organization as a fixed percentage of net wealth
# Sale taxes are defined by industry and country region
# Of course, both changed over time

# The deductable inputs vary over time
# Industrial expenditure distribution showed the top of the 
# distribution had the lowest log share ratio

# Learn productivity from non-deductible intermediates,
# assume productivity in non-deductible intermediates is the
# same for deductible intermediates, and learn
# tax evasion distribution from imposing non-deductible
# intermediates productivity to deductible intermediates

# Update the model to include non-deductable inputs, like the 
# Spanish paper

# Before 1987, Corporations' income were taxed 40% and limited liability
# and partnerships paid 20%. Corporations reported to the superintendent of
# Corporations.
# I

## Exploratory analysis ####
colombia_data_frame %>%
    filter(
        year <= 84,
        year >= 83
    ) %>%
    feols(
        log_share ~ relevel(factor(juridical_organization), ref="9")*factor(year) +share_sales_tax| factor(sic_3)+factor(metro_area_code),
        data = .
    ) %>%
    summary()

colombia_data_frame %>%
    filter(
        year <= 86
    ) %>%
    feols(
        log_share ~ relevel(factor(year), ref="82")*relevel(factor(juridical_organization), ref="9") +share_sales_tax| factor(sic_3)+factor(metro_area_code),
        data = .
    ) %>%
    summary()

colombia_data_frame %>%
    ungroup() %>%
    mutate(
        fiscal_period = case_when(
            year <= 83 ~ '74',
            year <= 86 ~ '83',
            .default = '86'
        )
    ) %>%
    # filter(
    #     year <= 86
    # ) %>%
    # select(fiscal_period, year)
    feols(
        log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(fiscal_period)+share_sales_tax*factor(fiscal_period)| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable(
        view = TRUE
    )

# colombia_data_frame %>%
#     filter(
#         year <= 84
#     ) %>%
#     feols(
#         log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="9")*relevel(factor(year), ref="82") +share_sales_tax| factor(sic_3)+factor(metro_area_code),
#         data = .
#     ) %>%
#     etable()

## non-deductible intermediate

colombia_data_frame %>%
    # filter(
    #     year <= 83
    # ) %>%
    feols(
        log(industrial_expenditure/sales) ~ polym(materials, k, l,log(industrial_expenditure), degree = 2, raw = TRUE)+factor(juridical_organization)*factor(year) +share_sales_tax| csw0(sic_3,metro_area_code,year),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable()

colombia_data_frame %>%
    feols(
        log(consumed_energy/sales) ~ polym(log(mats_serv), k, l,log(consumed_energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization),ref="3")*factor(year) +share_sales_tax| csw0(sic_3,metro_area_code),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable(
    )

colombia_data_frame %>%
    feols(
        log(mats_serv/sales) ~ polym(log(mats_serv), k, l,log(energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization),ref="3")*factor(year) +share_sales_tax| csw0(sic_3,metro_area_code),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable()

## Evidence of bumping ####
# at the individual income tax treshholds
# for jur org 0 and 1, but not for the rest 2-9
# For Proprietorships (0) and Limited partnerships
colombia_data_frame %>%
    filter(
        year<=83,
        juridical_organization==0
    ) %>%
    pull(sales) %>%
    hist(., 
    breaks=30000,
    xlim=c(0,5000))
abline(v=real_tax_tresh_1983)

colombia_data_frame %>%
    filter(
        year<=83,
        juridical_organization==1
    ) %>%
    pull(sales) %>%
    hist(., 
    breaks=100000,
    xlim=c(0,5000))
abline(v=real_tax_tresh_1983)

colombia_data_frame %>%
    filter(
        year<=83,
        juridical_organization %in% c(0,1)
    ) %>%
    pull(sales) %>%
    hist(., 
    breaks=100000,
    xlim=c(0,5000))
abline(v=real_tax_tresh_1983)
title("Revenue distribution\n Juridical Org. 0 and 1")

## 1983 Reform ####
# Sales tax rates 1984 from previous

# en Perry and others
VAT_1984<-tribble(
    ~Sector, ~CIIU, ~Tax1984, ~TaxBefore, ~Change,
    "Textiles", 321, 0.1, 0.06, "increased",
    "Oil and coal derivates", 354, 0.14, 0.10, "increased",
    "Plastics", 356, 0.1, 0.15, "decreased",
    "Paper", 341, 0.10, 0.15, "decreased",
    "Equipment and machinery", 382, 0.10, 0.06,"increased",
    "Transportation", 384, 0.10, 0.06, "increased"
)

# Data 

(xtabs(share_sales_tax~.,
    stats::aggregate(
        share_sales_tax~sic_3+year,
        colombia_data_frame %>%
        filter(
            sales>0,
            year <= 86
            ),
        mean)
) -> avg_sales_tax)

colombia_data_frame %>%
    filter(
        sales>0#,
        # year <= 86
        ) %>%
        mutate(
            share_i = consumed_energy/sales
        )|>
    stats::aggregate(
            share_i~sic_3+juridical_organization,
            data=_,
            mean) |>
    xtabs(share_i~.,
        data= _
    )

# Increased 382 , 371, 361, 362, 341, 321, 322, 324,383, 313 Drinks (85)
# Decrease 353 Petroleum ? 314 Tobacco, 384
# No change 311, 312 exempt
# No change 313, 323, 331, 332, 342, 352,354, 351?, 355, 
# 356, 369, 372, 381, 385, 390

avg_sales_tax %>%
    as_tibble() %>%
    pivot_wider(
        # year,
        names_from = year,
        values_from = n
    ) %>%
    mutate(
        delta_84 = `84`-`83`,
        delta_85 = `85`-`84`
    ) %>%
    print(n=29)

colombia_data_frame %>%
    ungroup() %>%
    filter(
        sales>0
    ) %>%
    pivot_wider(
        # year,
        names_from = juridical_organization,
        values_from = share_sales_tax,
        names_prefix = "y_"
    ) %>%
    group_by(sic_3) %>%
    summarise( 
       across(
            starts_with("y_"),
            list(
                mean = ~mean(.x, na.rm = TRUE),
                CI_l = ~sd(.x, na.rm = TRUE)/sqrt(n())*1.96
            )
        )
    ) %>%
    # select(contains(c("sic","82","83","84","85"))) %>%
    print(n=29)

sales_tax_change<-tribble(
    ~sic_3, ~Change, ~Change_Year,
    311, "no change", 84,
    312, "no change", 84,
    313, "increased", 85,
    314, "decreased", 84,
    321, "increased", 84,
    322, "increased", 84,
    323, "no change", 84,
    324, "increased", 84,
    331, "no change", 84,
    332, "no change", 84,
    341, "increased", 84, # Paper, data increase 10->15, Perry decreased 15->10
    342, "no change", 84,
    351, "decreased", 84, # Industrial Chemicals,
    352, "no change", 84,
    353, "decreased", 83, # Petroleum refineris from 30% to 8% 83 and 2% 84
    354, "no change", 84,
    355, "no change", 84,
    356, "no change", 84,
    361, "no change", 84,
    362, "no change", 84,
    369, "no change", 84,
    371, "increased", 84,
    372, "no change", 84,
    381, "no change", 84,
    382, "increased", 85, #Machinery mfg from 8% to 11% in 84, to 14% in 85
    383, "increased", 85,
    384, "increased", 85,
    385, "increased", 85,
    390, "no change", 84
)
# save()
cdf<-colombia_data_frame %>%
    left_join(sales_tax_change) %>%
    mutate(
        time = ifelse(year>=Change_Year,1,0),
        treat = factor(Change)
    ) %>%
    # filter(
    #     year <= 86
    # )

did <- feols(
        log_share ~ time*treat | csw0(metro_area_code,juridical_organization),
        data = cdf, cluster=~year+sic_3)

etable(did)

did_poly <- feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+time*treat|csw0(metro_area_code,juridical_organization),
        data = cdf, cluster=~sic_3+year
    )
    
etable(did_poly)

## Regressions ####
colombia_data_frame %>%
    left_join(sales_tax_change) %>%
    mutate(
        time = ifelse(year>=Change_Year,1,0),
        treat = factor(Change)
    ) %>%
    filter(
        year <= 86
    ) %>%
    feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(year), ref="81")*relevel(treat, ref="decreased")+share_sales_tax|csw0(sic_3,year,metro_area_code, juridical_organization),
        data = _, cluster=~sic_3+year
    ) |> etable()

colombia_data_frame %>%
    left_join(sales_tax_change) %>%
    mutate(
        time = ifelse(year>=Change_Year,1,0),
        treat = factor(Change)
    ) %>%
    filter(
        year <= 86
    ) %>%
    feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(sic_3), ref="311")*factor(time)+share_sales_tax|sw0(sic_3,year,metro_area_code, juridical_organization),
        data = ., cluster=~sic_3+year
    ) |> etable()

feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(year), ref="81")*relevel(treat, ref="increased")+share_sales_tax|sw0(sic_3,year,metro_area_code, juridical_organization),
        data = cdf, cluster=~sic_3+year
    ) |> etable()

feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(year), ref="83")*treat+share_sales_tax|sw0(sic_3,year,metro_area_code, juridical_organization),
        data = cdf, cluster=~treat+time
    ) |> etable()

# New real winners
feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+share_sales_tax+relevel(factor(juridical_organization),ref="3")*factor(year)|csw0(sic_3,year,metro_area_code),
        data = cdf, cluster=~sic_3+year
    ) |> etable()

feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+share_sales_tax+relevel(factor(juridical_organization),ref="3")*factor(year)|csw0(sic_3,metro_area_code),
        data = colombia_data_frame, cluster=~sic_3+year
    ) |> etable()

# feols(
#         log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+share_sales_tax+relevel(factor(juridical_organization),ref="9")*factor(year)|csw0(sic_3,year,metro_area_code),
#         data = colombia_data_frame, cluster=~sic_3+year
#     ) -> reg_tbl_tax_jo

# colombia_data_frame %>%
#     ungroup() %>%
#     mutate(
#         fiscal_period = case_when(
#             year <= 83 ~ '74',
#             year <= 86 ~ '83',
#             .default = '86'
#         )
#     ) %>%
#     # filter(
#     #     year <= 86
#     # ) %>%
#     # select(fiscal_period, year)
#     feols(
#         log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(fiscal_period)| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
#         data = . , cluster = ~sic_3+year
#     ) %>%
#     etable()

colombia_data_frame %>%
    ungroup() %>%
    mutate(
        fiscal_period = case_when(
            year <= 83 ~ '74',
            year <= 86 ~ '83',
            .default = '86'
        )
    ) %>%
    # filter(
    #     year <= 86
    # ) %>%
    # select(fiscal_period, year)
    feols(
        log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(fiscal_period)+share_sales_tax| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable()


colombia_data_frame %>%
    ungroup() %>%
    mutate(
        fiscal_period = case_when(
            year <= 83 ~ '74',
            year <= 86 ~ '83',
            .default = '86'
        ),
        energy_generator = ifelse(generated_energy>0,1,0)
    ) %>%
    # filter(
    #     year <= 86
    # ) %>%
    # select(fiscal_period, year)
    feols(
        log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(fiscal_period)+share_sales_tax+energy_generator| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable()

## Noup

colombia_data_frame %>%
    ungroup() %>%
    mutate(
        fiscal_period = case_when(
            year <= 83 ~ '74',
            year <= 86 ~ '83',
            .default = '86'
        ),
        energy_generator = ifelse(generated_energy>0,1,0)
    ) %>%
    feols(
        log(mats_serv/sales) ~ polym(mats_serv, k, l,log(purchased_energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(year)+share_sales_tax| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable()
colombia_data_frame %>%
    ungroup() %>%
    mutate(
        fiscal_period = case_when(
            year <= 83 ~ '74',
            year <= 86 ~ '83',
            .default = '86'
        ),
        energy_generator = ifelse(generated_energy>0,1,0)
    ) %>%
    feols(
        log(energy/sales) ~ polym(mats_serv, k, l,log(energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(fiscal_period)+share_sales_tax+energy_generator*factor(fiscal_period)| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable()
colombia_data_frame %>%
    ungroup() %>%
    mutate(
        fiscal_period = case_when(
            year <= 83 ~ '74',
            year <= 86 ~ '83',
            .default = '86'
        ),
        energy_generator = ifelse(generated_energy>0,1,0)
    ) %>%
    feols(
        log(consumed_energy/sales) ~ polym(mats_serv, k, l,log(consumed_energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(fiscal_period)+share_sales_tax+energy_generator*factor(fiscal_period)| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable()
colombia_data_frame %>%
    ungroup() %>%
    mutate(
        fiscal_period = case_when(
            year <= 83 ~ '74',
            year <= 86 ~ '83',
            .default = '86'
        ),
        energy_generator = ifelse(generated_energy>0,1,0)
    ) %>%
    feols(
        log(purchased_energy/sales) ~ polym(mats_serv, k, l,log(purchased_energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(fiscal_period)+share_sales_tax+energy_generator*factor(fiscal_period)| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable()
colombia_data_frame %>%
    mutate(
        energy_generator = ifelse(generated_energy>0,1,0)
    ) %>%
    feols(
        log(energy/sales) ~ polym(log(mats_serv), k, l,log(energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(year) +share_sales_tax+energy_generator| csw0(sic_3,metro_area_code),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable(
    )

colombia_data_frame %>%
    mutate(
        energy_generator = ifelse(generated_energy>0,1,0)
    ) %>%
    feols(
        log(consumed_energy/sales) ~ polym(log(materials+industrial_expenditure), k, l,log(consumed_energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(year) +share_sales_tax+factor(energy_generator)| csw0(sic_3,metro_area_code),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable(
    )

colombia_data_frame %>%
    mutate(
        energy_generator = ifelse(generated_energy>0,1,0)
    ) %>%
    feols(
        log(purchased_energy/sales) ~ polym(log(mats_serv), k, l,log(purchased_energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="3")*factor(year) +share_sales_tax+factor(energy_generator)| csw0(sic_3,metro_area_code),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable(
    )

colombia_data_frame %>%
    feols(
        log(mats_serv/sales) ~ polym(log(mats_serv), k, l,log(energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization),ref="3")*factor(year) +share_sales_tax| csw0(sic_3,metro_area_code),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable()

colombia_data_frame %>%
    feols(
        log((materials+deductible_expenses)/sales) ~ polym(log(materials+deductible_expenses), k, l,log(consumed_energy), degree = 2, raw = TRUE)+relevel(factor(juridical_organization),ref="3")*factor(year) +share_sales_tax| csw0(sic_3,metro_area_code),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable()

## Are technologies different? Corporations vs others? ####

colombia_data_frame %>%
    filter(
        sales>0#,
        # year <= 86
        ) %>%
        mutate(
            share_i = consumed_energy/sales,
            Corp = ifelse(juridical_organization==3,"Corp","Other")
        )|>
    stats::aggregate(
            share_i~sic_3+Corp,
            data=_,
            mean) |>
    xtabs(share_i~.,
        data= _
    )
    
colombia_data_frame %>%
    filter(
        sales>0#,
        # year <= 86
        ) %>%
        mutate(
            share_i = capital/sales,
            Corp = ifelse(juridical_organization==3,"Corp","Other")
        )|>
    stats::aggregate(
            share_i~sic_3+Corp,
            data=_,
            mean) |>
    xtabs(share_i~.,
        data= _
    )

colombia_data_frame %>%
    ungroup() %>%
    filter(
        sales>0
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
        capital_share = capital/sales,
        consumed_energy_share = consumed_energy/sales,
        Corp = ifelse(juridical_organization==3,"Corp","Other"),
        mats_serv_share = mats_serv/sales,
        labor_employee_years_share = labor_employee_years/sales,
        gen_exp_share = general_expenditure/total_expenditure,
        inds_exp_share = industrial_expenditure/total_expenditure,
        non_deductible_expenses_share = non_deductible_expenses/total_expenditure,
        deductible_expenses_share = deductible_expenses/total_expenditure,
        inds_exp_non_deductible_share = inds_exp_non_deductible/industrial_expenditure,
        inds_exp_deductible_share = inds_exp_deductible/industrial_expenditure,
        total_expenses_share = total_expenditure/sales,
        serv_share = services/total_expenditure
    ) %>%
    # pivot_wider(
    #     # year,
    #     names_from = Corp,
    #     values_from = share_sales_tax,
    #     names_prefix = "y_"
    # ) %>%
    group_by(sic_3,Corp) %>%
    mutate( n=n()) %>%
    summarise( 
       across(
            ends_with("_share"),
            list(
                mean = ~mean(.x, na.rm = TRUE),
                se = ~sd(.x, na.rm = TRUE)/sqrt(n)#*1.96
            )
        )
    ) %>%
    # select(sic_3, Corp)#, starts_with("inds_exp"))# contains("expenses_share"))
    mutate(
        # Energy  = str_glue("{round(consumed_energy_share_mean,1)} ({round(consumed_energy_share_se,2)})"),
        # Capital = str_glue(
        #     "{round(capital_share_mean,2)} ({round(capital_share_se,2)})"
        # ),
        # Labor = str_glue(
        #     "{round(labor_employee_years_share_mean,2)} ({round(labor_employee_years_share_se,2)})"
        # ),
        # `Skilled Labor` = str_glue(
        #     "{round(skilled_labor_share_mean,2)} ({round(skilled_labor_share_se,2)})"
        # ),
        # `Unskilled Labor` = str_glue(
        #     "{round(unskilled_labor_share_mean,2)} ({round(unskilled_labor_share_se,2)})"
        # ),
        `Total Expenditure` = str_glue(
            "{round(total_expenses_share_mean,2)} ({round(total_expenses_share_se,2)})"#,
            # .sep = "\n"
        ),
        `Services (GNR)` = str_glue(
            "{round(serv_share_mean,2)} ({round(serv_share_se,2)})"
        ),
        `General Exp.` = str_glue(
            "{round(gen_exp_share_mean,2)} ({round(gen_exp_share_se,2)})"
        ),
        `Industrial Exp.` = str_glue(
            "{round(inds_exp_share_mean,2)} ({round(inds_exp_share_se,2)})"
        ),
        `Deductible Exp.` = str_glue(
            "{round(deductible_expenses_share_mean,2)} ({round(deductible_expenses_share_se,2)})"
        ),
        `Deductible Inds. Exp.` = str_glue(
            "{round(inds_exp_deductible_share_mean,2)} ({round(inds_exp_deductible_share_se,2)})"
        )
    ) %>%
    # select(
    #     sic_3, Corp, Energy, Capital, Labor, `Skilled Labor`,`Unskilled Labor`
    # )
    select(
        sic_3, Corp, `Total Expenditure`, `Services (GNR)`,`General Exp.`,`Industrial Exp.`,
        `Deductible Exp.`,`Deductible Inds. Exp.`
    )


colombia_data_frame %>% 
    ungroup() %>%
    mutate(
        # sold_energy = purchased_energy+generated_energy-consumed_energy
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
        total_energy_sold = sum(sold_energy, na.rm=TRUE),
        total_energy_purchased = sum(purchased_energy, na.rm = TRUE)
    ) %>%
    group_by(JO) %>% 
    summarise(
        # p_purchased = mean(p_energy_purchased, na.rm = TRUE), 
        # p_sold_purchase = mean(p_energy_sold/p_energy_purchased, na.rm = TRUE),
        # mean_energy_sold = mean(sold_energy, na.rm = TRUE),
        # total_energy_sold_JO = sum(sold_energy, na.rm = TRUE),
        # percent_energy_sold_JO = sum(sold_energy/total_energy_sold, na.rm = TRUE)*100,
        # mean_energy_purchased_JO = mean(purchased_energy, na.rm = TRUE),
        # percent_energy_purchased_JO = sum(sold_energy/total_energy_purchased, na.rm = TRUE)*100
        generated_energy_mean = mean(generated_energy, na.rm=TRUE),
        generated_energy_perc = mean(generated_energy/consumed_energy, na.rm=TRUE)*100
    )
  

## Dec, 2023 ####

colombia_data_frame %>%
    ungroup() %>%
    filter(
        sales>0
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
            skilled_labor_share,
            unskilled_labor_share,
            total_expenses_share,
            serv_share_exp,
            inds_exp_share_exp,
            deductible_expenses_share_exp
            ) ~ Corp|
        sic_3+metro_area_code+year,
        data = ., cluster = ~sic_3+year
    ) %>%
    etable()


colombia_data_frame %>%
    ungroup() %>%
    filter(
        sales>0
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
    ggplot(aes(x=log(sales), fill=Corp))+
    geom_histogram(aes(y=..density..), bins = 50)+
    theme_classic()
