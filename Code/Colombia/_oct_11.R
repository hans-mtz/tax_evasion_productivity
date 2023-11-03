library(tidyverse)
library(ggplot2)
library(modeest)
library(fixest)


load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
# load("Code/Products/functions.RData")

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
        log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="9")*factor(fiscal_period)| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable()

colombia_data_frame %>%
    filter(
        year <= 84
    ) %>%
    feols(
        log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="9")*relevel(factor(year), ref="82") +share_sales_tax| factor(sic_3)+factor(metro_area_code),
        data = .
    ) %>%
    etable()


colombia_data_frame %>%
    # filter(
    #     year <= 83
    # ) %>%
    feols(
        log(industrial_expenditure/sales) ~ polym(m, k, l,log(industrial_expenditure), degree = 2, raw = TRUE)+factor(juridical_organization) +share_sales_tax| csw0(sic_3,metro_area_code,year),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable()

colombia_data_frame %>%
    # filter(
    #     year <= 83
    # ) %>%
    feols(
        log(energy/sales) ~ polym(m, k, l,log(energy), degree = 2, raw = TRUE)+factor(juridical_organization) +share_sales_tax| csw0(sic_3,metro_area_code,year),
        data = ., cluster = ~sic_3+year
    ) %>%
    etable()

# Evidence of bumping at the individual income tax treshholds
# for jur org 0 and 1, but not for the rest 2-9
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

## 1983 Reform 
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
        names_from = year,
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
    select(contains(c("sic","82","83","84","85"))) %>%
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

# Winners
feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(year), ref="81")*relevel(treat, ref="increased")+share_sales_tax|sw0(sic_3,year,metro_area_code, juridical_organization),
        data = cdf, cluster=~sic_3+year
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
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+share_sales_tax+relevel(factor(juridical_organization),ref="9")*factor(year)|csw0(sic_3,year,metro_area_code),
        data = cdf, cluster=~sic_3+year
    ) |> etable()

feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+share_sales_tax+relevel(factor(juridical_organization),ref="9")*factor(year)|csw0(sic_3,year,metro_area_code),
        data = colombia_data_frame, cluster=~sic_3+year
    ) |> etable()

feols(
        log_share ~  polym(m, k, l, degree = 2, raw = TRUE)+share_sales_tax+relevel(factor(juridical_organization),ref="9")*factor(year)|csw0(sic_3,year,metro_area_code),
        data = colombia_data_frame, cluster=~sic_3+year
    ) -> reg_tbl_tax_jo

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
        log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="9")*factor(fiscal_period)| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
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
        )
    ) %>%
    # filter(
    #     year <= 86
    # ) %>%
    # select(fiscal_period, year)
    feols(
        log_share ~ polym(m, k, l, degree = 2, raw = TRUE)+relevel(factor(juridical_organization), ref="9")*factor(fiscal_period)+share_sales_tax| csw0(sic_3,metro_area_code,year),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable()
