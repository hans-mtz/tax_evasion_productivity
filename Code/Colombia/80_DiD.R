library(fixest)
library(tidyverse)

# Load data, global vars, and functions ---------
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/functions.RData")

# Setting a dictionary 
setFixest_dict(c(
    treatment = "Firms required to repor income tax in 1983", 
    time = "Years (1982,1983)",
    log_share = "Intermediates cost share of sales (logs)", 
    sic_3 = "Industry",
    metro_area_code = "Metropolitian area",
    `treat_fact1-15%` = "1-15%",
    `treat_fact20-50%` = "20-50%",
    `treat_fact55-75%` = "55-75%",
    `treat_fact80-95%` = "80-95%",
    year_fct84 = "Year of 1984",
    year_fct85 = "Year of 1985",
    year_fct86 = "Year of 1986",
    year_fct87 = "Year of 1987",
    year_fct88 = "Year of 1988",
    year_fct89 = "Year of 1989",
    year_fct90 = "Year of 1990"#,
))

# Change in threshold -------

colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(lag_log_sales_tax, 20),
        treatment = ifelse(
            nthile == 4,
            1, 0
        ),
        time = ifelse(
            year == 83,
            1, 0
        )
    ) %>%
    filter(
        year %in% c(81,82,83),
        nthile %in% c(2,4,20),
    ) %>%
    feols(
        log_share ~ treatment*time | csw(sic_3,metro_area_code),
        data = .,
        vcov = "hc1"
    ) %>%
    summary()

threshold_model <- colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(lag_log_sales_tax, 20),
        treatment = ifelse(
            nthile == 4,
            1, 0
        ),
        time = ifelse(
            year == 83,
            1, 0
        )
    ) %>%
    filter(
        year %in% c(82,83),
        nthile %in% c(2,4,20),
    ) %>%
    feols(
        log_share ~ treatment*time | csw(sic_3,metro_area_code),
        data = .,
        vcov = "hc1"
    ) #%>%
    # summary()

etable(threshold_model)

## 1983 fiscal reform

colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(lag_log_sales_tax, 20),
        treatment = case_when(
            nthile %in% 1:3 ~ "1-15%",
            nthile %in% 4:10 ~ "20-50%",
            nthile %in% 11:15 ~ "55-75%",
            nthile %in% 16:19 ~ "80-95%",
            .default = "Top 5%"
        ),
        treat_fact = relevel(
            as.factor(treatment),
            ref = "Top 5%"
        ),
        time = ifelse(
            year %in% c(84, 85),
            1, 0
        ),
        year_fct = factor(year)
    ) %>%
    filter(
        year %in% c(83, 84, 85, 86),
        # nthile %in% c(2,4,20),
    ) %>%
    mutate(
        year_fct = relevel(
            year_fct,
            ref = "83"
        )
    ) %>%
    feols(
        log_share ~ treat_fact*year_fct | csw(sic_3,metro_area_code),
        data = .,
        vcov = "hc1"
    ) %>%
    summary()

did_1983 <- colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(lag_log_sales_tax, 20),
        treatment = case_when(
            nthile %in% 1:3 ~ "1-15%",
            nthile %in% 4:10 ~ "20-50%",
            nthile %in% 11:15 ~ "55-75%",
            nthile %in% 16:19 ~ "80-95%",
            .default = "Top 5%"
        ),
        treat_fact = relevel(
            as.factor(treatment),
            ref = "Top 5%"
        ),
        time = ifelse(
            year %in% c(84, 85),
            1, 0
        ),
        year_fct = factor(year)
    ) %>%
    filter(
        year %in% c(83, 84, 85, 86),
        # nthile %in% c(2,4,20),
    ) %>%
    mutate(
        year_fct = relevel(
            year_fct,
            ref = "83"
        )
    ) %>%
    feols(
        log_share ~ treat_fact*year_fct | csw(sic_3,metro_area_code),
        data = .,
        vcov = "hc1"
    ) #%>%
    # summary()

etable(
    did_1983,
    keep = "x",
    drop = "84"
)

## 1986 fiscal reform

colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(lag_log_sales_tax, 20),
        treatment = case_when(
            nthile %in% 1:3 ~ "1-15%",
            nthile %in% 4:10 ~ "20-50%",
            nthile %in% 11:15 ~ "55-75%",
            nthile %in% 16:19 ~ "80-95%",
            .default = "Top 5%"
        ),
        treat_fact = relevel(
            as.factor(treatment),
            ref = "Top 5%"
        ),
        time = ifelse(
            year %in% c(84, 85),
            1, 0
        ),
        year_fct = factor(year)
    ) %>%
    filter(
        year %in% c(86, 87, 88, 89),
        # nthile %in% c(2,4,20),
    ) %>%
    mutate(
        year_fct = relevel(
            year_fct,
            ref = "86"
        )
    ) %>%
    feols(
        log_share ~ treat_fact*year_fct | csw(sic_3,metro_area_code),
        data = .,
        vcov = "hc1"
    ) %>%
    summary()

did_1986<-colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(lag_log_sales_tax, 20),
        treatment = case_when(
            nthile %in% 1:3 ~ "1-15%",
            nthile %in% 4:10 ~ "20-50%",
            nthile %in% 11:15 ~ "55-75%",
            nthile %in% 16:19 ~ "80-95%",
            .default = "Top 5%"
        ),
        treat_fact = relevel(
            as.factor(treatment),
            ref = "Top 5%"
        ),
        time = ifelse(
            year %in% c(84, 85),
            1, 0
        ),
        year_fct = factor(year)
    ) %>%
    filter(
        year %in% c(86, 87, 88, 89),
        # nthile %in% c(2,4,20),
    ) %>%
    mutate(
        year_fct = relevel(
            year_fct,
            ref = "86"
        )
    ) %>%
    feols(
        log_share ~ treat_fact*year_fct | csw(sic_3,metro_area_code),
        data = .,
        vcov = "hc1"
    )
    #%>%
    # summary()


etable(
    did_1986
)
# Saving results

save(
    threshold_model,
    did_1983,
    did_1986,
    file = "Results/Tables/Colombia/regression_tables.RData"
)

