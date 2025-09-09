## %% load libraries %%

library(tidyverse)

## %% Load data %%
data_folder <- "Data/Spain/"
load(paste0(data_folder, "spain-data.RData"))

## %% Data wrangling %%

df <- df %>%
    filter(
        !is.na(industry),
        # !is.na(sales),
        # !is.na(vpv)
    ) %>%
    group_by(firmid) %>%
    arrange(firmid, year) %>%
    mutate(
        vpv = ifelse(
            is.na(first(vpv)),
            0,
            vpv
        ),
        pv_perc = 1+vpv/100,
        sales_pv_cum = cumprod(pv_perc),
        price_index_output = 100*cumprod(pv_perc),
        r_sales = sales*(100/price_index_output),
    ) %>%
    ungroup()#%>%
    select(
        industry,
        firmid,
        year,
        vpv,
        sales,
        # sales_pv_cum,
        pi_output,
        price_index_output
    ) %>%
    ungroup() %>%
    filter(
        # is.na(pi_output),
        # !is.na(price_index_output),
        firmid == 432
    )
    summarise(
        pi_missing = sum(is.na(pi_output)),
        price_index_missing = sum(is.na(price_index_output)),
        N = n()
    )


#           14     14  2017   4    31730633        NA               160.
#  2       14     14  2018   4    31730633        NA               167.
#  3        7    152  2018   0     5556937        NA               100 
#  4       10    284  2015   3    52506492        NA               134.
#  5       10    284  2016   3    45753962        NA               138.
#  6       17    317  2016   0    73192437        NA               100 
#  7        7    378  2017   0   774340864        NA               100 
#  8        7    378  2018   0   553969255        NA               100 
#  9        5    432  2017   2.9   5661629        NA               141.
# 10        5    432  2018   2.9   5661629        NA               145.
## %% fun stuff

top_inds <-df %>% 
    mutate(
        total_market_sales = sum(routput, na.rm = TRUE),
    ) %>%
    group_by(industry) %>%
    summarise(
        market_share = 100*sum(routput, na.rm = TRUE)/max(total_market_sales),
        N = length(unique(firmid)),
        N_LTU = length(unique(firmid[routput > 6000]))
    ) %>%
    arrange(desc(market_share))


df %>% filter(
    sales < 10000000,
    industry == ) %>%
    ggplot(aes(x = sales)) +
    geom_histogram(bins = 200) +
    scale_x_log10() +
    labs(
        title = "Sales Distribution",
        x = "Sales (log scale)",
        y = "Frequency"
    )

hist(df$sales, breaks=200)

hist(df$routput[df$routput>1000000], na.rm = TRUE, breaks=100) 

df %>%
    mutate(
        LTU = ifelse(
            sales > 6000000 ,
            1,0
        ),
        sales_group = round(sales/1000000,0)
    ) %>%
    group_by(industry) %>%
    summarise(
        N = length(unique(firmid)),
        N_LTU = length(unique(firmid*LTU))-1,
        # .after = year
    )


df %>%
    mutate(
        LTU = ifelse(
            sales > 6000000 ,
            1,0
        ),
        sales_group = round(routput/1000,0)
    ) %>%
    filter(
        # sales < 20000000,
        # sales_group < 10,
        industry == 6
    ) %>%
    group_by(industry, sales_group) %>%
    summarise(
        mean_s = mean((pi_intermediate_goods*rintermediates_noRD)/ (pi_output*routput), na.rm = TRUE),
        mean_m = mean((pi_materials*rmaterials)/ (pi_output*routput), na.rm = TRUE),
    ) %>%
    ggplot(aes(x = sales_group, y = mean_m, color = factor(industry))) +
    geom_point()+
    theme_classic()

df %>%
    mutate(
        LTU = ifelse(
            sales > 6000000 ,
            1,0
        ),
        sales_group = round(sales/1000000,0),
        total_sales = pbs
    ) %>%
    filter(
        sales > 0,
        sales < 9000000,
        sales > 2000000,
        # industry == 6
    ) %>%
    # group_by(industry) %>%
    mutate(
        total_industry_sales = sum(pbs, na.rm = TRUE),
        sales_share = total_industry_sales/total_sales,
        services = coint - comp,
        i_share = (coint/sales),
        m_share = comp/sales,
        s_share = services/sales,
        tax_share = (dedton/sales)*100,
        exports_share = vexpor/sales,
        exporters = case_when(
            exports_share <= 0 ~ "0 Non-Exporter",
            exports_share < 0.1 ~ "1 Low Exporter",
            exports_share < 0.5 ~ "2 Medium Exporter",
            exports_share >= 0.5 ~ "3 High Exporter"
        )
    ) %>%
    group_by(industry, exporters) %>%
    # arrange(desc(sales_share)) %>%
    # # slice_max(sales_share, n=10) %>%
    # group_by(industry, LTU) %>%
    summarise(
        mean_i = mean((coint/sales), na.rm = TRUE),
        mean_m = mean((comp/sales), na.rm = TRUE),
        mean_s = mean((services/sales), na.rm = TRUE),
        N = length(unique(firmid)),
        # mean_tax = mean(dedton/sales, na.rm = TRUE)*100,
    ) |> View()#%>%
    # filter(
    #     mean_i < 2.5
    # ) %>%
    ggplot(aes(x=exports_share, y=m_share, color = factor(industry), fill=factor(industry))) +
    # geom_histogram(bins = 70)+
    geom_point()+
    theme_classic()

## %% Do firms that change LTU threshold change behavior?

# Identify firms that cross the LTU threshold

df %>%
    filter(
        !is.na(sales),
        sales > 0
    ) %>%
    mutate(
        LTU = ifelse(
            routput > 6000 ,
            1,0
        ),
        sales_group = round(sales/1000000,0),
        share_i = coint/sales,
        share_m = comp/sales,
        share_s = (coint - comp)/sales
    ) %>%
    group_by(firmid) %>%
    arrange(year) %>%
    mutate(
        LTU_lag1 = LTU - lag(LTU,1),
        LTU_lag2 = LTU - lag(LTU,2),
        LTU_lag3 = LTU - lag(LTU,3),
        LTU_lag4 = LTU - lag(LTU,4),
        LTU_lead1 = lead(LTU,1) - LTU,
        LTU_lead2 = lead(LTU,2) - LTU,
        LTU_lead3 = lead(LTU,3) - LTU,
        LTU_lead4 = lead(LTU,4) - LTU,
        LTU_change = case_when(
            LTU_lag4 > 0 ~ -4,
            LTU_lag3 > 0 ~ -3,
            LTU_lag2 > 0 ~ -2,
            LTU_lag1 > 0 ~ -1,
            LTU_lead1 == 0 ~ 1,
            LTU_lead2 == 0 ~ 2,
            LTU_lead3 == 0 ~ 3,
            LTU_lead4 == 0 ~ 4,
            # LTU_lag1 > 0 & LTU_lead1 > 0 ~ 0,
            .default = NA
        ),
        delta_si = share_i - lag(share_i,1),
        delta_sm = share_m - lag(share_m,1),
        delta_ss = share_s - lag(share_s,1)
        # ever_LTU = max(LTU, na.rm = TRUE),
        # ever_changed = max(abs(LTU_change), na.rm = TRUE)
    ) %>%
    group_by(industry,LTU_change) %>%
    summarise(
        beta_i = mean((coint/sales), na.rm = TRUE),
        beta_m = mean((comp/sales), na.rm = TRUE),
        beta_s = mean(((coint-comp)/sales), na.rm = TRUE),
        avg_delta_si = mean(delta_si, na.rm = TRUE),
        avg_delta_sm = mean(delta_sm, na.rm = TRUE),
        avg_delta_ss = mean(delta_ss, na.rm = TRUE),
        se_delta_si = sd(delta_si, na.rm = TRUE) / sqrt(n()),
        se_delta_sm = sd(delta_sm, na.rm = TRUE) / sqrt(n()),
        se_delta_ss = sd(delta_ss, na.rm = TRUE) / sqrt(n())
    ) %>%
    filter(
        # industry == 6
        # industry != 8
        industry == 17
    ) %>%
    ggplot(aes(x = LTU_change, y = avg_delta_si, color = factor(industry), fill=factor(industry))) +
    geom_point() +
    geom_errorbar(aes(ymin = avg_delta_si - 1.96 * se_delta_si,
                      ymax = avg_delta_si + 1.96 * se_delta_si),
                  width = 0.2)+
    geom_line() +
    theme_classic()

df_ltu


LTU_passing_firms <-df %>%
    filter(
        !is.na(sales),
        sales > 0
    ) %>%
    mutate(
        LTU = ifelse(
            r_sales > 6000000 ,
            1,0
        ),
        sales_group = round(sales/1000000,0),
        share_i = coint/sales,
        share_m = comp/sales,
        share_s = (coint - comp)/sales,
        industry = factor(industry, levels = top_inds$industry)
    ) %>%
    group_by(firmid) %>%
    arrange(year) %>%
    mutate(
        LTU_lag1 = LTU - lag(LTU,1),
        LTU_lag2 = LTU - lag(LTU,2),
        LTU_lag3 = LTU - lag(LTU,3),
        LTU_lag4 = LTU - lag(LTU,4),
        LTU_lead1 = lead(LTU,1) - LTU,
        LTU_lead2 = lead(LTU,2) - LTU,
        LTU_lead3 = lead(LTU,3) - LTU,
        LTU_lead4 = lead(LTU,4) - LTU,
        LTU_change = case_when(
            LTU_lag1 > 0 ~ -1,
            # LTU_lag1 > 0 & LTU_lag2 > 0 ~ -2,
            # LTU_lag1 > 0 & LTU_lag3 > 0 ~ -3,
            # LTU_lag1 > 0 & LTU_lag4 > 0 ~ -4,
            LTU_lag1 > 0 & LTU_lead1 == 0 ~ 1,
            # LTU_lag1 > 0 & LTU_lead2 == 0 ~ 2,
            # LTU_lag1 > 0 & LTU_lead3 == 0 ~ 3,
            # LTU_lag1 > 0 & LTU_lead4 == 0 ~ 4,
            # LTU_lag1 > 0 & LTU_lead1 > 0 ~ 0,
            .default = NA
        ),
        delta_si = share_i - lag(share_i,1),
        delta_sm = share_m - lag(share_m,1),
        delta_ss = share_s - lag(share_s,1),
        ever_LTU = max(LTU, na.rm = TRUE),
        ever_changed = LTU_lag1 > 0,
        year_change = ifelse(
            LTU_lag1 > 0,
            year,
            NA
        )
    ) %>%
    filter(
        ever_changed > 0,
        # !is.na(LTU_change),
        !is.na(year_change)
    ) %>%
    group_by(firmid) %>%
    summarise(
        firmid = first(firmid),
        industry = first(industry),
        year_change = first(year),
    )
    group_by(industry,LTU_change) %>%
    summarise(
        beta_i = mean((coint/sales), na.rm = TRUE),
        beta_m = mean((comp/sales), na.rm = TRUE),
        beta_s = mean(((coint-comp)/sales), na.rm = TRUE),
        n = n(),
        # avg_delta_si = mean(delta_si, na.rm = TRUE),
        # avg_delta_sm = mean(delta_sm, na.rm = TRUE),
        # avg_delta_ss = mean(delta_ss, na.rm = TRUE),
        # se_delta_si = sd(delta_si, na.rm = TRUE) / sqrt(n()),
        # se_delta_sm = sd(delta_sm, na.rm = TRUE) / sqrt(n()),
        # se_delta_ss = sd(delta_ss, na.rm = TRUE) / sqrt(n())
    ) %>%
    filter(
        industry %in% top_inds$industry[1:5]
    ) |> View()

df %>%
    filter(
        firmid %in% LTU_passing_firms$firmid,
    ) %>%
    mutate(
        industry = factor(industry, levels = top_inds$industry)
    ) %>%
    group_by(industry) %>%
    summarise(
        obs= n(),
        N = length(unique(firmid)),
    )

df %>%
    filter(
        firmid %in% LTU_passing_firms$firmid,
    ) %>%
    group_by(firmid) %>%
    arrange(industry, firmid, year) %>%
    mutate(
        LTU = ifelse(
            r_sales > 6000000 ,
            1,0
        ),
        LTU = ifelse(
            LTU > 0 | lag(LTU, default = 0) > 0,
            1, 0),
        sales_group = round(sales/1000000,0),
        share_i = coint/sales,
        share_m = comp/sales,
        share_s = (coint - comp)/sales,
        industry = factor(industry, levels = top_inds$industry),
        d_si = share_i - lag(share_i,1),
        d_sm = share_m - lag(share_m,1),
        d_ss = share_s - lag(share_s,1)
    ) %>%
    group_by(industry, LTU) %>%
    summarise(
        mean_i = mean(share_i, na.rm = TRUE),
        mean_m = mean(share_m, na.rm = TRUE),
        mean_s = mean(share_s, na.rm = TRUE),
        mean_d_si = mean(d_si, na.rm = TRUE),
        mean_d_sm = mean(d_sm, na.rm = TRUE),
        mean_d_ss = mean(d_ss, na.rm = TRUE),
        N = length(unique(firmid))
    ) |> View()
    select(
        industry,
        firmid,
        year,
        # sales,
        r_sales,
        share_i,
        share_m,
        LTU
    ) |> View()
