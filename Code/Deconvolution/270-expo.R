## %% Load packages and data --------------------------------------------
library(tidyverse)
library(ggplot2)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
## %% Review distribution of exporters vs cal_V and s ----------------

colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        # sic_3 %in% top_20_inds$sic_3
        sic_3 == 331
    ) %>%
    mutate(
        Corp = ifelse(
            juridical_organization == 3,
            "Corp", "Other"
        )
    ) %>%
    filter(
        age < 40
    ) %>%
    ggplot(aes(x=age, y=exp(log_mats_share), group=Corp, col=Corp, fill=Corp)) +
    geom_point() +
    geom_smooth(method = "lm")

colombia_data_frame %>% 
        filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        # sic_3 %in% top_20_inds$sic_3
        # sic_3 %in% top_20_inds$sic_3
        sic_3 == 323
    ) %>% 
    mutate(Corp=ifelse(juridical_organization==3,"Corp","Other")) %>% 
    group_by(sic_4, Corp) %>% 
    summarise(n=length(unique(plant))) %>%
    group_by(Corp) %>%
    mutate(total= sum(n)) %>%
    group_by(Corp,sic_4) %>%
    mutate(
        share = sum(n)*100 / total
    ) %>%
    pivot_wider(
        id_cols = !total,
        names_from = Corp,
        values_from = c(n, share)
    )

colombia_data_frame %>% 
        filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        # sic_3 %in% top_20_inds$sic_3
        sic_3 %in% top_20_inds$sic_3
        # sic_3 == 323
    ) %>% 
    mutate(Corp=ifelse(juridical_organization==3,"Corp","Other")) %>% 
    group_by(sic_4, Corp) %>% 
    summarise(
        n=length(unique(plant)),
        obs = n(),
        # n_sic3 = max(n_sic3),
        sic_3 = first(sic_3)
    ) %>%
    group_by(Corp, sic_3) %>%
    mutate(
        n_sic3= sum(n),
        obs_sic3 = sum(obs)
    ) %>%
    group_by(Corp,sic_4) %>%
    mutate(
        share = round(sum(n)*100 / n_sic3,1),
        share_obs = round(sum(obs) * 100 / obs_sic3,1)
    ) %>%
    pivot_wider(
        # id_cols = !obs,
        names_from = Corp,
        values_from = c(n, obs, share, share_obs, n_sic3, obs_sic3)
    ) |> View()

colombia_data_frame %>% 
        filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        # sic_3 %in% top_20_inds$sic_3
        sic_3 %in% top_20_inds$sic_3
    ) %>% 
    mutate(
        Corp=ifelse(juridical_organization==3,"Corp","Other"),
        total_sales = sum(sales, na.rm=TRUE)
    ) %>% 
    group_by(sic_3) %>%
    mutate(
        n_sic3 = length(unique(plant)),
        obs_sic3 = n()
    ) %>%
    group_by(sic_4) %>%
    mutate(
        sales_sic4 = sum(sales, na.rm=TRUE),
        sales_mkt_share_sic4 = sum(sales, na.rm=TRUE) / total_sales * 100,
        # n_sic4 = length(unique(plant)),
        # obs_sic4 = n()
    ) %>%
    group_by(sic_4, Corp) %>% 
    summarise(
        N =length(unique(plant)),
        obs = n(),
        n_share_sic3 = N / max(n_sic3) *100,
        obs_share_sic3 = obs / max(obs_sic3) * 100,
        sales_mkt_share = sum(sales, na.rm=TRUE) / max(sales_sic4) * 100,
        sic4_mkt_share = max(sales_mkt_share_sic4),
    ) #%>%
    group_by(sic_4, Corp) %>%
    mutate(total= sum(n)) %>%
    group_by(Corp,sic_4) %>%
    mutate(
        share = sum(n)*100 / total
    )