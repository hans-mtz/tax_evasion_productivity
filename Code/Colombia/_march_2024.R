## Load packages and data ####
library(tidyverse)
library(ggplot2)
library(modeest)
library(fixest)


load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
# load("Code/Products/functions.RData")

## Do firms change JO in the data? ####
# 2% of firms per year; Across industries over time, 2-3%

changes_inds<-colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    group_by(sic_3) %>%
    summarise(
        # JO_num_mean = mean(JO_num),
        JO_changes_mean = mean(JO_change, na.rm=TRUE)
    )

changes_year<-colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    group_by(year) %>%
    summarise(
        # JO_num_mean = mean(JO_num),
        JO_changes_mean = mean(JO_change, na.rm=TRUE)
    )

changes_year_main_inds<-colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    group_by(sic_3,year) %>%
    summarise(
        JO_change_mean = 100*sum(JO_change, na.rm=TRUE)/n()#mean(JO_change, na.rm=TRUE)
    ) %>%
    filter(sic_3 %in% big_industries) %>%
    xtabs(JO_change_mean~year+sic_3, data=.)

changes_tbl <- rbind(changes_year_main_inds[,1:5], `Industry average`=changes_inds[changes_inds$sic_3 %in% big_industries,][[2]]*100)
changes_tbl<- cbind(changes_tbl, `Annual average`=c(changes_year[[2]]*100,100*mean(changes_year[[2]], na.rm=TRUE)))
changes_tbl[2:12,]
## something something -----
colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1),
        num = 1
    ) %>%
    xtabs(JO_change~sic_3+year, data=.)
    # xtabs(num~sic_3+year, data=.)

