# Do firms change JO in the data?
# 2% of firms per year; Across industries over time, 2-3%

colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    group_by(sic_3) %>%
    summarise(
        JO_num_mean = mean(JO_num),
        JO_changes_mean = mean(JO_change, na.rm=TRUE)
    )

colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    group_by(year) %>%
    summarise(
        JO_num_mean = mean(JO_num),
        JO_changes_mean = mean(JO_change, na.rm=TRUE)
    )

colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    xtabs(JO_change~year, data=.) %>%
    barplot()
