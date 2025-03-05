top_20_inds_table <- colombia_data_frame %>%
    ungroup() %>%
    filter(
        sales > 0,
        # !is.na(capital),
        !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
    ) %>%
    mutate( 
        corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        market_value = sum(sales, na.rm = TRUE),
    ) %>%
    group_by(sic_3,corp) %>%
    summarise(
        n_sic = unique(plant) |> length(),
        revenues_sic = sum(sales, na.rm = TRUE),
        # corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    group_by(sic_3) %>%
    mutate(
        n_cum = cumsum(n_sic),
        n_perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
        market_share = revenues_sic/ sum(revenues_sic)*100,
        tmvs = revenues_sic/market_share*100
        # sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        # corps_share = corps_n/n_sic * 100
    ) %>%
    pivot_wider(
        id_cols = sic_3,
        names_from = corp,
        values_from = c(n_cum,n_sic,n_perc,perc_acc,market_share)
    ) %>%
    select(
        N =`n_cum_Non-Corp`,
        `Corps. (N)` = n_sic_Corp,
        `Corps. (%)` = n_perc_Corp,
        `Market Share (Corps)` = market_share_Corp
    ) %>% arrange(desc(N)) %>%
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


colombia_data_frame %>%
    ungroup() %>%
    filter(
        sales > 0,
        # !is.na(capital),
        !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
    ) %>%
    mutate( 
        corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        corp_num = ifelse(juridical_organization==3,1,0),
        market_value = sum(sales, na.rm = TRUE),
        n = unique(plant) |> length()
    ) %>%
    group_by(sic_3) %>%
    summarise(
        n_sic = unique(plant) |> length(),
        n_Corp = unique(plant*corp_num) |> length()-1,
        revenues_sic = sum(sales, na.rm = TRUE),
        # corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    filter( n_sic >= 100) %>%
    ungroup() %>%
    arrange(desc(revenues_sic)) %>%
    mutate(
        `Market Share` = revenues_sic/sum(revenues_sic)*100,
        CumSum = cumsum(revenues_sic),
        `Cum. Mkt Share` = CumSum/sum(revenues_sic)*100,
        `N Share` = n_sic/sum(n_sic)*100,
        CumSumN = cumsum(n_sic),
        `Cum. N Share` = CumSumN/sum(n_sic)*100
    ) %>%
    select(
        -starts_with("CumSum"), -revenues_sic
    ) %>%
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
