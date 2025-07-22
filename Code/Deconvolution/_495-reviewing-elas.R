## %% load data and packages --------------------
# rm(list=ls())
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
library(tidyverse)
library(foreign)

## %% Reviewing Elas --------------------------------

colombia_data_frame %>%
    filter(
        # is.finite(log_sales), # is.finite if it is not NA,NaN, Inf or -Inf
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(0.05),
        # !is.na(y),
        # !is.na(k),
        # !is.na(l),
        # !is.na(m)
        is.finite(log_mats_share),
        # is.finite(log(materials)),
        # is.finite(log_share)
    ) %>%
    group_by(sic_3) %>%
    summarise(
        across(
            c(gross_output, intermediates, capital, labor_employee_years,
            gnr_int_share, deductible_intermediates_share, deductible_intermediates,
            materials_share),
            ~ mean(.x, na.rm = TRUE),
            .names = "{.col}_mean"
        ),
        n = n()
    ) |> View()

## Success !!!


ols_1s<-colombia_data_frame %>%
    filter(
        is.finite(log_sales), # is.finite if it is not NA,NaN, Inf or -Inf
        is.finite(log(gross_output)),
        is.finite(log_mats_share),
        is.finite(log(materials)),
        is.finite(l),
        is.finite(k),
        is.finite(log_share)
    ) %>%
    filter(sic_3 == "311") %>%
    fixest::feols(
        log_mats_share ~ 1, data = .
    )

log_D <- coefficients(ols_1s)[[1]]
epsilon <- residuals(ols_1s)
big_E <- -epsilon |> exp() |> mean()
beta <- exp(log_D - log(big_E))
mean_epsilon <- mean(-epsilon)
variance_epsilon <- var(-epsilon)


colombia_data_frame %>%
    arrange(plant,year) %>%
    group_by(plant) |> View()


# Sum rows across columns ignoring NAs unless all are NAs
sum_rows <- function(...) {
    df <- cbind(...)
    apply(df, 1, function(row) {
        if (all(is.na(row))) {
        return(NA)
        } else {
        return(sum(row, na.rm = TRUE))
        }
    })
}

colombia_data_frame %>%
    arrange(plant,year) %>%
    group_by(plant,year) %>%
    mutate(
        sum = sum_rows(
            l,
            k,
            m,
            log_share,
            log_mats_share,
            log_sales
        )
    ) %>% select(sum)

colombia_data_frame %>%
    filter(
        # is.finite(log_sales), # is.finite if it is not NA,NaN, Inf or -Inf
        sic_3 == 311,
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m)
        # !is.na(y),
        # !is.na(k),
        # !is.na(l),
        # !is.na(m)
        # is.finite(log_mats_share),
        # is.finite(log(materials)),
        # is.finite(log_share)
    ) %>%
    lm(y~ k +l +m, data = .) |> summary()

## %% Compare with Stata Data ----------------


stata_data_folder <- '/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado/Replication_files/Tables_2_3/Colombia/Industry/'
stata_data <- read.dta(paste0(stata_data_folder, "data_col.dta"))

stata_data |> names()
stata_data |> head()
stata_data %>%
    mutate(
        sic_3 = str_extract(id, "^\\d{3}"),
    ) %>%
    filter(
        is.finite(logL),
        is.finite(logK),
        is.finite(logRGO)
    ) %>%
    group_by(sic_3) %>%
    summarise(
        across(
            c(logL, logK,logRGO),
            ~ mean(.x, na.rm = TRUE),
            .names = "{.col}_mean"
        ),
        n = n()
    ) |> View()


colombia_data_frame %>%
    ungroup() %>%
    filter(
        # sales > 0,
        # # !is.na(capital),
        # !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m)
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
        sic_rev_per_firm = sum(sales, na.rm = TRUE)/n_sic,
        # corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    filter( n_sic >= 100) %>%
    ungroup() %>%
    arrange(desc(sic_rev_per_firm)) #%>%

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



load("Code/Products/intermediates.RData")


back_env_ev_tbl<- colombia_data_frame %>%
    ungroup() %>%
    filter(
        # sales > 0,
        # # !is.na(capital),
        # !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m)
    ) %>%
    left_join(
        tax_ev_test_tbl,
        by = "sic_3",
        suffix = c("", "_tax")
    ) %>%
    mutate( 
        ev_mean_rate = as.numeric(str_extract(log_deductible_intermediates_share_tax, "^\\d+\\.\\d+")),
        corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        corp_num = ifelse(juridical_organization==3,1,0),
        market_value = sum(gross_output, na.rm = TRUE),
        # gross_output_tax_purchases_rate = meandeductible_intermediates/gross_output_tax_purchases,
        ded_int_spending_total = sum(deductible_intermediates, na.rm = TRUE),
        gov_rev_loss = 0.06*deductible_intermediates*(1-exp(-ev_mean_rate)),
        # gov_rev_loss_perc_pot = gov_rev_loss/rowSums(cbind(gross_output,-deductible_intermediates), na.rm = FALSE),
        gov_rev_loss_total = sum(gov_rev_loss, na.rm = TRUE)
        # n = unique(plant) |> length()
    ) %>%
    group_by(sic_3) %>%
    summarise(
        n_sic = unique(plant) |> length(),
        n_Corp = unique(plant*corp_num) |> length()-1,
        gross_output_tax_purchases_rate_sic = mean(sales_tax_purchases/deductible_intermediates, na.rm = TRUE),
        revenues_sic = sum(sales, na.rm = TRUE),
        # ded_int_spending_total = sum(deductible_intermediates, na.rm = TRUE),
        sic_market_share = revenues_sic/min(market_value),
        sic_spending_share = sum(deductible_intermediates, na.rm = TRUE)/min(ded_int_spending_total),
        sic_rev_per_firm = mean(sales, na.rm = TRUE)/n_sic,
        sic_spending_per_firm = mean(deductible_intermediates, na.rm = TRUE)/n_sic,
        gov_rev_loss_sic = sum(gov_rev_loss, na.rm = TRUE),
        gov_rev_loss_perc_pot_sic = gov_rev_loss_sic/(sum(gross_output,-deductible_intermediates, na.rm = TRUE)*0.06),
        sic_gov_rev_loss_share = gov_rev_loss_sic/min(gov_rev_loss_total),
            # corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    filter( n_sic >= 100) %>%
    ungroup() %>%
    arrange(desc(sic_gov_rev_loss_share)) %>%
    select(sic_3, n_sic, n_Corp, ends_with("share"), ends_with("per_firm"), ends_with("sic")) %>%
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

back_env_ev_tbl |> View()

top_5_ev_inds <- back_env_ev_tbl$sic_3[2:6] #322 is not in the top 10 rev inds
top_5_ev_inds

back_env_ev_tbl_all<-colombia_data_frame %>%
    ungroup() %>%
    filter(
        # sales > 0,
        # # !is.na(capital),
        # !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m)
    ) %>%
    left_join(
        tax_ev_test_tbl,
        by = "sic_3",
        suffix = c("", "_tax")
    ) %>%
    mutate( 
        ev_mean_rate = as.numeric(str_extract(log_deductible_intermediates_share_tax, "^\\d+\\.\\d+")),
        corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        corp_num = ifelse(juridical_organization==3,1,0),
        market_value = sum(gross_output, na.rm = TRUE),
        # sales_tax_purchases_rate = meandeductible_intermediates/sales_tax_purchases,
        ded_int_spending_total = sum(deductible_intermediates, na.rm = TRUE),
        gov_rev_loss = 0.06*deductible_intermediates*(1-exp(-ev_mean_rate)),
        # gov_rev_loss_perc_pot = gov_rev_loss/rowSums(cbind(gross_output,-deductible_intermediates), na.rm = FALSE),
        gov_rev_loss_total = sum(gov_rev_loss, na.rm = TRUE)
        # n = unique(plant) |> length()
    ) %>%
    summarise(
        n_sic = unique(plant) |> length(),
        n_Corp = unique(plant*corp_num) |> length()-1,
        sales_tax_purchases_rate_sic = mean(sales_tax_purchases/deductible_intermediates, na.rm = TRUE),
        revenues_sic = sum(gross_output, na.rm = TRUE),
        # ded_int_spending_total = sum(deductible_intermediates, na.rm = TRUE),
        sic_market_share = revenues_sic/min(market_value),
        sic_spending_share = sum(deductible_intermediates, na.rm = TRUE)/min(ded_int_spending_total),
        sic_rev_per_firm = mean(gross_output, na.rm = TRUE)/n_sic,
        sic_spending_per_firm = mean(deductible_intermediates, na.rm = TRUE)/n_sic,
        gov_rev_loss_sic = sum(gov_rev_loss, na.rm = TRUE),
        gov_rev_loss_perc_pot_sic = gov_rev_loss_sic/(sum(gross_output,-deductible_intermediates, na.rm = TRUE)*0.06),
        sic_gov_rev_loss_share = gov_rev_loss_sic/min(gov_rev_loss_total),
            # corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    filter( n_sic >= 100) %>%
    ungroup() %>%
    arrange(desc(sic_gov_rev_loss_share)) %>%
    select(n_sic, n_Corp, ends_with("share"), ends_with("per_firm"), ends_with("sic"))

back_env_ev_tbl_all |> View()
