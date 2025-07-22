## %% load libraries and data ----------------
library(tidyverse)
# load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")


## %% Load beta coefficients ----------------

load("Code/Products/i_elas.RData")

# elas<-elas_tst_tbl %>%
#     rename(
#         Corp = corps,
#         Other = others
#     ) %>%
#     # select(-mean_V) %>%
#     pivot_longer(
#         cols = !c(sic_3, type, mean_V),
#         names_to = 'Corp',
#         values_to = 'value'
#     ) %>%
#     pivot_wider(
#         names_from = type,
#         values_from = c(value, mean_V)
#     )

elas <- elas_tst_tbl %>%
    rename(
        Corp = corps,
        Other = others
    ) %>%
    # select(-mean_V) %>%
    pivot_longer(
        cols = !c(sic_3, type, mean_V),
        names_to = 'Corp',
        values_to = 'value'
    ) %>%
    pivot_wider(
        names_from = c(Corp, type),
        values_from = c(value, mean_V)
    ) %>%
    select(
        !ends_with("_CI"), -mean_V_Other_coeff,
        mean_V = mean_V_Corp_coeff
    )

## %% Sales Tax from sales and purchases ----------------

test_data %>%
    ungroup() %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        sic_3 %in% elas$sic_3
    ) %>%
    mutate(
        Corp = factor(ifelse(juridical_organization==3,"Corp","Other")),
        reform = factor(ifelse(year < 1984, "Pre", "Post"))
    ) %>%
    left_join(
        elas,
        by = c("sic_3")
    ) %>%
    group_by(sic_3, Corp) %>%
    summarise(
        sales_tax_sales_rate = mean(sales_tax_sales, na.rm = TRUE),
        sales_tax_purchases_rate = mean(sales_tax_purchases, na.rm = TRUE),
        sales_tax_purch_share_Bc = mean(sales_tax_purchases*as.numeric(value_Corp_coeff), na.rm = TRUE),
        sales_tax_purch_share_Bo = mean(sales_tax_purchases*as.numeric(value_Other_coeff), na.rm = TRUE),
        # sales_tax_share = mean(share_sales_tax, na.rm = TRUE),
        # effective_sales_tax_rate_true = mean(sales_tax_sales - sales_tax_purchases*as.numeric(value_Corp_coeff), na.rm = TRUE),
        # effective_sales_tax_rate = mean(sales_tax_sales - sales_tax_purchases*as.numeric(value_Other_coeff), na.rm = TRUE),
    ) %>%
    # pivot_longer(
    #     cols = !c(sic_3, Corp),
    #     names_to = "sales_tax_type",
    #     values_to = "value"
    # ) #%>%
    pivot_wider(
        names_from = Corp,
        values_from = starts_with("sales_tax_"),
        names_glue = "{Corp}_{.value}"
    ) %>%#|> View()
    mutate(
        Corp_effective_sales_tax_rate = Corp_sales_tax_sales_rate - Corp_sales_tax_purch_share_Bc,
        Other_effective_sales_tax_rate_reported = Other_sales_tax_sales_rate - Other_sales_tax_purch_share_Bo,
        Other_effective_sales_tax_rate = Other_sales_tax_sales_rate - Corp_sales_tax_purch_share_Bc,
        .after = sic_3
    ) |> View()


