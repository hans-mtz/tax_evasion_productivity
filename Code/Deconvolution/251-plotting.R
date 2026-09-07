## %% Packages and data ------------------------
library(tidyverse)
library(ggplot2)
load("Code/Products/i_elas.RData")


## %% Plot coefficient test vs different tax rates ------------------------

main_tbl %>%
    pivot_wider(
        values_from = c(corps,others,mean_V_log_mats_share),
        names_from = type
    ) |> View()
    mutate(
        diff = str_extract(mean_V_log_mats_share, "-*\\d+\\.\\d+") %>% as.numeric(),
        .after = mean_V_log_mats_share
    )

main_tbl %>%
    filter(
        type == "coeff",
        # sic_3 %in% setdiff(sic_3,c(311,312))
    ) %>%
    mutate(
        diff = str_extract(mean_V_log_mats_share, "-*\\d+\\.\\d+") %>% as.numeric(),
        .after = mean_V_log_mats_share
    ) %>%
    pivot_longer(
        cols = sales_sales_tax_rate:effective_sales_tax_rate_corrected,
        names_to = "tax_type",
        values_to = "rate"
    ) %>%
    arrange(
        rate
    ) %>%
    filter(
        # diff > 0.03
        # rate >0
        sic_3 %in% setdiff(sic_3,c(311,312))
    ) %>%
    ggplot(
        aes(
            y = diff,
            x = rate,
            color = tax_type
            )
    ) +
    geom_point()+
    geom_smooth(method = "lm", se = FALSE)+
    # geom_line()+
    theme_minimal()+
    scale_color_manual(
        values = c(
            "sales_sales_tax_rate" = "lightgray",
            "purchases_sales_tax_rate" = "red",
            "effective_sales_tax_rate" = "#45c1eb",
            "effective_sales_tax_rate_corrected" = "#48e8e8",
            "tau_1_beta" = "violet",
            "pur_sales_tax_share_sales" = "orange"
        ),
        labels = c(
            "sales_sales_tax_rate" = "Sales Tax Rate on Sales",
            "purchases_sales_tax_rate" = "Sales Tax Rate on Purchases",
            "effective_sales_tax_rate" = "Effective Sales Tax Rate",
            "effective_sales_tax_rate_corrected" = "Corrected Effective Sales Tax Rate",
            "tau_1_beta" = expression(tau[1]*beta),
            # "tau_1_beta" = "$\\tau_1 \\beta$",
            "pur_sales_tax_share_sales" = "Purchases Sales Tax Share of Sales"
        )
    ) + # remove legend title
    labs(color = "Tax Type")+ # place legend on top
    theme(legend.position = "top") +
    xlab("Tax Rate (%)")+
    ylab("Difference in Coefficient Test")

## %% Save plot ------------------------

ggsave(
    filename = "Paper/images/graphs/coef_test_vs_tax_rates.png",
    width = 8,
    height = 6
)

## %% Adding SE to plot ------------------------

tst_tax_se_plot <- elas_tst_tbl %>%
    pivot_wider(
        names_from = type,
        values_from = corps:mean_V
    ) %>%
    mutate(
        mean_V = str_extract(mean_V_coeff, "-*\\d+\\.\\d+") |> as.numeric(),
        LCI = str_extract(mean_V_CI, "-*\\d+.\\d+")|> as.numeric(),
        UCI = str_extract(mean_V_CI, ", (-*\\d+.\\d+)]$", group = 1)|> as.numeric(),
        .after = mean_V_CI
    ) %>%
    left_join(
        inds_char_tbl
    ) %>%
    mutate(
        tau_1_beta = round(purchases_sales_tax_rate*as.numeric(corps_coeff),1),
        .after = pur_sales_tax_share_sales
    ) %>%
    mutate(
        effective_sales_tax_rate_corrected = round((sales_sales_tax_rate-tau_1_beta),1),
        .after = effective_sales_tax_rate
    ) %>%
    pivot_longer(
        cols = sales_sales_tax_rate:effective_sales_tax_rate_corrected,
        names_to = "tax_type",
        values_to = "rate"
    ) %>%
    arrange(
        rate
    ) %>%
    filter(
        # diff > 0.03
        # rate >0
        sic_3 %in% setdiff(sic_3,c(311,312))
    ) %>%
    ggplot(
        aes(
            y = mean_V,
            x = rate,
            color = tax_type
            )
    ) +
    geom_point()+
    geom_smooth(method = "lm", se = FALSE)+
    # add error bars to points using LCI and UCI
    geom_errorbar(
        aes(
            ymin = LCI,
            ymax = UCI
        ),
        width = 0.2,
        alpha = 0.5
    ) +
    # geom_line()+
    theme_minimal()+
    labs(color = "Tax Type")+ # place legend on top
    theme(legend.position = "top") +
    xlab("Tax Rate (%)")+
    ylab("Difference in Coefficient Test")


# All but sales tax rate
tst_tax_se_plot +
    scale_color_manual(
        values = c(
            "sales_sales_tax_rate" = "lightgray",
            "purchases_sales_tax_rate" = "red",
            "effective_sales_tax_rate" = "#45c1eb",
            "effective_sales_tax_rate_corrected" = "#48e8e8",
            "tau_1_beta" = "violet",
            "pur_sales_tax_share_sales" = "orange"
        ),
        labels = c(
            "sales_sales_tax_rate" = "Sales Tax Rate on Sales",
            "purchases_sales_tax_rate" = "Sales Tax Rate on Purchases",
            "effective_sales_tax_rate" = "Effective Sales Tax Rate",
            "effective_sales_tax_rate_corrected" = "Corrected Effective Sales Tax Rate",
            "tau_1_beta" = expression(tau[1]*beta),
            "pur_sales_tax_share_sales" = "Purchases Sales Tax Share of Sales"
        )
    ) 
ggsave(
    filename = "Paper/images/graphs/251-plot_all_but_sales.png",
    width = 8,
    height = 6
)
# Puechase sales tax rate only

tst_tax_se_plot +
    scale_color_manual(
        values = c(
            "sales_sales_tax_rate" = "lightgray",
            "purchases_sales_tax_rate" = "red",
            "effective_sales_tax_rate" = "lightgray",
            "effective_sales_tax_rate_corrected" = "lightgray",
            "tau_1_beta" = "lightgray",
            "pur_sales_tax_share_sales" = "lightgray"
        ),
        labels = c(
            "sales_sales_tax_rate" = "Sales Tax Rate on Sales",
            "purchases_sales_tax_rate" = "Sales Tax Rate on Purchases",
            "effective_sales_tax_rate" = "Effective Sales Tax Rate",
            "effective_sales_tax_rate_corrected" = "Corrected Effective Sales Tax Rate",
            "tau_1_beta" = expression(tau[1]*beta),
            "pur_sales_tax_share_sales" = "Purchases Sales Tax Share of Sales"
        )
    ) 
ggsave(
    filename = "Paper/images/graphs/251-purchases.png",
    width = 8,
    height = 6
)
# Purchase sales taxes share of sales
tst_tax_se_plot +
    scale_color_manual(
        values = c(
            "sales_sales_tax_rate" = "lightgray",
            "purchases_sales_tax_rate" = "lightgray",
            "effective_sales_tax_rate" = "lightgray",
            "effective_sales_tax_rate_corrected" = "lightgray",
            "tau_1_beta" = "violet",
            "pur_sales_tax_share_sales" = "orange"
        ),
        labels = c(
            "sales_sales_tax_rate" = "Sales Tax Rate on Sales",
            "purchases_sales_tax_rate" = "Sales Tax Rate on Purchases",
            "effective_sales_tax_rate" = "Effective Sales Tax Rate",
            "effective_sales_tax_rate_corrected" = "Corrected Effective Sales Tax Rate",
            "tau_1_beta" = expression(tau[1]*beta),
            "pur_sales_tax_share_sales" = "Purchases Sales Tax Share of Sales"
        )
    ) 
ggsave(
    filename = "Paper/images/graphs/251-pur-share-sales.png",
    width = 8,
    height = 6
)
# Effective sales tax rate
tst_tax_se_plot +
    scale_color_manual(
        values = c(
            "sales_sales_tax_rate" = "lightgray",
            "purchases_sales_tax_rate" = "lightgray",
            "effective_sales_tax_rate" = "#45c1eb",
            "effective_sales_tax_rate_corrected" = "#48e8e8",
            "tau_1_beta" = "lightgray",
            "pur_sales_tax_share_sales" = "lightgray"
        ),
        labels = c(
            "sales_sales_tax_rate" = "Sales Tax Rate on Sales",
            "purchases_sales_tax_rate" = "Sales Tax Rate on Purchases",
            "effective_sales_tax_rate" = "Effective Sales Tax Rate",
            "effective_sales_tax_rate_corrected" = "Corrected Effective Sales Tax Rate",
            "tau_1_beta" = expression(tau[1]*beta),
            "pur_sales_tax_share_sales" = "Purchases Sales Tax Share of Sales"
        )
    )
ggsave(
    filename = "Paper/images/graphs/251-effective.png",
    width = 8,
    height = 6
)
# Sales tax rate on sales
# I observe that as the sales tax rate on sales increases, the lower the tax evasion
# through overreporting of materials. This is counterintuive.
# The higher the sales tax rate on sales, the higher the incentives to evade.
# Does this suggests that sales might be underrereported? 

tst_tax_se_plot +
    scale_color_manual(
        values = c(
            "sales_sales_tax_rate" = "blue",
            "purchases_sales_tax_rate" = "lightgray",
            "effective_sales_tax_rate" = "lightgray",
            "effective_sales_tax_rate_corrected" = "lightgray",
            "tau_1_beta" = "lightgray",
            "pur_sales_tax_share_sales" = "lightgray"
        ),
        labels = c(
            "sales_sales_tax_rate" = "Sales Tax Rate on Sales",
            "purchases_sales_tax_rate" = "Sales Tax Rate on Purchases",
            "effective_sales_tax_rate" = "Effective Sales Tax Rate",
            "effective_sales_tax_rate_corrected" = "Corrected Effective Sales Tax Rate",
            "tau_1_beta" = expression(tau[1]*beta),
            "pur_sales_tax_share_sales" = "Purchases Sales Tax Share of Sales"
        )
    )
ggsave(
    filename = "Paper/images/graphs/251-sales.png",
    width = 8,
    height = 6
)
