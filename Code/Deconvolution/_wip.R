## Packages adn Data --------------------------
library(tidyverse)
load("Code/Products/intermediates.RData")
load("Code/Products/global_vars.RData")

##

inds_sales_tax <- tribble(
    ~ sic_3, ~ Category,
    311, "Exempt Product",
    312, "Exempt Product",
    382, "Exempt Product",
    383, "Exempt Product",
    384, "Exempt Product",
    369, "Exempt Material",
    323, "Exempt Material",
    356, "Imported Material"
)

tax_ev_test_tbl %>%
    # left_join(
    #     top_20_inds
    # ) %>%
    left_join(
        inds_sales_tax
    ) %>%
    arrange(desc(Category),desc(log_mats_share)) %>%
    select(
        Category,
        sic_3,
        # description,
        log_mats_share:log_inds_nded_share
    )

tax_ev_test_tbl_gnr %>%
    # left_join(
    #     top_20_inds
    # ) %>%
    left_join(
        inds_sales_tax
    ) %>%
    arrange(desc(Category),desc(log_mats_share)) %>%
    select(
        Category,
        sic_3,
        # description,
        log_share:log_services_share
    )



       mu.mu sigma.sigma   mean.mu sd.mu   mode.mu median.mu convergence
1  0.2695661   0.1604330

mu<-0.2695661
sigma<-0.1604330
alpha <- -mu/sigma
Z = 1 - pnorm(0,mean=mu, sd=sigma)
num = dnorm(0,mean=mu,sd=sigma)
mean_trcnorm = mu+sigma*num/Z
num_median = pnorm(0,mean=mu,sd=sigma)+1
median_trcnorm = mu+qnorm(num_median/2, mean=mu, sd=sigma)*sigma
variance_trcnorm = sigma*sigma*(1+alpha*num/Z-(num/Z)^2)

1-pnorm(0,0.2695661,0.1604330)

##

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

colombia_data_frame %>%
    filter(
        sic_3 %in% top_20_inds_table$sic_3[1:20],
        is.finite(materials),
        materials >0
    ) %>%
    group_by(sic_3) %>%
    summarise(
        `Sale Tax` = mean(share_sales_tax, na.rm=TRUE),
        `Sale Tax Purchase` = mean(sales_tax_purchases/materials, na.rm=TRUE),
        `Material Imports` = mean(share_imports_materials, na.rm=TRUE),
        Exports = mean(share_exports, na.rm=TRUE),
        Materials = mean(materials_share, na.rm=TRUE),
        Electricity = mean(energy_share, na.rm=TRUE),
        Fuels = mean(fuels_share,na.rm=TRUE),
        `R&M` = mean(repair_maint_share, na.rm=TRUE),
    ) %>%
    arrange(
        desc(`Sale Tax`),
        desc(`Material Imports`),
        desc(Materials)
    )


## prod fun estimates -----------------

load("Code/Products/deconv_prod_fun.RData")
