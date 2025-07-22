##%% Load packages
library(tidyverse)
load("Code/Products/colombia_data.RData")

## %% Data Preparation ------------------------

intermediates<-c(
    "log_mats_share", "log_energy_share","log_fuels_share",
    "log_repair_maint_share","log_deductible_intermediates_share",
    "log_non_deductible_intermediates_share"
)

test_data <- colombia_data_frame %>%
    ungroup() %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m)
    ) %>%
    select(
        !all_of(intermediates)
    ) %>%
    mutate(
        log_mats_share = log(nom_mats/nom_gross_output),
        log_energy_share = log(nom_energy/nom_gross_output),
        log_fuels_share = log(nom_fuels/nom_gross_output),
        log_repair_maint_share = log(nom_repair_maint/nom_gross_output),
        log_deductible_intermediates_share = log(nom_deductible_intermediates/nom_gross_output),
        log_non_deductible_intermediates_share = log(nom_non_deductible_intermediates/nom_gross_output)
    ) %>%
    select(
        sic_3, plant, year, juridical_organization, 
        y, k, l, m, log_mats_share:log_non_deductible_intermediates_share,
        materials, energy, fuels, repair_maintenance, deductible_intermediates,
        share_imports, share_exports, share_imports_materials,
        share_sales_tax, sales_taxes, sales, log_share, intermediates,
        sales_tax_sales, sales_tax_purchases
    )

## %% Save Data ------------------------

save(
    test_data,
    file = "Code/Products/test_data.RData"      
)