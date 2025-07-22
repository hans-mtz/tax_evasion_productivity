## Load libraries and data ----------------

library(tidyverse)
load("Code/Products/stata_colombia_df.RData")

## Data wrangling ----------------------------

colombia_data_frame %>%
    mutate(
        log_energy_share = log(energy/go)
    )
    select(
        plant,
        year,
        sic_3=sic,
        log_mats_share = lmats_share,
        log_energy_share = lenergy_share,

    )