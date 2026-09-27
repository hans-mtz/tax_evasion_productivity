## Industry-level counts and revenue shares for the Setting and Data chapter,
## shared by ch03-top-industries-table.R and ch03-corps-by-industry-table.R.
## Sourced after 001-setup.R; provides `ch3_ind`, one row per industry.
##
## Replaces (2026-09-26) top_10_revenue / top_20_inds_table from
## Code/Colombia/15_global_vars.R (legacy, loaded by many scripts, untouched):
## those used every juridical organization (codes 6-9 included), and the
## top-10 revenue shares were computed over industries with 100+ plants only.
## Here: ch3_base sample; revenue = real sales summed over 1981-1991; shares
## over all of manufacturing in the sample; plants counted once per industry,
## a plant counted as a corporation if it is one in any year.
source("Code/Thesis/ch03-sample.R")

ch3_ind <- ch3_base %>%
    mutate(sic_3 = as.character(sic_3)) %>%
    group_by(sic_3) %>%
    summarise(
        plants = n_distinct(plant),
        corp_plants = n_distinct(plant[corp]),
        revenue = sum(sales, na.rm = TRUE),
        corp_revenue = sum(sales[corp], na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        rev_share = 100 * revenue / sum(revenue),
        plant_share = 100 * plants / sum(plants),
        corp_plant_pct = 100 * corp_plants / plants,
        corp_rev_pct = 100 * corp_revenue / revenue
    ) %>%
    left_join(ch3_ind_names, by = "sic_3")
stopifnot(!anyNA(ch3_ind$name))
