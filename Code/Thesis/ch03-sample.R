## Working sample for the Setting and Data chapter, shared by every ch03-*
## data script (summary statistics, juridical organization, industries).
## Sourced after 001-setup.R; provides `ch3_base` and `ch3_ind_names`.
##
## Sample (PLAN.md §9a, option b, 2026-09-26): firm-years with finite log
## output, capital, labour and materials, and juridical organization codes
## 0-5. Codes 6-9 are dropped: 6 = stock partnerships (sociedades en comandita
## por acciones, taxed as corporations), 7-9 = cooperatives, state
## enterprises and other non-profit entities. Corporations = code 3 only;
## partnerships = codes 2, 4, 5 (general, de facto, ordinary limited). Same
## rule as the re-estimation inputs (1501-two-tax-first-stage.R, 1510, 1514).
## Industry 353 (petroleum refineries, 3 firm-years) is dropped too, so ch. 3
## describes the same 28 industries the first stage estimates (Hans, 2026-09-26).

load(file.path(PRODUCTS_DIR, "colombia_data.RData")) # colombia_data_frame

JO_LEVELS <- c("Proprietorship", "LLC", "Partnership", "Corporation")

ch3_base <- colombia_data_frame %>%
    ungroup() %>%
    filter(
        is.finite(y), is.finite(k), is.finite(l), is.finite(m),
        juridical_organization %in% 0:5,
        sic_3 != 353
    ) %>%
    mutate(
        jo = case_when(
            juridical_organization == 0 ~ "Proprietorship",
            juridical_organization == 1 ~ "LLC",
            juridical_organization %in% c(2, 4, 5) ~ "Partnership",
            juridical_organization == 3 ~ "Corporation"
        ),
        jo = factor(jo, levels = JO_LEVELS),
        corp = juridical_organization == 3
    )

## Short ISIC Rev. 2 names, same as ch04-evasion-test.R; the ciiu_3 descriptions
## are long, repeat "Food manufacturing" for 311 and 312, and carry a typo for 369.
ch3_ind_names <- tibble::tribble(
    ~sic_3, ~name,
    "311", "Food products", "312", "Other food products", "313", "Beverages",
    "314", "Tobacco", "321", "Textiles", "322", "Wearing apparel",
    "323", "Leather products", "324", "Footwear", "331", "Wood products",
    "332", "Furniture", "341", "Paper products", "342", "Printing and publishing",
    "351", "Industrial chemicals", "352", "Other chemicals", "353", "Petroleum refineries",
    "354", "Petroleum and coal products", "355", "Rubber products", "356", "Plastic products",
    "361", "Pottery and china", "362", "Glass products", "369", "Non-metallic minerals",
    "371", "Iron and steel", "372", "Non-ferrous metals", "381", "Metal products",
    "382", "Non-electrical machinery", "383", "Electrical machinery", "384", "Transport equipment",
    "385", "Professional equipment", "390", "Other manufacturing"
)
