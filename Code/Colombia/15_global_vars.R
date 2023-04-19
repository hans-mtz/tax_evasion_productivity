library(tidyverse)
load("Code/Products/colombia_data.RData")

## Relevant variables ----

size <- colombia_data_frame %>%
    ungroup() %>%
    select(
        # !c(plant, year, sic_3, p_gdp),
        labor_employee_years, wages, capital, lag_sales, lag_taxes
    ) %>%
    names()

output <- c(
    "gross_output", # real value of gross production
    "sales" # real sales
    # "va" #value added
)
input <- c(
    "materials",
    "intermediate_inputs",
    "mats_serv"
)

# Colombian big industries ----

industries <- colombia_data_frame %>%
    group_by(sic_3) %>%
    summarise(n_sic = n()) %>%
    arrange(desc(n_sic)) %>%
    mutate(
        n_cum = cumsum(n_sic),
        perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
    ) %>%
    select(sic_3, n_sic, perc, perc_acc) %>%
    unique()

big_industries <- industries %>%
    filter(n_sic >= 3500) %>%
    select(sic_3) %>%
    pull()

# Quantiles ---------------------------------

probs <- c(0.00, seq(0.80, 0.99, by = 0.01), 0.99)
probs_up <- seq(0.80, 0.99, by = 0.01)
probs_down <- seq(0.01, 0.20, by = 0.01)

# Saving results folder -----

products_dir <- "Code/Products/"

# Plotting vars ----------

my_colors <- c(
    purple = "#4F2683", # "purple",
    green = "#26837D",
    dark_purple = "#281342",
    gray = "#807F83", # "gray"
    darker_purple = "#1E0E31"
)

fig_dir <- "Results/Figures/Colombia/"

industry_description <- c(
    "Food Products",
    "Clothing",
    "Metal products",
    "Textiles",
    "Printers"
)
names(industry_description) <- big_industries

size_desc <- c(
    wages = "Labour: Wages",
    labor_employee_years = "Labour: Employee x Years",
    capital = "Capital",
    lag_sales = "Revenue last period",
    lag_taxes = "Taxes last period"
)

# unrelated_size <- results_df %>%
#     ungroup() %>%
#     select(Size, Structurally.related) %>%
#     filter(Structurally.related == "Not Related") %>%
#     unique() %>%
#     pull(Size)

# Saving global variables -----------

save(
    # input,
    # output,
    # size,
    # industries,
    # big_industries,
    # probs,
    # probs_up,
    # probs_down,
    # my_colors,
    # fig_folder,
    # industry_description,
    # size_desc,
    list = setdiff(ls(), "colombia_data_frame"),
    file = "Code/Products/global_vars.RData"
)
