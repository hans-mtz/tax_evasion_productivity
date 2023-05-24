library(tidyverse)
load("Code/Products/colombia_data.RData")

## Excercise name --------

ex_name <- "night"#"select"#"tax"#"win" #"subsidies"#"share"#  "exp-ratio"#"sale"#

## Relevant variables ----

size <- colombia_data_frame %>%
    ungroup() %>%
    select(
        # !c(plant, year, sic_3, p_gdp),
        # labor_employee_years, wages, capital, lag_sales, lag_taxes # original
        # lag_M, lag_K, capital, lag_sales #capital
        # lag_indirect_tax, lag_sales_tax, lag_consumption_tax, lag_imex_tax #tax
        # lag_share_exports, share_exports, general_expenditure, lag_gen_exp #exp
        # k, lag_k, lag_m, lag_log_sales, lag_log_gen_exp #not-win
        # lag_k, lag_m, lag_log_consumption_tax, lag_log_sales_tax #win
        # log_int_pym_lag_k, lag_log_sales_k, lag_log_sales_tax_sales #sales, 
        # lag_log_gen_exp_k, lag_log_ind_exp_k, lag_log_exp_k, age #exp-capital ratio
        # starts_with("share"),
        # lag_log_int_pym_l, lag_log_sales_l, lag_log_gen_exp_l,lag_log_ind_exp_l,lag_log_exp_l# labor
        # subsidies, subsidies_prod, subsidies_exp # sub
        age, lag_log_ind_exp_k, lag_log_sales_tax , lag_log_sales_k, lag_log_sales, lag_k, share_exports#select
        # lag_log_sales, lag_log_sales_tax, lag_log_sales_k, lag_log_sales_l
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

probs <- c(0.95, 0.95, 0.20, 0.2) #c(0.00, seq(0.80, 0.99, by = 0.01), 0.99)
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

color_palette <- c(
    "#4f2683", 
    "#221338", 
    "#72a98f", 
    "#5c80bc", 
    "#d90368"
)

color_year <- c(
    "82" = color_palette[[1]],#"chartreuse3",
    "83" = color_palette[[3]],#"blue",
    "84" = color_palette[[5]] #"red"#,
    # "86"= "green",
    # "87"= "orange"
)

color_year_2 <- c(
    # "83" = "blue",
    # "84" = "red"#,
    # "86"= "magenta",
    # "88"= "purple",
    # "87"= "darkturquoise",
    "86" = color_palette[[1]],#"chartreuse3",
    "87" = color_palette[[3]],#"blue",
    "88" = color_palette[[5]] #"red"#,
)

color_year_3 <- c(
    # "83" = "blue",
    # "84" = "red"#,
    # "90"= "green",
    # "91"= "orange",
    "89" = color_palette[[1]],#"chartreuse3",
    "90" = color_palette[[3]],#"blue",
    "91" = color_palette[[5]] #"red"#,
)

shape_year <- c(
    "82"= 19,
    "83"= 17,
    "84"= 15
)

shape_year_2 <- c(
    "86"= 19,
    "87"= 17,
    "88"= 15
)

shape_year_3 <- c(
    "89"= 19,
    "90"= 17,
    "91"= 15
)
# color_year_4 <- c(
#     "82" = "chartreuse3",
#     "86" = "blue",
#     "90" = "red"#,
#     # "86"= "green",
#     # "87"= "orange"
# )

# color_year_5 <- c(
#     # "83" = "blue",
#     # "84" = "red"#,
#     "82"= "magenta",
#     "86"= "purple",
#     "90"= "cyan"
# )

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
    lag_sales = "Sales last period",
    lag_indirect_tax = "Taxes last period",
    lag_M = "Intermediates last period",
    lag_K = "Capital last period",
    lag_consumption_tax = "Consumption tax last period",
    lag_imex_tax = "Import/Export taxes last period",
    lag_sales_tax = "Sales taxes last period",
    share_exports = "Share of sales exported",
    share_fem_owners = "Share of female owners",
    general_expenditure = "General expenditures",
    lag_gen_exp = "General expenditure last period",
    lag_share_exports = "Share of exports last period",
    k = "Log capital",
    lag_k = "Log capital last period",
    lag_m = "Log intermediates last period",
    lag_log_sales = "Log sales last period",
    lag_log_gen_exp = "Log general expenditures last period",
    lag_log_consumption_tax = "Log consumption tax last period",
    lag_log_sales_tax = "Log sales tax last period",
    share_fem_managers = "Share of female managers",
    share_imports = "Share of inputs imported",
    log_int_pym_lag_k = "Log interest payments current period/capital last period",
    lag_log_int_pym_k = "Log interest payments/capital last period", 
    lag_log_sales_k = "Log sales/capital last period", 
    lag_log_sales_tax_sales = "Log sales taxes/sales last period",
    age = "Age",
    lag_log_gen_exp_k = "Log general expenditures/capital last period", 
    lag_log_ind_exp_k = "Log industrial expenditures/capital last period", 
    lag_log_exp_k = "Log expenditures/capital last period",
    lag_log_int_pym_l = "Log interest payments/ labour last period",# = log(lag(interest_payments, order_by = year))-lag_l,
    lag_log_sales_l = "Log sales/labour last period",# = lag_log_sales-lag_l,
    lag_log_gen_exp_l = "Log general expenditures/labour last period",# = lag_log_gen_exp-lag_l,
    lag_log_ind_exp_l = "Log industrial expenditures/labout last period",# = log(lag(industrial_expenditure, order_by = year))-lag_l,
    lag_log_exp_l = "Log expenditures/labour last period",#,
    subsidies = "Total subsidies",
    subsidies_prod = "Production subsidies",
    subsidies_exp = "Export subsidies",
    log_share = "Log inputs'cost/sales",
    share = "Inputs'cost share of sales",
    share_sales_tax = "Sales taxes as a share of sales",
    log_sales = "Log sales",
    sales = "Sales"
    # intermediate_inputs = "Intermediates"
)

# unrelated_size <- results_df %>%
#     ungroup() %>%
#     select(Size, Structurally.related) %>%
#     filter(Structurally.related == "Not Related") %>%
#     unique() %>%
#     pull(Size)

# Saving global variables -----------
print("Saving global variables")
save(
    list = setdiff(ls(), "colombia_data_frame"),
    file = "Code/Products/global_vars.RData"
)
