library(tidyverse)
load("Code/Products/colombia_data.RData")

## Excercise name --------

ex_name <- "night"#"select"#"tax"#"win" #"subsidies"#"share"#  "exp-ratio"#"sale"#

## Relevant variables ----

# size <- colombia_data_frame %>%
#     ungroup() %>%
#     select(
#         # !c(plant, year, sic_3, p_gdp),
#         # labor_employee_years, wages, capital, lag_sales, lag_taxes # original
#         # lag_M, lag_K, capital, lag_sales #capital
#         # lag_indirect_tax, lag_sales_tax, lag_consumption_tax, lag_imex_tax #tax
#         # lag_share_exports, share_exports, general_expenditure, lag_gen_exp #exp
#         # k, lag_k, lag_m, lag_log_sales, lag_log_gen_exp #not-win
#         # lag_k, lag_m, lag_log_consumption_tax, lag_log_sales_tax #win
#         # log_int_pym_lag_k, lag_log_sales_k, lag_log_sales_tax_sales #sales, 
#         # lag_log_gen_exp_k, lag_log_ind_exp_k, lag_log_exp_k, age #exp-capital ratio
#         # starts_with("share"),
#         # lag_log_int_pym_l, lag_log_sales_l, lag_log_gen_exp_l,lag_log_ind_exp_l,lag_log_exp_l# labor
#         # subsidies, subsidies_prod, subsidies_exp # sub
#         age, lag_log_ind_exp_k, lag_log_sales_tax , lag_log_sales_k, lag_log_sales, lag_k, share_exports#select
#         # lag_log_sales, lag_log_sales_tax, lag_log_sales_k, lag_log_sales_l
#     ) %>%
#     names()

# output <- c(
#     "gross_output", # real value of gross production
#     "sales" # real sales
#     # "va" #value added
# )
# input <- c(
#     "materials",
#     "intermediate_inputs",
#     "mats_serv"
# )

# Colombian big industries ----

industries <- colombia_data_frame %>%
    filter(
        sales > 0,
        # !is.na(capital),
        !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
    ) %>%
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

labels <- c(
    # 'capital' = 'Capital',
    # 'mats_serv' = 'Materials & Services',
    # 'skilled_labor' = 'Labor (Skilled)',
    # 'unskilled_labor' = 'Labor (Unskilled)',
    # 'sales' = 'Revenue',
    # 'sales_taxes' = 'Sales Taxes',
    # 'JO_class' = 'J. Org.',
    'skilled_wage_bill_share' = 'Skilled Labor (Wages)',
    'unskilled_wage_bill_share' = 'Unskilled Labor (Wages)',
    'share_sales_tax' = 'Sales Taxes',
    'gnr_int_share' = 'GNR (M+E+S)',
    # 'mats_serv_share' = 'Materials + Services',
    # 'mats_deduct_share' = 'Materials + Deductible T.S.',
    'materials_share' = 'Materials (M)',
    'energy_share' = 'Electricity (E)',
    'capital_share' = 'Capital',
    'fuels_share' = 'Fuels (F)',
    'services_share' = 'Services (S)',
    'repair_maint_share' = 'Repair & Maintenance'#,
    # 'gnr_ded_share' = 'Deductible GNR',
    # 'lp_share' = 'LP (M+E+F)',
    # 'lp_ded_share' = 'Deductible LP',
    # 'inds_int_share' = 'EAM (M+E+I.S.)',
    # 'inds_nded_share' = 'Non-Deductible Industrial',
    # 'total_expenses_share' = 'Total Services (G.S.+I.S.)',
    # 'industrial_exp_share' = 'Industrial Services (I.S.)',
    # 'deductible_exp_share' = 'Deductible T.S.',
    # 'energy_nondeductibles_share' = 'Electricity + \nNon-Deductibles T.S.',
    # 'deductible_services_share' = 'Deductible G.S.',
    # 'industrial_ded_exp_share' = 'Deductible I.S.',

)


# unrelated_size <- results_df %>%
#     ungroup() %>%
#     select(Size, Structurally.related) %>%
#     filter(Structurally.related == "Not Related") %>%
#     unique() %>%
#     pull(Size)

thresholds_1983 <- "a 500,000 a 1,000,000 a 1,500,000 a 2,000,000 a 2,500,000 a 3,000,000 a 3,500,000 a 4,000,000 a 4,500,000 a 5,000,000 a 5,500,000 a 6,000,000 a 6,500,000"

pattern <- "[[:space:]]*a[[:space:]]"
tax_tresh <- gregexpr(pattern,thresholds_1983) |> regmatches(thresholds_1983,m=_, invert = T)

tax_tresh_1983<-as.numeric(gsub(",","",tax_tresh[[1]]))[2:length(tax_tresh[[1]])]

deflators <-colombia_data_frame %>% ungroup() %>% select(year, p_gdp) %>% unique()
real_tax_tresh_1983 <- tax_tresh_1983/(deflators[deflators$year==83, "p_gdp"][[1]]*1000)


# Split Plot Data for Tinytable --------



sales_tax_change<-tribble(
    ~sic_3, ~Change, ~Change_Year, ~Perry, ~Perry_inds_desc, 
    311, "exempt", NA, " ", " ",# Exempt in data less than 0.3 %  food prods
    312, "exempt", NA, " ", " ",# Exempt in data less than 0.3 % food prods
    313, "increased", 85, "- to 35;10", "Beverages and Tobacco",# Beverages in data 5 -> 9 (6 -> 10)
    314, "decreased", 84, "- to 35;10", "Beverages and Tobacco",# NO: Tobacco in data not significant decrease, maybe 82-83 vs 85 (not top 20)
    321, "increased", 84, "6 to 10", "Textiles",# @Perry1990 6 -> 10 Textiles
    322, "increased", 84, " ", " ",# @Perry1990 6 -> 10 Textiles
    323, "no change", NA, " ", " ",# Leather products in data 8%, no change
    324, "increased", 84, " ", " ",# Footwear in data 6->10
    331, "no change", NA, " ", " ",# Wood and cork products
    332, "decreased", 84, " ", " ",# Furniture, in data small decrease 11->9
    341, "increased", 84, "15 to 10", "Paper",# Paper, data increase 7->9, Perry decreased 15->10
    342, "decreased", 84, " ", " ",# Printing in small decrease 8->7
    351, "decreased", 84, "15 to 10", "Other Chemical Products",# Chemical Products, no change in data 
    352, "no change", NA, "6;15 to 10", "Soap",# Industrial Chemicals @Perry1990 data 8->6
    353, "no change", NA, " ", " ",# Petroleum refineris NO: huge variation.  from 30% to 8% 83 and 2% 84
    354, "no change", NA, "10 to 14", "Oil and Coal Derivatives",# Oil and Coal Derivatives NO:  4%
    355, "no change", NA, " ", " ",# Rubber prods. No change, maybe 82 vs 86-91 
    356, "no change", NA, "15 to 10", "Plastics",# Plastic products 
    361, "increased", 85, " ", " ",# Pottery, china, earthware. NO: Increase 81-83 vs 85-88
    362, "increased", 84, " ", " ",# Glass and glass products: In data significant increase 81-83 vs 84-91
    369, "decreased", 84, " ", " ",# Non-Metallic mineral products; data significant decrease 81-83 vs 84-90
    371, "increased", 84, "6;15 to 10", "Iron and Steel; Nickel Smelting",# Iron and Steel basic industries; significant increase indata 81-83 vs 84-91
    372, "no change", NA, " ", " ",# Metal basic inds: NO. No significant change in data
    381, "increased", 85, " ", " ",# Metal prods. no machinery. Minor significant increase 81-84 vs 85-91
    382, "increased", 85, "6 to 10", "Equipment and Machinery",# Machinery mfg no electrical from 8% to 11% in 84, to 14% in 85
    383, "increased", 85, "6 to 10", "Equipment and Machinery",# Electrical machinery, increase, from 81-83 vs 84-91
    384, "increased", 85, "6 to 10", "Transportation",# Transport equipment, significant increase, 81-84 vs 85-91
    385, "increased", 85, " ", " ",# Prof equipment, gradual increase, 81-83 vs 86-91
    390, "decreased", 84, " ", " ",# Other mfg equipment, gradual decrease, 81-83 vs 88-90
)


candidate_inds<-colombia_data_frame %>%
    filter(
        sales > 0,
        # !is.na(capital),
        !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
    ) %>%
    group_by(sic_3) %>%
    summarise(
        n_sic = n(),
        revenues = sum(sales, na.rm = TRUE),
        corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    arrange(desc(revenues)) %>%
    mutate(
        n_cum = cumsum(n_sic),
        n_perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
        market_share = revenues/ sum(revenues)*100,
        # sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        corps_share = corps_n/n_sic * 100
    ) %>%
    select(sic_3, n_sic, corps_n, corps_share, market_share, n_perc) %>%
    arrange(desc(n_sic)) #%>%
    # filter(n_sic>1000|sic_3==351)

# SIC Data ---------------
ciiu_data <- read.csv("Data/Colombia/ciiu-rev2-en.csv", skip = 3)

ciiu_3 <- ciiu_data %>%
    filter(
        Nivel == "Agrupaciones"
    )
ciiu_4 <- ciiu_data %>%
    filter(
        Nivel == "Grupos"
    )

top_20_inds <- candidate_inds[1:20,] %>%
    left_join(
        ciiu_3[,2:3],
        by = join_by(sic_3 == `Código`)
    ) %>%
    left_join(sales_tax_change) %>%
    mutate(
        description = str_remove(`Descripción`,"Manufacture of |Manufacture ot "),
        description = str_replace_all(
            str_to_title(description),
            c(" And " = " and ", " Of " = " of ", " Or | Ord " = " or ")
        ),
        across(
            corps_share:n_perc,
            ~round(.x, digits = 2)
        ),
        Change = factor(Change, levels = c("exempt","increased","decreased","no change"))
    ) %>%
    arrange(Change) %>%
    select(!`Descripción`)

top_20_inds_table <- colombia_data_frame %>%
    ungroup() %>%
    filter(
        sales > 0,
        # !is.na(capital),
        !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
    ) %>%
    mutate( 
        corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        market_value = sum(sales, na.rm = TRUE),
    ) %>%
    group_by(sic_3,corp) %>%
    summarise(
        n_sic = unique(plant) |> length(),
        revenues_sic = sum(sales, na.rm = TRUE),
        # corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    group_by(sic_3) %>%
    mutate(
        n_cum = cumsum(n_sic),
        n_perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
        market_share = revenues_sic/ sum(revenues_sic)*100,
        tmvs = revenues_sic/market_share*100
        # sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        # corps_share = corps_n/n_sic * 100
    ) %>%
    pivot_wider(
        id_cols = sic_3,
        names_from = corp,
        values_from = c(n_cum,n_sic,n_perc,perc_acc,market_share)
    ) %>%
    select(
        N =`n_cum_Non-Corp`,
        `Corps. (N)` = n_sic_Corp,
        `Corps. (%)` = n_perc_Corp,
        `Market Share (Corps)` = market_share_Corp
    ) %>% arrange(desc(N)) %>%
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

top_10_revenue<-colombia_data_frame %>%
    ungroup() %>%
    filter(
        sales > 0,
        # !is.na(capital),
        !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
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
        # corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    filter( n_sic >= 100) %>%
    ungroup() %>%
    arrange(desc(revenues_sic)) %>%
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
    
plot_data <- colombia_data_frame %>%
    ungroup() %>%
    filter(
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    mutate(
        sic_3 = factor(sic_3, levels = top_20_inds$sic_3)
    ) %>%
    select(plant, juridical_organization, sic_3, year, share_sales_tax)

split_plot_data <- plot_data %>%
    split(plot_data$sic_3)

exps_tbl<-tribble(
    ~Expenditure, ~Code, ~Services, ~Industrial, ~Deductible,
    "Purchases of accessories and replacement parts of less than one year duration", "c1"," ", "$+$"," ",  
    "Purchases of fuels and lubricants consumed by the establishment", "c2"," ", "$+$", "$+$",
    "Payments for industrial work by other establishments", "c3"," ", "$+$", " ",
    "Payment of domestic workers", "c4"," ","$+$", " ",
    "Payments of third parties for repairs and maintenance", "c5"," ", "$+$", "$+$",
    "Purchases of raw materials and goods sold without transformation", "c6"," ", "$+$", "$+$",
    "TOTAL Industrial Expenditures (c1:c6)", "c7",  " "," ", " ",
    "Rent of fixed property", "c8","$+$", " ", " ",
    "Payments for professional services", "c9","$+$", " "," ", 
    "Machinery rental", "c10", " ", " ", "$+$",
    "Insurance, excl. employe benefits", "c11","$+$", " ", "$+$",
    "Water, mail, telephone, etc.", "c12","$+$", " ", "$+$",
    "Publicity and advertising", "c13","$+$", " ", " ",
    "Interest payments", "c14", " "," ", " ", 
    "Royalty payments", "c15" ,"$+$", " ", " ",
    "Other expenditures", "c16","$+$", " ", " ",
    "TOTAL General Expenditures (c8:c16)", "c17", " "," ", " ",
    "TOTAL Expenditure (c7+c17)", " ", " ", " ", " ",
)

# Saving global variables -----------
print("Saving global variables")
save(
    list = setdiff(ls(), "colombia_data_frame"),
    file = "Code/Products/global_vars.RData"
)
