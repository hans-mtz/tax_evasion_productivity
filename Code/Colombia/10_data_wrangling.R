library(tidyverse)
# library(data.table)
load("Code/Products/col_df.RData")
## Following SN&DR "ols.do" file to get real vars----

# formula to change price base
change_base_81 <- function(data, vars) {
    a81 <- data %>%
        filter(datayear == 81) %>%
        summarise(
            across(
                {{ vars }},
                ~ mean(.x, na.rm = TRUE)
            )
        )
    data %>%
        mutate(
            across(
                {{ vars }},
                ~ .x / a81[[cur_column()]],
                .names = "{.col}_new"
            )
        )
}

## Real vars base 81 ----

cdf <- col_df %>%
    filter(datayear >= 81) %>%
    mutate(
        p_energy_purchased = ifelse(e1 == 0, NA, e5 / e1),
        p_energy_sold = ifelse(e3 == 0, NA, e6 / e3),
        p_energy = ifelse(
            sum(e1, e3, na.rm = TRUE) == 0,
            NA,
            sum(e5, e6, na.rm = TRUE) / sum(e1, e3, na.rm = TRUE)
        )
    ) %>%
    # Changing from GNR p_energy=e7/e4, because e4 = e1+e2-e3, and e7=e5-e6
    group_by(datayear) %>%
    change_base_81(
        c(p_machin, p_struc, p_transp, p_gdp, p_energy)
    ) %>%
    ungroup() %>%
    mutate(
        rland_80 = rland_80 * p_struc / p_struc_new,
        rbldg_80 = rbldg_80 * p_struc / p_struc_new,
        rmach_80 = rmach_80 * p_machin / p_machin_new,
        rtrans_80 = rtrans_80 * p_transp / p_transp_new,
        roffice_80 = roffice_80 * p_machin / p_machin_new,
        rcap80 = rland_80 + rbldg_80 + rmach_80 + rtrans_80 + roffice_80,
        real_gross_output = pg / p_gdp_new, # real gross output, base 1981
        real_capital = ifelse( # real capital stock, base 1981
            rcap80 > 0,
            rcap80,
            NA
        ),
        labor_employees = sklab * (skwages / sklab) / (unskwages / unsklab) + unsklab,
        labor_employee_years = labor_employees * x7 / 12,
        real_wages = (skwages + unskwages) / p_gdp_new,
        real_energy = e7 / p_energy_new,
        consumed_energy = e4 * p_energy_new,
        generated_energy = e2 * p_energy_new,
        purchased_energy = e5 / p_energy_new,
        sold_energy = e6 / p_energy_new,
        real_materials = s10 / p_gdp_new,
        # rserv is general expenses - machinery rental - interest payments; c17-c10-c14
        real_services = rserv * p_gdp / p_gdp_new,
        real_mats_serv = real_materials + real_services,
        real_intermediate_inputs = real_materials + real_services + real_energy,
        real_sales = s5 / p_gdp_new,
        sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        share = real_intermediate_inputs / real_sales,
        log_share = log(real_intermediate_inputs / real_sales),
        real_indirect_taxes = t6 / p_gdp_new,
        real_consumption_taxes = t3 / p_gdp_new,
        real_sales_taxes = (t1) / p_gdp_new,
        sales_tax_purchases = t2 / p_gdp_new,
        real_import_taxes = (t4) / p_gdp_new,
        export_taxes = (t5) / p_gdp_new,
        real_general_expenditure = c17 / p_gdp_new,
        real_industrial_expenditure = c7 / p_gdp_new,
        real_expenditure = real_general_expenditure + real_industrial_expenditure,
        non_deductible_expenses = (c7 + c17 - c2 - c5 - c6 - c10 - c11 - c12 ) / p_gdp_new,
        deductible_expenses = ((c7 + c17) / p_gdp_new) - non_deductible_expenses,
        inds_exp_non_deductible = (c7 - c2 - c5 - c6) / p_gdp_new,
        inds_exp_deductible = (c7 / p_gdp_new) - inds_exp_non_deductible,
        share_fem_managers = k11 / (k3 + k11),
        male_owners = k2,
        female_owners = k10,
        total_owners = k2 + k10,
        share_fem_owners = k10 / (k2 + k10),
        share_exports = s4 / s5,
        share_imports = s7 / s8,
        real_interest_payments = c14 / p_gdp_new,
        age = datayear - x6,
        real_subsidies = t9 / p_gdp_new,
        real_subsidies_prod = t7 / p_gdp_new,
        real_subsidies_exp = t8 / p_gdp_new,
        real_exports = s4 / p_gdp_new,
        real_export_tax = t5 / p_gdp_new,
        real_imported_inputs = s7 / p_gdp_new,
        real_raw_material_foreign = s11 / p_gdp_new,
        real_import_tax_raw_mat = t4 / p_gdp_new,
        share_sales_tax = real_sales_taxes / real_sales,
        skilled_labor_share = (sklab * (skwages / p_gdp_new)) / real_sales,
        unskilled_labor_share = (unsklab * (unskwages / p_gdp_new)) / real_sales
    ) %>%
    select(
        plant,
        year = datayear,
        sic_3,
        p_gdp = p_gdp_new,
        gross_output = real_gross_output,
        sales = real_sales,
        materials = real_materials,
        energy = real_energy,
        consumed_energy,
        generated_energy,
        purchased_energy,
        sold_energy,
        services = real_services,
        mats_serv = real_mats_serv,
        intermediate_inputs = real_intermediate_inputs,
        labor_employees,
        labor_employee_years,
        wages = real_wages,
        capital = real_capital,
        log_share,
        indirect_taxes = real_indirect_taxes,
        consumption_taxes_on_sales = real_consumption_taxes,
        sales_taxes = real_sales_taxes,
        sales_tax_purchases,
        export_taxes,
        import_taxes = real_import_taxes,
        share_fem_owners,
        share_exports,
        general_expenditure = real_general_expenditure,
        industrial_expenditure = real_industrial_expenditure,
        total_expenditure = real_expenditure,
        non_deductible_expenses,
        deductible_expenses,
        inds_exp_non_deductible,
        inds_exp_deductible,
        share_fem_managers,
        share_imports,
        interest_payments = real_interest_payments,
        age,
        subsidies = real_subsidies,
        subsidies_prod = real_subsidies_prod,
        subsidies_exp = real_subsidies_exp,
        exports = real_exports,
        export_tax = real_export_tax,
        imported_inputs = real_imported_inputs,
        raw_material_foreign = real_raw_material_foreign,
        import_tax_raw_mat = real_import_tax_raw_mat,
        share_sales_tax,
        juridical_organization = x3,
        metro_area_code = x4,
        section_country_code = x5,
        male_owners,
        total_owners,
        female_owners,
        skilled_labor = sklab,
        unskilled_labor = unsklab,
        skilled_labor_share,
        unskilled_labor_share,
        p_energy_new,
        p_energy_sold,
        p_energy_purchased
    )

## Adding size variables March 20, 2023 ----

# Production last year, sales last year, paid taxes

# Using data.table
# colombia_data_table <- as.data.table(cdf)
# colombia_data_table[, c("lag_gross_output","lag_sales") := shift(.SD), by=plant, .SDcols= c("gross_output","sales")]

# Using tidyverse
colombia_data_frame <- cdf %>%
    group_by(plant) %>%
    mutate(
        lag_gross_output = lag(gross_output, 1, order_by = year),
        lag_sales = lag(sales, 1, order_by = year),
        lag_indirect_tax = lag(indirect_taxes, order_by = year),
        lag_M = lag(intermediate_inputs, order_by = year),
        lag_K = lag(capital, order_by = year),
        lag_sales_tax = lag(sales_taxes, order_by = year),
        # lag_imex_tax = lag(imex_taxes, order_by = year),
        # lag_consumption_tax = lag(consumption_taxes, order_by = year),
        # lag_gen_exp = lag(general_expenditure, order_by = year),
        # lag_share_exports = lag(share_exports, order_by = year),
        k = log(capital),
        l = log(labor_employee_years),
        m = log(intermediate_inputs),
        lag_k = log(lag_K),
        lag_m = log(lag_M),
        lag_l = lag(l, order_by = year),
        log_sales = log(sales),
        lag_log_sales = log(lag_sales),
        # log_gen_ex = log(general_expenditure),
        # lag_log_gen_exp = log(lag_gen_exp),
        # # lag_log_consumption_tax = log(lag_consumption_tax),
        # lag_log_sales_tax = log(lag_sales_tax),
        # lag_log_sales_tax_sales = lag_log_sales_tax-lag_log_sales,
        # log_int_pym_lag_k = log(interest_payments)-lag_k,
        # lag_log_int_pym_k = log(lag(interest_payments, order_by = year))-lag_k,
        # lag_log_sales_k = lag_log_sales-lag_k,
        # lag_log_gen_exp_k = lag_log_gen_exp-lag_k,
        # lag_log_ind_exp_k = log(lag(industrial_expenditure, order_by = year))-lag_k,
        # lag_log_exp_k = log(lag(expenditure, order_by = year))-lag_k,
        # log_int_pym_lag_l = log(interest_payments)-lag_l,
        # lag_log_int_pym_l = log(lag(interest_payments, order_by = year))-lag_l,
        # lag_log_sales_l = lag_log_sales-lag_l,
        # lag_log_gen_exp_l = lag_log_gen_exp-lag_l,
        # lag_log_ind_exp_l = log(lag(industrial_expenditure, order_by = year))-lag_l,
        # lag_log_exp_l = log(lag(expenditure, order_by = year))-lag_l,
    )

# Comparing DataTable vs. DataFrame. They're the same!
# all(
#     colombia_data_table$lag_gross_output == colombia_data_frame$lag_gross_output,
#     na.rm = TRUE
# )
print("Saving Colombia DF")
save(colombia_data_frame, file = "Code/Products/colombia_data.RData")
