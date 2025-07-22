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


# Sum rows across columns ignoring NAs unless all are NAs
sum_rows <- function(...) {
    df <- cbind(...)
    apply(df, 1, function(row) {
        if (all(is.na(row))) {
        return(NA)
        } else {
        return(sum(row, na.rm = TRUE))
        }
    })
}



## Real vars base 81 ----

cdf <- col_df %>%
    filter(datayear >= 81) %>%
    ungroup() %>% #rowwise() %>%
    mutate(
        p_energy_purchased = ifelse(e1 == 0, NA, e5 / e1),
        p_energy_sold = ifelse(e3 == 0, NA, e6 / e3),
        p_energy_weighted = p_energy_purchased* (e1/sum_rows(e1, e3))+p_energy_sold*(e3 / sum_rows(e1, e3)),
        p_energy = ifelse(e4 == 0, NA, e7 / e4)
    ) %>%
    # ungroup() %>%
    # Changing from GNR p_energy=e7/e4, because e4 = e1+e2-e3, and e7=e5-e6
    group_by(datayear) %>%
    change_base_81(
        c(p_machin, p_struc, p_transp, p_gdp, p_energy, p_energy_purchased, p_energy_weighted, p_energy_sold)
    ) %>%
    ungroup() %>%
    mutate(
        rland_80 = rland_80 * p_struc / p_struc_new,
        rbldg_80 = rbldg_80 * p_struc / p_struc_new,
        rmach_80 = rmach_80 * p_machin / p_machin_new,
        rtrans_80 = rtrans_80 * p_transp / p_transp_new,
        roffice_80 = roffice_80 * p_machin / p_machin_new,
        # rcap80 = rland_80 + rbldg_80 + rmach_80 + rtrans_80 + roffice_80, # This code generates NAs if a value is missing
        rcap80 = rowSums(cbind(rland_80, rbldg_80, rmach_80, rtrans_80, roffice_80)),
        real_gross_output = pg / p_gdp_new, # real gross output, base 1981
        real_capital = ifelse( # real capital stock, base 1981
            rcap80 > 0,
            rcap80,
            NA
        ),
        labor_employees = sklab * (skwages / sklab) / (unskwages / unsklab) + unsklab,
        labor_employee_years = labor_employees * x7 / 12,
        real_wages = rowSums(cbind(skwages,unskwages)) / p_gdp_new,
        real_energy = e7 / p_energy_new,
        consumed_energy = e4 * p_energy_weighted_new,
        generated_energy = e2 * p_energy_weighted_new,
        purchased_energy = e5 / p_energy_purchased_new,
        sold_energy = e6 / p_energy_sold_new,
        real_materials = s10 / p_gdp_new,
        # rserv is general expenses - machinery rental - interest payments; c17-c10-c14
        # real_services = rserv * p_gdp / p_gdp_new,
        nom_services = rowSums(cbind(c17,-c10,-c14)),
        real_services = nom_services/ p_gdp_new,
        deductible_services = real_services-rowSums(cbind(c8,c9,c13,c15,c16))/p_gdp_new,
        non_deductible_services = real_services-rowSums(cbind(c11,c12))/p_gdp_new,
        nom_mats_serv = rowSums(cbind(s10,nom_services)),
        real_mats_serv = rowSums(cbind(real_materials,real_services)),
        nom_intermediates = rowSums(cbind(s10,nom_services, e7)),
        real_intermediate_inputs = rowSums(cbind(real_materials,real_services,real_energy)),
        nom_sales = ifelse(s5 > 0, s5 ,NA),
        real_sales = nom_sales / p_gdp_new,
        sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        share = ifelse(pg==0, NA, nom_intermediates / pg),
        log_share = log(share),
        real_indirect_taxes = t6 / p_gdp_new,
        real_consumption_taxes = t3 / p_gdp_new,
        real_sales_taxes = t1 / p_gdp_new,
        real_sales_tax_purchases = t2 / p_gdp_new,
        real_import_taxes = t4 / p_gdp_new,
        export_taxes = t5 / p_gdp_new,
        real_general_expenditure = c17 / p_gdp_new,
        real_industrial_expenditure = c7 / p_gdp_new,
        real_expenditure = rowSums(cbind(real_general_expenditure,real_industrial_expenditure)),
        deductible_expenses = rowSums(cbind(c2, c5, c6, c10, c11, c12))/ p_gdp_new, #c14 interest payments deductible for Corporations) ?
        non_deductible_expenses = rowSums(cbind(real_expenditure, -deductible_expenses)),
        non_deductible_inds_exp = rowSums(cbind(c7, -c2, -c5, -c6)) / p_gdp_new,
        deductible_inds_exp = rowSums(cbind(c7,-c1,-c3,-c4 ))/ p_gdp_new,
        share_fem_managers = k11 / rowSums(cbind(k3,k11)),
        male_owners = k2,
        female_owners = k10,
        total_owners = rowSums(cbind(k2,k10)),
        share_fem_owners = k10 / total_owners,
        share_exports = s4 / s5,
        share_imports = s7 / s8,
        share_imports_materials = s11 / s10,
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
        sales_tax_sales = t1 / nom_sales,
        sales_tax_purchases = t2 / nom_sales,
        skilled_wage_bill_share = (skwages / p_gdp_new) / pg,
        unskilled_wage_bill_share = (unskwages / p_gdp_new) / pg,
        fuels = c2/p_gdp_new,
        repair_maintenance = c5/p_gdp_new

    ) %>%
    select(
        plant,
        year = datayear,
        sic_3,
        p_gdp = p_gdp_new,
        gross_output = real_gross_output,
        nom_gross_output = pg,
        sales = real_sales,
        materials = real_materials,
        nom_energy = e7,
        energy = real_energy,
        consumed_energy,
        generated_energy,
        purchased_energy,
        sold_energy,
        services = real_services,
        mats_serv = real_mats_serv,
        intermediates = real_intermediate_inputs,
        labor_employees,
        labor_employee_years,
        wages = real_wages,
        capital = real_capital,
        log_share,
        indirect_taxes = real_indirect_taxes,
        consumption_taxes_on_sales = real_consumption_taxes,
        sales_taxes = real_sales_taxes,
        # sales_tax_purchases,
        export_taxes,
        import_taxes = real_import_taxes,
        share_fem_owners,
        share_exports,
        general_expenditure = real_general_expenditure,
        industrial_expenditure = real_industrial_expenditure,
        total_expenditure = real_expenditure,
        non_deductible_expenses,
        deductible_expenses,
        non_deductible_inds_exp,
        deductible_inds_exp,
        share_fem_managers,
        share_imports,
        share_imports_materials,
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
        skilled_wage_bill_share,
        unskilled_wage_bill_share,
        p_energy_purchased_new,
        p_energy_sold,
        p_energy_new,
        p_energy_purchased,
        fuels,
        deductible_services,
        non_deductible_services,
        repair_maintenance,
        nom_mats = s10,
        nom_services,
        nom_mats_serv,
        nom_intermediates,
        nom_fuels = c2,
        nom_energy = e7,
        nom_repair_maint = c5,
        nom_sales,
        sales_tax_purchases,
        sales_tax_sales
        # share
    )

## Adding size variables March 20, 2023 ----

# Production last year, sales last year, paid taxes

# Using data.table
# colombia_data_table <- as.data.table(cdf)
# colombia_data_table[, c("lag_gross_output","lag_sales") := shift(.SD), by=plant, .SDcols= c("gross_output","sales")]

# Using tidyverse
colombia_data_frame <- cdf %>%
    arrange(plant,year) %>%
    group_by(plant) %>%
    mutate(
        lag_gross_output = lag(gross_output),
        lag_sales = lag(sales),
        lag_indirect_tax = lag(indirect_taxes),
        # lag_M = lag(intermediate_inputs),
        lag_K = lag(capital),
        lag_sales_tax = lag(sales_taxes),
        # lag_imex_tax = lag(imex_taxes),
        # lag_consumption_tax = lag(consumption_taxes),
        # lag_gen_exp = lag(general_expenditure),
        # lag_share_exports = lag(share_exports),
        y = log(gross_output),
        k = log(capital),
        l = log(labor_employee_years),
        m = log(intermediates),
        lag_k = lag(k),
        lag_m = lag(m),
        lag_l = lag(l),
        log_sales = log(sales),
        # den = nom_gross_output, #nom_gross_output in GNR
        lag_log_sales = lag(log_sales),
        capital_share = capital/sales, #gross_output
        materials_share = nom_mats/nom_gross_output,
        # mats_serv_share = nom_mats_serv/nom_gross_output,
        energy_share = nom_energy/nom_gross_output,
        fuels_share = nom_fuels/nom_gross_output,
        repair_maint_share = nom_repair_maint/nom_gross_output,
        services_share = nom_services/nom_gross_output,
        # inds_nded_share =  non_deductible_inds_exp/gross_output,
        gnr_int_share = nom_intermediates/nom_gross_output,
        deductible_intermediates = rowSums(cbind(materials,energy,fuels,repair_maintenance)),
        nom_deductible_intermediates = rowSums(cbind(nom_mats,nom_energy,nom_fuels,nom_repair_maint)),
        nom_non_deductible_intermediates = nom_services,
        deductible_intermediates_share = nom_deductible_intermediates/nom_gross_output,
        non_deductible_intermediates_share = nom_non_deductible_intermediates/nom_gross_output,
        # gnr_other_share = sum_rows(cbind(energy_share,services_share)),
        # intermediates_share = intermediates/gross_output,
        log_mats_share = log(materials_share),
        # log_mats_serv_share = log(mats_serv_share),
        log_energy_share = log(energy_share),
        log_fuels_share = log(fuels_share),
        log_repair_maint_share = log(repair_maint_share),
        log_services_share = log(services_share),
        # log_inds_nded_share =  log(inds_nded_share),
        log_deductible_intermediates_share = log(deductible_intermediates_share),
        log_non_deductible_intermediates_share = log(non_deductible_intermediates_share)#,
        # log_gnr_other_share = log(gnr_other_share)
        # lp_share = sum_rows(cbind(materials,fuels,consumed_energy/1000))/sales,
        # inds_int_share = sum_rows(cbind(materials,consumed_energy/1000,industrial_expenditure))/sales,
        # mats_deduct_share = sum_rows(cbind(materials,deductible_expenses))/sales,
        # energy_nondeductibles_share = sum_rows(cbind(consumed_energy/1000,non_deductible_expenses))/sales,
        # log_ded_share = log(mats_deduct_share),
        # gnr_nded_share = sum_rows(cbind(consumed_energy/1000,non_deductible_services))/sales,
        # lp_ded_share = sum_rows(cbind(materials,fuels))/sales,
        # lp_nded_share = sum_rows(cbind(consumed_energy/1000))/sales,
        # inds_ded_share =  sum_rows(cbind(materials,deductible_inds_exp))/sales,
        # inds_nded_share =  sum_rows(cbind(consumed_energy/1000,non_deductible_inds_exp))/sales,
        # log_gnr_ded = log(gnr_ded_share),
        # log_gnr_nded = log(gnr_nded_share),
        # log_lp = log(lp_share),
        # log_lp_ded = log(lp_ded_share),
        # log_lp_nded = log(lp_nded_share),
        # log_inds_int = log(inds_int_share),
        # log_inds_ded =  log(inds_ded_share),
        # total_expenses_share = total_expenditure/sales,
        # industrial_exp_share = industrial_expenditure/total_expenditure,
        # deductible_exp_share = deductible_expenses/total_expenditure,
        # deductible_services_share = deductible_services/total_expenditure,
        # industrial_ded_exp_share = deductible_inds_exp/total_expenditure,
        # inds_exp_non_deductible = non_deductible_inds_exp
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

# Adding JO Classes ---------------

## JO Classes ------------
jo_class <- tibble(
    JO_code = 0:9,
    JO_class = c(
        "Proprietorship",
        "Ltd. Co.",
        "Partnership",
        "Corporation",
        "Partnership",
        "Partnership",
        "Corporation",
        # "Stock Co. (Corp.)",
        "Other",
        "Other",
        "Other"
    )
)

colombia_data_frame <- colombia_data_frame %>%
    ungroup() %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code))

# Saving data ---------------------------------
print("Saving Colombia DF")
save(colombia_data_frame, jo_class, sum_rows, file = "Code/Products/colombia_data.RData")
