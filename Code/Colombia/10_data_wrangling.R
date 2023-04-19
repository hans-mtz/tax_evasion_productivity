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
    mutate(p_energy = ifelse(e4 == 0, NA, e7 / e4)) %>%
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
        real_materials = s10 / p_gdp_new,
        real_services = rserv * p_gdp / p_gdp_new,
        real_mats_serv = real_materials + real_services,
        real_intermediate_inputs = real_materials + real_services + real_energy,
        real_sales = s5 / p_gdp_new,
        sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        real_indirect_taxes = t6 / p_gdp,
        log_share = log(real_intermediate_inputs / real_sales)
        # real_consumption_taxes = t3/p_gdp,
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
        services = real_services,
        mats_serv = real_mats_serv,
        intermediate_inputs = real_intermediate_inputs,
        labor_employees,
        labor_employee_years,
        wages = real_wages,
        capital = real_capital,
        indirect_taxes = real_indirect_taxes,
        log_share
        # consumption_taxes = real_consumption_taxes
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
        lag_taxes = lag(indirect_taxes, order_by = year)
    )

# Comparing DataTable vs. DataFrame. They're the same!
# all(
#     colombia_data_table$lag_gross_output == colombia_data_frame$lag_gross_output,
#     na.rm = TRUE
# )
print("Saving Colombia DF")
save(colombia_data_frame, file = "Code/Products/colombia_data.RData")
