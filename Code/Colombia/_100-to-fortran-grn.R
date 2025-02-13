# Load data and packages ---------------
library(tidyverse)
library(haven)
library(readxl)
library(modelsummary)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv.RData")
load("Code/Products/boot_tax_ev_mmt.RData")

# Setting up folders and vars ----------------

folder_results <- "/Volumes/SSD Hans 1/Github/gnr/Data/"
# Stata_GNR_results_folder <- "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"
evasion_inds<-top_evading_inds[1:5] #c(322,324, 342, 313, 321, 351) #same as order_sic[1:6]

gnr_inds<-c(311,321,322,331,381)

# Wrangling data to save to raw ---------

## Saving data for fortran -----
# for (inds in union(gnr_inds,evasion_inds)){
for (inds in evasion_inds){
fort_data<-colombia_data_frame %>%
    ungroup() %>%
    select(
        !m
    ) %>%
    filter(
        is.finite(log_sales), # is.finite if it is not NA,NaN, Inf or -Inf
        is.finite(log(gross_output)),
        # is.finite(m),
        is.finite(log_mats_share),
        is.finite(materials),
        is.finite(l),
        is.finite(k),
        is.finite(log_share),
        # !is.na(l),
        # !is.na(k),
        # !is.na(m),
        # !is.na(log_sales),
        # !is.na(log(gross_output)),
        # !is.na(log_share)
    ) %>%
    mutate(
        y=log(gross_output),
        m=log(materials),
        s=log_mats_share,
        ll=l*l,
        mm=m*m,
        mk=m*k,
        ml=m*l,
        kk=k*k,
        kl=k*l,
        klm=k*l*m#,
        # across(
        #     ll:s,
        #     ~lag(.x, order_by = year),
        #     .names = "lag_{.col}"
        # )
    ) %>%
    filter(sic_3==inds) %>%
    group_by(plant,year) %>% 
    arrange(plant,year) %>%
    select(
        y,
        s,
        m,
        k,
        l,
        matches("^[[:lower:]]{2}$"),
        klm
        # matches("^lag_[[:lower:]]{1,2}$") &  !lag_K &!lag_M
    )
# fort_data
fort_data<-fort_data[complete.cases(fort_data), , drop=FALSE]
fort_data
# fort_data |> datasummary_skim() |>
print(dim(fort_data))
write_delim(
    fort_data,
    paste0(folder_results,"R_data_",inds,".raw"),
    append = FALSE,
    col_names = FALSE
)
variable_names<-names(fort_data)
write.table(
    variable_names,
    paste0(folder_results,"R_variables_",inds,".raw"),
    # folder_results,"variables_all.raw",
    col.names = FALSE,
    row.names = FALSE
    # append = FALSE
)
}


## All industries -------------------------

fort_data<-colombia_data_frame %>%
    group_by(plant) %>%
    filter(
        sales>0,
        # k>0,
        # m>0,
        # labor_employee_years>0,
        # labor_employee_years<Inf,
        # unskilled_labor>0,
        # unskilled_wage_bill_share>0,
        !is.na(l),
        l < Inf,
        l > -Inf,
        # !is.na(k),
        # !is.na(m),
        # !is.na(lag_l),
        # lag_l<Inf,
        # lag_l> -Inf,
        # !is.na(lag_ml),
        # !is.na(lag_ll),
        # !is.na(lag_kl)
    ) %>%
    mutate(
        y=log(gross_output),
        s=log_share,
        ll=l*l,
        mm=m*m,
        mk=m*k,
        ml=m*l,
        kk=k*k,
        kl=k*l,
        klm=k*l*m#,
        # across(
        #     ll:s,
        #     ~lag(.x, order_by = year),
        #     .names = "lag_{.col}"
        # )
    ) %>%
    # filter(sic_3==inds) %>%
    group_by(plant,sic_3,year) %>% 
    select(
        y,
        s,
        m,
        k,
        l,
        matches("^[[:lower:]]{2}$"),
        klm
        # matches("^lag_[[:lower:]]{1,2}$") &  !lag_K &!lag_M
    )
# fort_data
fort_data<-fort_data[complete.cases(fort_data), , drop=FALSE]
fort_data

fort_data |> datasummary_skim()

write_delim(
    fort_data,
    paste0(folder_results,"R_data_999.raw"),
    append = FALSE,
    col_names = FALSE
)

variable_names<-names(fort_data)
write.table(
    variable_names,
    paste0(folder_results,"R_variables_all.raw"),
    col.names = FALSE,
    row.names = FALSE
    # append = FALSE
)


### Testing FORTRAN OLS-------------------------

# feols(s~m+k+l+ll+mm+mk+ml+kk+kl,fort_data)
# X<-read.csv(
#     paste0(folder_results,'fortran_data_X).out',
#     header = FALSE
# )
# y<-read.csv(
#     paste0(folder_results,'fortran_data_y).out',
#     header = FALSE
# )

# lm(y[[1]]~as.matrix(X)) |> summary()

# Save to dta ---------------------------
# write_dta(fort_data,"Code/Products/fort_data.dta")
write_dta(
    fort_data,
    paste0(folder_results,"fort_data.dta"))


# Saving results -----------------------------


save(
    evasion_inds, gnr_inds,
    file = "Code/Products/to_fortran_CD_GNR.RData"
)
