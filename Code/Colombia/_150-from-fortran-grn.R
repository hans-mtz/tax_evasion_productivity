# Load data and packages ---------------
library(tidyverse)
library(haven)
library(readxl)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv.RData")
load("Code/Products/to_fortran_CD_GNR.RData")
load("Code/Products/deconv_prod_fun.RData")

# Setting up folders and vars ----------------

Stata_GNR_results_folder <- "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"

# Testing Fortran Estimator -------------

## Reading Fortran Results -------------------

# "CD_coeffs_stata_311.out"

## CD GNR Fortran Results PF parameters (Stata Data) -------------------
read.csv(paste0(folder_results,"CD_coeffs_stata_",311,".out"))

(CD_fortran <- lapply(
    union(evasion_inds,gnr_inds),
    \(x){
        temp_coeff<-read.csv(paste0(folder_results,"CD_coeffs_stata_",x,".out"))
        temp_coeff$inds <- x
        return(temp_coeff)
    }
)
)

CD_fortran_tbl <- do.call(rbind,CD_fortran)

# CD_fortran_tbl[CD_fortran_tbl$inds %in% evasion_inds,c(4,1,3,2)]
# CD_fortran_tbl[CD_fortran_tbl$inds %in% order_sic[1:6],c(4,1,3,2)]



## CD GNR Fortran Results R Data -------------------


(CD_fortran_R <- lapply(
    # union(evasion_inds,gnr_inds),
    evasion_inds,
    \(x){
        temp_coeff<-read.csv(paste0(folder_results,"CD_coeffs_R_",x,".out"))
        temp_coeff$inds <- x
        return(temp_coeff)
    }
)
)

CD_fortran_tbl_R <- do.call(rbind,CD_fortran_R)
CD_fortran_tbl_R

# evasion_tbl<-sapply(paste0(order_sic[1:6]," log_share"),\(x)prod_fun_list[[x]]$coeffs) |> t() |> round(4)
evasion_tbl<-sapply(names(prod_fun_list),\(x)prod_fun_list[[x]]$coeffs) |> t() |> round(4)

## CD GNR Fortran Results Productivity R Data -------------------

(CD_fortran <- lapply(
    union(evasion_inds,gnr_inds),
    \(x){
        temp_coeff<-read.csv(paste0(folder_results,"CD_productivity_stata_",x,".out"))
        temp_coeff$inds <- x
        return(temp_coeff)
    }
)
)

CD_fortran_tbl <- do.call(rbind,CD_fortran)


## Reading Stata CD GNR Results to compare with Fortran -----------------------
# Especifica la ruta al archivo Excel
Stata_GNR_results_folder <- "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"
file_path <- file.path(Stata_GNR_results_folder, "CD_GNR_coeffs.xlsx")
# Lee el archivo Excel
Stata_results <- read_excel(file_path)


# Saving results -----------------------------

save(
    CD_fortran_tbl, CD_fortran_tbl_R,evasion_inds, Stata_results,
    file = "Code/Products/Fortran_CD_GNR.RData"
)
