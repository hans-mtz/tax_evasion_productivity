## %% load libraries and data ------------------------
# library(tidyverse)

## %% Set up vars

folder_results <- "/Volumes/SSD Hans 1/Github/gnr/Data/"

gnr_inds <- c(311, 321, 322, 331, 381)
vars <- c("m", "k", "l")
err_vars <- c("bigE", "err_sd")

## %% Collecting Stata Results ------------------------

wrk_data_stata_code <- read.csv("Code/Products/gnr_rep.csv", skip = 1)
wrk_data_stata_code

stata_data_stata_code <- read.csv("Code/Products/gnr_org.csv", skip = 1)
stata_data_stata_code

## %% Collecting Fortran Results ------------------------



# names_tbl <- paste0("_", gnr_inds)

for_list <- lapply(
    gnr_inds,
    \(x){
        temp_coeff<-read.csv(paste0(folder_results,"stata_coeffs_",x,".out"))
        temp_coeff$inds <- x
        return(temp_coeff)
    }
)


(wrk_data_fortran_code <- do.call(rbind,for_list))

wrk_data_fortran_code |>
    round(2) -> wrk_data_fortran_code
wrk_data_fortran_code
# names(wrk_data_fortran_code) <- inds

## %% Merging Results ------------------------


gnr_rep_tbl<-cbind(stata_data_stata_code[,vars], wrk_data_stata_code[,vars], wrk_data_fortran_code[,vars])

# names(gnr_rep_tbl) <- rep(names_tbl, 3)

rownames(gnr_rep_tbl) <- gnr_inds
gnr_rep_tbl


gnr_rep_tbl[,7:9] |>
    knitr::kable()
## %% Merge Error Stats Results ------------------------


gnr_err_tbl<-cbind(stata_data_stata_code[,err_vars], wrk_data_stata_code[,err_vars], wrk_data_fortran_code[,err_vars])

rownames(gnr_err_tbl) <- gnr_inds
gnr_err_tbl

## %% Saving Results ------------------------

save(
    gnr_rep_tbl, gnr_err_tbl,
    file = "Code/Products/replication.RData"
)

# %% CD RESULTS ------------------------

# Collecting Stata Results ------------------------

wrk_data_stata_code_CD <- read.csv("Code/Products/gnr_rep_cd.csv", skip = 1)
wrk_data_stata_code_CD

stata_data_stata_code_CD <- read.csv("Code/Products/gnr_org_cd.csv", skip = 1)
stata_data_stata_code_CD

## %% Collecting Fortran Results ------------------------

for_list_CD <- lapply(
    gnr_inds,
    \(x){
        temp_coeff<-read.csv(paste0(folder_results,"CD_coeffs_Stata_",x,".out"))
        temp_coeff$inds <- x
        return(temp_coeff)
    }
)


(wrk_data_fortran_code_CD <- do.call(rbind,for_list_CD))

wrk_data_fortran_code_CD |> 
    round(2) -> wrk_data_fortran_code_CD


wrk_data_fortran_code_CD
## %% Merging Results ------------------------

gnr_rep_tbl_CD<-cbind(stata_data_stata_code_CD[,vars], wrk_data_stata_code_CD[,vars], wrk_data_fortran_code_CD[,vars])

gnr_rep_tbl_CD
rownames(gnr_rep_tbl_CD) <- gnr_inds
gnr_rep_tbl_CD

## %% Merge Error Stats Results ------------------------


gnr_err_tbl_CD<-cbind(stata_data_stata_code_CD[,err_vars], wrk_data_stata_code_CD[,err_vars], wrk_data_fortran_code_CD[,err_vars])

rownames(gnr_err_tbl_CD) <- gnr_inds
gnr_err_tbl_CD

## %% Saving Results ------------------------
save(
    gnr_rep_tbl_CD, gnr_rep_tbl, gnr_err_tbl,
    gnr_err_tbl_CD,
    file = "Code/Products/replication.RData"
)

## %% fun stuff ------------------------------- 

# read.csv("Code/Products/stata-gnr-trim.csv", skip =1) %>%
#     mutate(
#         `Ind.` = str_extract(X,"\\d{3}"),
#         `Inter.` = str_extract(X,"(ded|m_s_e|mats)$"),
#         .before = m
#     ) %>%
#     select(-X)

load("Code/Products/replication.RData")
