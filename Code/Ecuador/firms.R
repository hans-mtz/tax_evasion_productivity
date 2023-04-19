# library(foreign)
# library(tidyverse)
# library(todor)

## Input-output df ----
# source("Estimation/output.R")
# source("Estimation/input.R")

## Reading Firm's output ----

firms_file<-"../data/Ecuador/2019/2019_ENESEM_TOMOI_BDD_CSV/2019_estructural_empresarial_bdd.csv"

firms_raw <- read.csv2(firms_file)

# names(firms_raw)

## Industries ----

inds_raw <- firms_raw  %>% 
    select(cod_letra:des_ciiu4d)  %>% 
    unique(.)

inds_4d_n <- firms_raw  %>% 
    select(cod_letra:des_ciiu4d)  %>%
    group_by(cod_ciiu4d) %>% 
    summarise(ciiu4d_n = n())

inds_2d_n <- firms_raw  %>% 
    select(cod_letra:des_ciiu4d)  %>%
    group_by(cod_ciiu2d) %>% 
    summarise(ciiu2d_n = n())

inds <- left_join(inds_raw,inds_2d_n)
inds <- left_join(inds,inds_4d_n)

## Data wrangling ----

# firms_raw  %>% 
#     head(c(6L,10L))


df <- firms_raw %>% 
    select(
        id_empresa,
        cod_letra:des_ciiu4d,
        cod_tamano:anio_ruc_dis,
        v1001,
        v2006,v4001,
        v5180,Vbp,
        matprima,totalpeoc,totremun,
        cant_ener,cant_agua
    ) %>% 
    filter(
        cod_letra=="C"
    ) %>%
    rename(
        tot_income=v1001,
        tot_sales=v2006, 
        tot_capital_val=v4001, 
        tot_wages=v5180
    ) %>% 
    right_join(
        df_val,
        by=c("id_empresa"="inec_identificador_empresa")
    ) #%>% 
    #head()
