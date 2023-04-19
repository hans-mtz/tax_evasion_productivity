# library(foreign)
# library(tidyverse)
# library(todor)


## Reading Firm's output ----

inputs_file <- "../data/Ecuador/2019/Materia primas/2019_ENESEM_BDD_MATERIAS_PRIMAS.sav"
input_raw <- read.spss(inputs_file, to.data.frame = TRUE)
input_raw$inec_identificador_empresa <- as.numeric(
    input_raw$inec_identificador_empresa
)

## Exploring data ----

# input_raw   %>% 
#     filter(uni_med!=99,cantidad_impor==0,cantidad_local==0)
# 1-5474/6287 = 13% of observations no unit
# TODO: Check later if this matters

# input  <-  input_raw   %>% filter(uni_med!=99)  %>% 
#     group_by(inec_identificador_empresa)  %>%
#     mutate(
#         rho_loc_ijt = ifelse(cantidad_local==0,0,valor_local/cantidad_local),
#         rho_imp_ijt = ifelse(cantidad_impor==0,0,valor_impor/cantidad_impor),
#         total_valor_loc=sum(valor_local),
#         wgt_loc = ifelse(total_valor_loc ==0,0,valor_local/total_valor_loc),
#         total_valor_imp=sum(valor_impor),
#         wgt_imp = ifelse(total_valor_imp ==0,0,valor_impor/total_valor_imp)
#         )  %>% 
#     summarise(
#         total_valor = sum(valor_local+valor_impor),
#         wgt = sum(valor_local)/total_valor,
#         rho_loc=sum(rho_loc_ijt*wgt_loc),
#         rho_imp=sum(rho_imp_ijt*wgt_imp),
#         rho_it=sum(wgt*rho_loc+(1-wgt)*rho_imp),
#         input_it=sum(valor_local+valor_impor)/rho_it
# )

# input$inec_identificador_empresa  <- as.numeric(input$inec_identificador_empresa)

## Joining ouput and input df's ----

# left_join(input,output)

## Input rho X ----

input_rhox <- input_raw  %>% 
    group_by(inec_identificador_empresa)  %>% 
    summarise(
        total_input_local=sum(valor_local),
        total_input_impor=sum(valor_impor),
        total_input=sum(valor_local+valor_impor)
)

df_val <- left_join(input_rhox,output_pq)

