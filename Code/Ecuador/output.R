# library(foreign)
# library(tidyverse)
# library(todor)


## Reading Firm's output ----

output_raw <- 
    read.spss(
        "../data/Ecuador/2019/Productos/2019_ENESEM_BBD_PRODUCTOS.sav",
        to.data.frame = TRUE
)

# output_vars <- names(output)

# View(output_vars)
# summary(output)

## Getting prices ----

# output_raw$price_ijt <- output_raw$valor_prod/output_raw$cantidad_prod

## Checking quantity produced variable ----

# output_raw  %>% select(inec_identificador_empresa, ciiu3, 
#                 ciiu3_desc, price_ijt, valor_prod, cantidad_prod,
#                 starts_with(c("valor","cantidad")))  %>% 
#     filter(cantidad_prod<=0)

# 291 of 4601 obs do not include quantities
# TODO Check the missing observations on quantities are not biasing results

## Industries ----

# inds_raw <- output_raw  %>% select(
#                 ciiu3,ciiu3_desc, ciiu_seccion, ciiu_seccion_desc)  %>% 
#             unique(.)

# inds_n <- output_raw  %>% select(
#                 ciiu3,ciiu3_desc, ciiu_seccion, ciiu_seccion_desc)  %>%
#                 group_by(ciiu3)  %>% 
#                 summarise(n = n())

# inds <- left_join(inds_n,inds_raw)

## Output df | Quantities ----

# output_raw  %>% filter(uni_med!=99, cantidad_prod!=0)  %>% 
#                 group_by(inec_identificador_empresa,ciiu3)  %>%
#                 mutate(price_ijt = valor_prod/cantidad_prod,
#                         total_valor_prod=sum(valor_prod),
#                         wgt = valor_prod/total_valor_prod) %>% 
#                 summarise(price_it=sum(price_ijt*wgt),
#                             total_v=min(total_valor_prod),
#                             output_it=total_v/price_it) %>% 
#                 select(inec_identificador_empresa,ciiu3,
#                         price_it,
#                         total_v,
#                         output_it)

# output <-  output_raw  %>% 
#     filter(uni_med!=99,cantidad_prod!=0)  %>% 
#     group_by(inec_identificador_empresa,ciiu3)  %>% 
#     mutate(total_valor_prod=sum(valor_prod),
#             wgt = valor_prod/total_valor_prod)  %>% 
#     summarise(price_it=sum(price_ijt*wgt),
#             output_it=min(total_valor_prod)/price_it)

## Output df | PQ ----

output_pq <-  output_raw  %>%
    group_by(inec_identificador_empresa)  %>% 
    summarise(total_valor_prod=sum(valor_prod),
        total_valor_ventas_n=sum(valor_ventas_n),
        total_valor_ventas_e=sum(valor_ventas_e),
        total_valor_ventas = sum(valor_ventas_n+valor_ventas_e))
