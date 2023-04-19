library(tidyverse)

## Load data ----
source("Code/Ecuador/data_prep.R")
# load("Code/EC.RData")

## Estimate Beta ----
### Beta is the log of the intermediates' share of revenue

### Defining variables

size_vars <- setdiff(firms_vars,
    grep("ii", firms_vars, value=TRUE)
)

output_vars <- c(
    "valor_ventas_total",
    "cantidad_ponderada_ventas_total"
)

size_vars <- c(
    size_vars, 
    output_vars, 
    "valor_compras_total",
    "cantidad_ponderada_compras_total"
)

stru_rel <- c(
    "proartve",
    "matprima",
    "totinsum",
    "prodtota",
    "valor_ventas_total",
    "cantidad_ponderada_ventas_total",
    "valor_compras_total",
    "cantidad_ponderada_compras_total"
)

inds_ec_15 <-
    ec_2015 |>
    group_by(wciiu2d) |>
    mutate(wciiu2d_n = n()) |>
    select(wciiu2d_n, wciiu2d, des_wciiu2d) |>
    arrange(desc(wciiu2d_n)) |>
    unique()

inds_ec <- inds_ec_15 |>
    filter(wciiu2d_n>73) |>
    select(wciiu2d) |>
    pull()

## Loop ----

if (!exists("probs")) {
    probs <- c(0.00,seq(0.80,0.99,by=0.01),0.99)
}
results_list <- list()
i <- 1
for (o in output_vars[1]) {
    for (j in inds_ec) {
        for (s in size_vars) {
            for (p in probs) {
                quant <- ec_2015 |>
                    select(.data[[s]]) |>
                    pull() |>
                    quantile(
                        probs = p,
                        na.rm = TRUE
                    )
                temp1 <- data.frame(Output=o,Industry=j,Size=s,Quantile=p)
                temp2 <- ec_2015 %>%
                    filter(wciiu2d==j, .data[[o]]!=0) %>% 
                    filter(.data[[s]] >= quant[[1]]) %>% 
                    summarise(
                        n=n(),
                        beta=exp(
                            mean(
                                log(
                                    valor_compras_total/.data[[o]]
                                ),
                                na.rm=TRUE
                            )
                        )
                    )
                results_list[[i]] <- cbind(temp1, temp2)
                i <- i+1
            }
        }
    }
}

# for (o in output_vars[1]) {
#     for (j in inds_ec[1]) {
#         for (s in size_vars) {
#             for (p in probs) {
#                 quant <- ec_2015 |>
#                     select(.data[[s]]) |>
#                     pull() |>
#                     quantile(
#                         probs = p,
#                         na.rm = TRUE
#                     )
#                 temp1 <- data.frame(Output=o,Industry="All",Size=s,Quantile=p)
#                 temp2 <- ec_2015 %>%
#                     filter(.data[[o]]!=0) %>% 
#                     filter(.data[[s]] >= quant[[1]]) %>% 
#                     summarise(
#                         n=n(),
#                         beta=exp(
#                             mean(
#                                 log(
#                                     valor_compras_total/.data[[o]]
#                                 ),
#                                 na.rm=TRUE
#                             )
#                         )
#                     )
#                 results_list[[i]] <- cbind(temp1, temp2)
#             }
#         }
#     }
# }

results_df <- do.call(rbind,results_list)


results_df <- results_df %>%
    mutate(
    `Struct. Rel.` = Size %in% stru_rel
    )

## Loop all industries ----
results_list <- list()
i <- 1
for (o in output_vars[1]) {
    for (j in inds_ec[1]) {
        for (s in size_vars) {
            for (p in probs) {
                quant <- ec_2015 |>
                    select(.data[[s]]) |>
                    pull() |>
                    quantile(
                        probs = p,
                        na.rm = TRUE
                    )
                temp1 <- data.frame(Output=o,Industry="All",Size=s,Quantile=p)
                temp2 <- ec_2015 %>%
                    filter(.data[[o]]!=0) %>% 
                    filter(.data[[s]] >= quant[[1]]) %>% 
                    summarise(
                        n=n(),
                        beta=exp(
                            mean(
                                log(
                                    valor_compras_total/.data[[o]]
                                ),
                                na.rm=TRUE
                            )
                        )
                    )
                results_list[[i]] <- cbind(temp1, temp2)
                i <- i+1
            }
        }
    }
}


results_df_all <- do.call(rbind,results_list)

results_df_all <- results_df_all %>%
    mutate(
    `Struct. Rel.` = Size %in% stru_rel
)

## Loop by industry realtive ranking----

if (!exists("probs")) {
    probs <- c(0.00,seq(0.80,0.99,by=0.01),0.99)
}
results_list <- list()
i <- 1
for (o in output_vars[1]) {
    for (j in inds_ec) {
        for (s in size_vars) {
            for (p in probs) {
                quant <- ec_2015 |>
                    filter(wciiu2d==j) |>
                    select(.data[[s]]) |>
                    pull() |>
                    quantile(
                        probs = p,
                        na.rm = TRUE
                    )
                temp1 <- data.frame(Output=o,Industry=j,Size=s,Quantile=p)
                temp2 <- ec_2015 %>%
                    filter(wciiu2d==j, .data[[o]]!=0) %>% 
                    filter(.data[[s]] >= quant[[1]]) %>% 
                    summarise(
                        n=n(),
                        beta=exp(
                            mean(
                                log(
                                    valor_compras_total/.data[[o]]
                                ),
                                na.rm=TRUE
                            )
                        )
                    )
                results_list[[i]] <- cbind(temp1, temp2)
                i <- i+1
            }
        }
    }
}

results_df_rel <- do.call(rbind,results_list)


results_df_rel <- results_df_rel %>%
    mutate(
    `Struct. Rel.` = Size %in% stru_rel
    )


## Cleaning space ----

# keep <- c(
#     grep("df|vars|ind|res",ls(),value=TRUE)
# )
# rem <- setdiff(ls(), keep)
# rm(list = rem)
