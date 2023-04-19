# Wrangling 2015 data ------------------------------

# Output -----------------------------------------

output_data_2015 <-
    data_list[["2015"]][[3]] %>%
    group_by(id_diie) %>%
    mutate(
        valor_ventas = valor_ventas_n + valor_ventas_e,
        cantidad_ventas = cantidad_ventas_n + cantidad_ventas_e,
        # ) %>%
        # mutate(
        across(
            ends_with("ventas"),
            list(sum = sum)
        ),
        w = valor_ventas / valor_ventas_sum,
        cantidad_ponderada_ventas = cantidad_ventas * w
    ) %>%
    summarise(
        across(
            ends_with("ventas"),
            list(
                missing = ~ sum(is.na(.x)),
                total = sum
            )
        )
    )

# Inputs ----

input_data_2015 <-
    data_list[["2015"]][[2]] %>%
    group_by(id_diie) %>%
    mutate(
        valor_compras = valor_n + valor_e,
        cantidad_compras = cantidad_n + cantidad_e,
        across(
            ends_with("compras"),
            list(sum = sum)
        ),
        w = valor_compras / valor_compras_sum,
        cantidad_ponderada_compras = cantidad_compras * w
    ) %>%
    summarise(
        across(
            ends_with("compras"),
            list(
                missing = ~ sum(is.na(.x)),
                total = sum
            )
        )
    )

## Firms in the datasets are not exactly the same.
## There is 56 missing from input missing in output
## but there is only 7 from output missing in input
# setdiff(input_data_2015$id_diie, output_data_2015$id_diie)
# setdiff(output_data_2015$id_diie, input_data_2015$id_diie)

io_data_2015 <- inner_join(output_data_2015, input_data_2015)

# save(
#     list = ls(pattern = "(data|dir|vars|years)"),
#     file = "Code/EC.RData"
# )
# load("Code/EC.RData")

# Firms ----

## Select variables ----------------------------

key_words <- c(
    "capital|activo", # capital
    "(empleados|personal|ocupado|remunerado)", # labor
    "(sueldo|salario|remuneracion)", # wages
    "impuesto", # imputestos
    "materia|insumos", # intermediates
    "producci|valor agregado", # output
    "ciiu|diie" # identifiers
)

selected_variables <- lapply(
    key_words,
    \(x) grep(
        x,
        variable_labs[["2015"]][[1]],
        value = TRUE,
        ignore.case = TRUE 
    )
)

selected <- do.call(c, selected_variables)

## Firms data base -------------------------

firm_data <- data_list[["2015"]][[1]][,names(selected)] 

# Merging all databases --------------------

emm_2015 <- firm_data |> 
    merge(x=_, y=io_data_2015) |> 
    within(
        data =_,
        year <- 2015
    ) #|>
    # head()

# DO NOT RUN --------------------------------
## Capital
variable_labs[["2015"]][[1]] |>
    grep(
        "capital|activo",
        x = _,
        value = TRUE,
        ignore.case = TRUE 
    ) #|>
    # names()

## Labor
variable_labs[["2015"]][[1]] |>
    grep(
        "(empleados|personal|ocupado|remunerado)",
        x = _,
        value = TRUE,
        ignore.case = TRUE 
    ) #|>
    # names()

## Wages
variable_labs[["2015"]][[1]] |>
    grep(
        "(sueldo|salario|remuneracion)",
        x = _,
        value = TRUE,
        ignore.case = TRUE 
    ) #|>
    # names()

## Taxes
variable_labs[["2015"]][[1]] |>
    grep(
        "impuesto",
        x = _,
        value = TRUE,
        ignore.case = TRUE 
    ) #|>
    # names()


## Intermediates
variable_labs[["2015"]][[1]] |>
    grep(
        "materia|insumos",
        x = _,
        value = TRUE,
        ignore.case = TRUE 
    ) #|>
    # names()

## Output
variable_labs[["2015"]][[1]] |>
    grep(
        "producci|valor agregado",
        x = _,
        value = TRUE,
        ignore.case = TRUE 
    ) #|>
    # names()

## identifiers
variable_labs[["2015"]][[1]] |>
    grep(
        "ciiu|diie",
        x = _,
        value = TRUE,
        ignore.case = TRUE 
    ) #|>

## Previous code ----
firms_vars <- c(
        "id_diie",
        "totalpeoc",
        "perremun",
        "suesalar",
        "totremun",
        "proartve",
        "impuesto",
        "imp_ivacob",
        "imp_ivapag",
        "agua",
        "matprima",
        "totinsum",
        "fbkf_1", "fbk",
        "prodtota",
        "wciiu2d",
        "des_wciiu2d",
        "wciiu3d",
        "des_wciiu3d",
        "wciiu4d",
        "des_wciiu4d"
        )

firms_vars %in% names(data_list[["2015"]][[1]])

var_labels <- 
    vars_key[["2015_EMM_Base_de_datos.sav"]][firms_vars]

## Choosing variables for size --------

setdiff(io_data_2015$id_diie, data_list[["2015"]][[1]]$id_diie)
setdiff(data_list[["2015"]][[1]]$id_diie, io_data_2015$id_diie)

firm_data_2015 <- data_list[["2015"]][[1]] 

ec_2015 <- firm_data_2015 %>%
    filter(
        wgrumanu == 2
    ) %>%
    select(
        firms_vars
    ) %>%
        inner_join(io_data_2015)

## Saving data & Cleaning workspace----

# save_v <-c(ec_2015, firms_vars,var_labels,spss_years)

# keep <- c(
#     "ec_2015", 
#     "firms_vars",
#     "var_labels",
#     "spss_years"
# )
# save(
#     list = keep,
#     file = "Code/EC.RData"
# )
# rem <- setdiff(ls(), keep)
# rm(list = rem)

## Sum stats ----

ec_2015 %>% 
    # filter(wciiu2d %in% inds_ec) %>%
    # group_by(wciiu2d) %>%
    summarise(
        `Obs.` = n(),
        across(
            c(valor_ventas_total),
            list(
                Avg = ~ mean(.x, na.rm = TRUE)/1000000, 
                SD = ~sd(.x, na.rm = TRUE)/1000000,
                Missing = ~sum(is.na(.x))
            )
        )
        # `Avg. Sales` = mean(total_valor_ventas, na.rm = TRUE),
        # `Sd. Sales` = sd(total_valor_ventas, na.rm = TRUE)
    )# %>% 
    # left_join(
    #     data.frame(
    #         wciiu2d = inds_ec,
    #         description = inds_des
    #     )
    # ) %>% select(1,9,2:8)

sum_stat_EC <- ec_2015 %>% 
    filter(wciiu2d %in% inds_ec) %>%
    group_by(wciiu2d) %>%
    summarise(
        `Obs.` = n(),
        across(
            c(valor_ventas_total, total_input),
            list(
                Avg = ~ mean(.x, na.rm = TRUE)/1000000, 
                SD = ~sd(.x, na.rm = TRUE)/1000000,
                Missing = ~sum(is.na(.x))
            )
        )
        # `Avg. Sales` = mean(total_valor_ventas, na.rm = TRUE),
        # `Sd. Sales` = sd(total_valor_ventas, na.rm = TRUE)
    ) %>% 
    left_join(
        data.frame(
            cod_ciiu2d = industries,
            description = inds_des
        )
    ) %>% select(1,9,2:8)

save(sum_stat_EC, file = "../data/Results/EC.RData")
