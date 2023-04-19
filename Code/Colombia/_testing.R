# Testing ------------------------------------------

quant <- colombia_data_frame[, size[1]] |>
    quantile(
    probs = probs,
    na.rm = TRUE
)


colombia_data_frame %>%
    ungroup() %>%
    mutate(
        sic_3_factor = relevel(as.factor(sic_3), ref = 311),
        up = ifelse(
            gross_output >= quant[[1]], 1, 0
        ),
        compliers = ifelse(p>=0.5, up, !up)
    ) %>%
    select(sic_3_factor)

    filter(
        .data[[o]]!=0, 
        !is.na(.data[[o]]),
        .data[[s]] >= quant[p]
    )

naive_beta <- results_df %>%
    filter(
        Industry == big_industries[[1]],
        Quantile == 0.0,
        Size == "labor_employee_years"
    ) %>% select(beta, LCI, UCI) %>%
    unlist()

library(googlesheets4)
library(knitr)
conferences <- read_sheet("https://docs.google.com/spreadsheets/d/17CK2WB_WeeGC0DGPQttrUA5Y5anvTu6U_kM706Ihrqo/edit?usp=sharing")
read_sheet("https://docs.google.com/spreadsheets/d/17CK2WB_WeeGC0DGPQttrUA5Y5anvTu6U_kM706Ihrqo/edit#gid=1250691441")
kable(conferences[-1,1:5])

# Regressing - difference -----

library(fixest)
probs_up <- c(0.00, seq(0.80, 0.99, by = 0.01), 0.99)
probs_down <- seq(0.01, 0.20, by = 0.01)

s <- size[[1]]
quant_up <- colombia_data_frame[, s] |>
    quantile(
        probs = probs_up,
        na.rm = TRUE
    )
quant_down <- colombia_data_frame[, s] |>
    quantile(
        probs = probs_down,
        na.rm = TRUE
    )

model <- colombia_data_frame |>
    within(
        data = _, {
        compliers <- ifelse(gross_output <= quant_down[["20%"]], 1, 0)
        # log_share <- log(intermediate_inputs / sales)
        }
    )|>
    feols(log_share ~ compliers, data = _) #|>
    # confint()|> exp()
## Regressing on a dummy to get the difference -------
coefs <- c(
    colombia_data_frame |>
    within(
        data = _, {
        compliers <- ifelse(gross_output <= quant_down[["20%"]], 1, 0)
        log_share <- log(intermediate_inputs / sales)
        }
    )|>
    feols(log_share ~ compliers, data = _) |>
    coef() |> exp() ,

## all ----------------------------------------------
    colombia_data_frame |>
        within(
            data = _, {
            compliers <- ifelse(gross_output <= quant_down[["20%"]], 1, 0)
            log_share <- log(intermediate_inputs / sales)
            }
        )|>
        feols(log_share ~ 1, data = _) |>
        coef() |> exp() ,

## Compliers ----------------------------------------------
    colombia_data_frame |>
        within(
            data = _, {
            # compliers <- ifelse(gross_output <= quant_down[["20%"]], 1, 0)
            log_share <- log(intermediate_inputs / sales)
            }
        )|>
        subset(gross_output <= quant_down[["20%"]]) |>
        feols(log_share ~ 1, data = _) |>
        coef() |> exp()
)

data.frame(
    Size = size,
    `Structurally related` = case_when(
        grepl("labor|lag|capital|wages", size) ~ "Unrelated",
        .default = "Related"
    )
)

## 
# Regressing - difference -----

library(fixest)
probs_up <- c(0.00, seq(0.80, 0.99, by = 0.01), 0.99)
probs_down <- seq(0.01, 0.20, by = 0.01)

s <- size[[1]]
quant_up <- colombia_data_frame[, s] |>
    quantile(
        probs = probs_up,
        na.rm = TRUE
    )

colombia_data_frame %>%
    ungroup() %>%
    mutate(
        sic_3_factor = relevel(as.factor(sic_3), ref = "311"),
        other_inds = ifelse(sic_3=="311", 0, 1),
        inds = ifelse(sic_3=="311", 1, 0),
        up = ifelse(
            labor_employee_years>= quant_up[[2]], 1, 0
        ),
        down = ifelse(
            labor_employee_years<= quant_up[[2]], 1, 0
        ),
        compliers = ifelse(probs_up[[2]]>=0.5, up, 1-up)
    ) %>% 
    filter(
        # .data[[o]] != 0,
        # !is.na(sales),
        !is.na(log_share)
    # .data[[s]] >= quant[[p]]
    ) %>%#select(sic_3_factor, other_inds, up, down, compliers)
    # filter(sic_3=="311") %>%
    feols(log_share ~ up*inds, data=.) %>%
    coef() %>%
    `[[`("up:inds")
## testing

get_beta_inds(output[[1]],big_industries[[1]],size[[1]],0.2)

## diff-diff

