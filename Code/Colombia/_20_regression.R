# library("fixest")
library(tidyverse)
# source("Code/Colombia/00_reading_data.r")
# source("Code/Colombia/10_data_wrangling.r")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

## Relevant variables ----

# size <- colombia_data_frame %>%
#     ungroup() %>%
#     select(
#         !c(plant,year, sic_3, p_gdp)
#     ) %>% names()

# output <- c(
#     "gross_output", #real value of gross production
#     "sales" #real sales
#     # "va" #value added
# )
# input <- c(
#     "materials",
#     "intermediate_inputs",
#     "mats_serv"
# )


## Regressing, using fixest ----

# cdf %>%
#     filter( sales != 0, intermediate_inputs != 0 , is.na(sales)==FALSE) %>%
#     # summarise(beta=mean(log(intermediate_inputs/sales), na.rm = TRUE))
#     lm(log((intermediate_inputs/sales))~ as.factor(sic_3), .) %>%
#     confint(., level = 0.95)
#     summarise( beta = exp(coef(.)[1]), )

# # select necessary data and fit the model
# model <- cdf %>%
#   filter(sales != 0, intermediate_inputs != 0, !is.na(sales)) %>%
#   lm(log((intermediate_inputs/sales)) ~ sic_3, .)

# # get the coefficients
# coef <- coef(model)
# exp(coef[[1]])

# # get the confidence intervals
# conf_int <- confint(model, level = 0.95)

# # display the confidence intervals
# conf_int

## Loop ---
# if (!exists("probs")) {
#     probs <- c(0.00,seq(0.80,0.99,by=0.01),0.99)
# }
results <- list()
i <- 1
for (o in output[2]) {
    for (s in size) {
        for (p in probs) {
            quant <- quantile(
                colombia_data_frame[,s],
                probs = p,
                na.rm = TRUE
            )
            temp1 <- data.frame(Output=o, Size=s, Quantile=p)
            data_temp <- colombia_data_frame %>%
                filter(.data[[o]] != 0, !is.na(.data[[o]])) %>% 
                filter(.data[[s]] >= quant[[1]])
            model <- lm(log_share ~ 1, data_temp)
            coefs <- coef(model)
            confidence_intervals <- confint(model, level = 0.95)
            temp2 <- data.frame(
                beta = exp(coefs[[1]]),
                LCI = exp(confidence_intervals[1, 1][[1]]),
                UCI = exp(confidence_intervals[1, 2][[1]])
            )

            results[[i]] <- cbind(temp1, temp2)
            i <- i+1
        }
    }
}

# Collecting results in data.frame -----

beta_all <- do.call(rbind, results)

beta_all <- data.frame(
    Size = size,
    `Structurally related` = case_when(
        grepl("labor|lag|capital|wages", size) ~ "Unrelated",
        .default = "Related"
    )
) %>% right_join(beta_all)

# Saving results -------
save(beta_all, file = "Code/Products/beta_all.RData")