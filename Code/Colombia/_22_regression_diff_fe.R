# library(fixest)
# library(tidyverse)
# source("Code/Colombia/00_reading_data.r")
# source("Code/Colombia/10_data_wrangling.r")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/functions.RData")

# get_beta_diff() returns the beta ----
# get_beta_diff <- function(
#     o = output,
#     # j = industry,
#     s = size,
#     p = probs,
#     data = colombia_data_frame) {
#     quant <- data[, s] |>
#         quantile(
#             probs = p,
#             na.rm = TRUE
#         )
#     temp1 <- data.frame(
#         Output = o,
#         # Industry = j,
#         Size = s,
#         Quantile = p,
#         Relative = FALSE
#     )

#     if (p >= 0.5) {
#         data_temp <- data %>%
#             ungroup() %>%
#             mutate(compliers = ifelse(
#                 .data[[s]] >= quant[[1]], 1, 0
#             )) #%>%
#             # filter(sic_3 == j)
#     } else {
#         data_temp <- data %>%
#             ungroup() %>%
#             mutate(compliers = ifelse(
#                 .data[[s]] <= quant[[1]], 1, 0
#             )) #%>%
#             # filter(sic_3 == j)
#     }
#     model <- data_temp %>%
#         fixest::feols(log_share ~ compliers + factor(sic_3), data = .)
#     # print(summary(model))
#     beta_coef <- coef(model)
#     n_obs <- model[["nobs"]]
#     confidence_intervals <- confint(model, level = 0.95)
#     temp2 <- data.frame(
#         beta = exp(beta_coef["compliers"][[1]]) - 1,
#         LCI = exp(confidence_intervals["compliers", "2.5 %"][[1]]) - 1,
#         UCI = exp(confidence_intervals["compliers", "97.5 %"][[1]]) - 1,
#         Obs = n_obs
#     )
#     return(cbind(temp1, temp2))
# }

# Then use expand.grid to get combinations

combinations <- expand.grid(
    # big_industries,
    size,
    c(probs_down, probs_up),
    stringsAsFactors = FALSE
)

# Finally use lapply to run the non-loop

beta_diff_fe_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff(
            o = "sales",
            s = Var1,
            p = Var2,
            # p = Var3,
            data = colombia_data_frame
        )
    )
})

# Collecting results in a DF -------------------

beta_diff_fe <- do.call(rbind, beta_diff_fe_list)

## Ordering the DF -------
beta_diff_fe <- beta_diff_fe[do.call(order, beta_diff_fe), ]

# Saving results ---------------

save(beta_diff_fe, file = "Code/Products/beta_diff_fe.RData")

