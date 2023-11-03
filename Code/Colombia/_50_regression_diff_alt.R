library(fixest)
library(tidyverse)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

# Vars --------------------------------

share <- seq(0.5, 0.9, by = 0.01)

# get_beta_diff() returns the beta ----
get_beta_diff_share <- function(
    o = output,
    s = size,
    p = share,
    data = colombia_data_frame) {

    temp1 <- data.frame(
        Output = o,
        Size = s,
        Share = p
    )

    model <- data %>%
        mutate(
            compliers = ifelse(
                .data[[s]] >= p, 1, 0
            )
        ) %>%
        fixest::feols(log_share ~ compliers + factor(sic_3), data = .)

    beta_coef <- coef(model)
    n_obs <- model[["nobs"]]
    confidence_intervals <- confint(model, level = 0.95)
    temp2 <- data.frame(
        beta = exp(beta_coef["compliers"][[1]]) - 1,
        LCI = exp(confidence_intervals["compliers", "2.5 %"][[1]]) - 1,
        UCI = exp(confidence_intervals["compliers", "97.5 %"][[1]]) - 1,
        Obs = n_obs
    )
    return(cbind(temp1, temp2))
}

# Then use expand.grid to get combinations

combinations <- expand.grid(
    size,
    share,
    stringsAsFactors = FALSE
)

# Finally use lapply to run the non-loop

beta_diff_share_fe_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff_share(
            o = "sales",
            s = Var1,
            p = Var2,
            data = colombia_data_frame
        )
    )
})

# Collecting results in a DF -------------------

beta_diff_share_fe <- do.call(rbind, beta_diff_share_fe_list)

## Ordering the DF -------
beta_diff_share_fe <- beta_diff_share_fe[do.call(order, beta_diff_share_fe), ]

# Saving results ---------------

save(beta_diff_share_fe, file = "Code/Products/beta_diff_share_fe.RData")

