library(fixest)
library(tidyverse)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

# get_beta_diff() returns the beta ----
get_beta_diff_poly <- function(
    o = output,
    s = size,
    p = probs,
    d = 2,
    data = colombia_data_frame) {
    quant <- data[, s] |>
        quantile(
            probs = p,
            na.rm = TRUE
        )
    temp1 <- data.frame(
        Output = o,
        # Industry = j,
        Size = s,
        Quantile = p,
        Relative = FALSE
    )

    if (p >= 0.5) {
        data_temp <- data %>%
            ungroup() %>%
            mutate(compliers = ifelse(
                .data[[s]] >= quant[[1]], 1, 0
            ))
    } else {
        data_temp <- data %>%
            ungroup() %>%
            mutate(compliers = ifelse(
                .data[[s]] <= quant[[1]], 1, 0
            ))
    }
    model <- data_temp %>%
        mutate(
            m = log(intermediate_inputs),
            k = log(capital),
            l = log(labor_employee_years)
        ) %>%
        fixest::feols(
            log_share ~ compliers + polym(m, k, l, degree = d, raw = TRUE) + factor(sic_3),
            data = .
        )
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
    c(probs_down, probs_up),
    stringsAsFactors = FALSE
)

# Finally use lapply to run the non-loop

beta_diff_poly_fe_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff_poly(
            o = "sales",
            s = Var1,
            p = Var2,
            data = colombia_data_frame
        )
    )
})

# Collecting results in a DF -------------------

beta_diff_poly_fe <- do.call(rbind, beta_diff_poly_fe_list)

## Ordering the DF -------
beta_diff_poly_fe <- beta_diff_poly_fe[do.call(order, beta_diff_poly_fe), ]

# Saving results ---------------

save(
    beta_diff_poly_fe,
    file = paste0(products_dir, "beta_diff_poly_fe.RData")
)
