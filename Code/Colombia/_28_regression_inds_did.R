library(fixest)
library(tidyverse)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

# get_beta_inds() returns the beta ----
get_did_inds <- function(
    o = output,
    j = industry,
    s = size,
    p = probs,
    data = colombia_data_frame) {
    quant <- data[, s] |>
        quantile(
            probs = p,
            na.rm = TRUE
        )
    temp1 <- data.frame(
        Output = o,
        Industry = j,
        Size = s,
        Quantile = p,
        Relative = FALSE
    )

    if (p >= 0.5) {
        data_temp <- data %>%
            ungroup() %>%
            mutate(compliers = ifelse(
                .data[[s]] >= quant[[1]], 1, 0
            )) #%>%
            # filter(sic_3 == j)
    } else {
        data_temp <- data %>%
            ungroup() %>%
            mutate(compliers = ifelse(
                .data[[s]] <= quant[[1]], 1, 0
            )) #%>%
            # filter(sic_3 == j)
    }
    model <- data_temp %>%
        mutate( inds = ifelse(
            sic_3 == j, 1, 0
        )) %>%
        fixest::feols(log_share ~ compliers*inds, data = .)
    beta_coef <- coef(model)
    n_obs <- model[["nobs"]]
    confidence_intervals <- confint(model, level = 0.95)
    temp2 <- data.frame(
        beta = exp(beta_coef["compliers:inds"][[1]]) - 1,
        LCI = exp(confidence_intervals["compliers:inds", "2.5 %"][[1]]) - 1,
        UCI = exp(confidence_intervals["compliers:inds", "97.5 %"][[1]]) - 1,
        Obs = n_obs
    )
    return(cbind(temp1, temp2))
}

# Then use expand.grid to get combinations

combinations <- expand.grid(
    big_industries,
    size,
    c(probs_down, probs_up),
    stringsAsFactors = FALSE
)

# Finally use lapply to run the non-loop

beta_inds_did_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_did_inds(
            o = "sales",
            j = Var1,
            s = Var2,
            p = Var3,
            data = colombia_data_frame
        )
    )
})

# Collecting results in a DF -------------------

beta_inds_did <- do.call(rbind, beta_inds_did_list)

## Ordering the DF -------
beta_inds_did <- beta_inds_did[do.call(order, beta_inds_did), ]

# Saving results ---------------

save(
    beta_inds_did,
    file = paste0(products_dir, "beta_inds_did.RData")
)
