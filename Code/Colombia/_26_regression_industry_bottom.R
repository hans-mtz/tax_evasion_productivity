# # library("fixest")
# source("Code/Colombia/00_reading_data.r")
# source("Code/Colombia/10_data_wrangling.r")
# source("Code/Colombia/25_regression_industry.r")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

# ## Relevant variables ----

# size <- colombia_data_frame %>%
#     ungroup() %>%
#     select(
#         !c(plant, year, sic_3, p_gdp)
#     ) %>%
#     names()

# output <- c(
#     "gross_output", # real value of gross production
#     "sales" # real sales
#     # "va" #value added
# )
# input <- c(
#     "materials",
#     "intermediate_inputs",
#     "mats_serv"
# )

# # Colombian big industries ----

# industries <- colombia_data_frame %>%
#     group_by(sic_3) %>%
#     summarise(n_sic = n()) %>%
#     arrange(desc(n_sic)) %>%
#     mutate(
#         n_cum = cumsum(n_sic),
#         perc = n_sic / sum(n_sic) * 100,
#         perc_acc = n_cum / sum(n_sic) * 100,
#     ) %>%
#     select(sic_3, n_sic, perc, perc_acc) %>%
#     unique()

# big_industries <- industries %>%
#     filter(n_sic >= 3500) %>%
#     select(sic_3) %>%
#     pull()

## Loop ----

# probs_down <- seq(0.01, 0.20, by = 0.01)
results_bottom <- list()
i <- 1
for (o in output[2]) {
    for (j in big_industries) {
        for (s in size) {
            # 1) Absolute size: First, ranking by size;
            # then, filtering by industry
            quant <- colombia_data_frame[, s] |>
                quantile(
                    probs = probs_down,
                    na.rm = TRUE
                )
            for (p in rev(seq_along(probs_down))) {
                temp1 <- data.frame(
                    Output = o,
                    Industry = j,
                    Size = s,
                    Quantile = probs_down[[p]],
                    Relative = FALSE
                )
                data_temp <- colombia_data_frame %>%
                    ungroup() %>%
                    mutate(
                        sic_3_factor = relevel(
                            as.factor(sic_3),
                            ref = as.character(j)
                        )
                    ) %>%
                    filter(
                        .data[[o]] != 0,
                        !is.na(.data[[o]]),
                        .data[[s]] <= quant[[p]]
                    )
                model <- lm(
                    log_share ~ sic_3_factor,
                    data_temp
                )
                coefs <- coef(model)
                confidence_intervals <- confint(model, level = 0.95)
                temp2 <- data.frame(
                    beta = exp(coefs[[1]]),
                    LCI = exp(confidence_intervals[1, 1][[1]]),
                    UCI = exp(confidence_intervals[1, 2][[1]])
                )
                results_bottom[[i]] <- cbind(temp1, temp2)
                i <- i + 1
                # print(i)
            }
        }
    }
}

## Collecting results in data.frame

beta_inds_bottom <- do.call(rbind, results_bottom)

beta_inds_bottom <- data.frame(
    Size = size,
    `Structurally related` = case_when(
        grepl("labor|lag|capital|wages", size) ~ "Unrelated",
        .default = "Related"
    )
) %>% right_join(beta_inds_bottom, multiple = "all")

# Merging top and bottom -----

beta_inds <- rbind(beta_inds_top, beta_inds_bottom)

# Saving results -------
save(beta_inds, file = "Code/Products/beta_inds.RData")
