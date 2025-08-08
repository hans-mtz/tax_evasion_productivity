## %% load packages and data %%
library(tidyverse)
load("Code/Products/intermediates.RData")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/boot_test_comp_tbl.RData")

## %% fun %%

test_ev_2means <- function(sic, var, data) {
    # This function perfomrs a one tail t-test for the presence of tax evasion 
    # through input overreporting. 
    # - Under the null hypothesis, there is no tax evasion H_0: cal_V == 0;
    # - Under the alternative hypothesis, there is tax evasion H_a: cal_V > 0;
    # There is no need to trim the data if the probability the firm reported a zero
    # is the same for corporations and non-corporations. 
    # fml <- paste0(var,"~1") |> as.formula()
    tbl <- data %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non_Corp")
        ) %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut),
            # juridical_organization == 3#,
            # ...
        ) %>%
        group_by(treat) %>%
        summarise(
            mean_s = mean(.data[[var]], na.rm = TRUE),
            var_s = var(.data[[var]], na.rm = TRUE),
            n = n()
        ) %>%
        ungroup() %>%
        pivot_wider(
            names_from = treat,
            values_from = c(mean_s, var_s, n)
        ) %>%
        mutate(
            sic_3 = sic,
            intermediates = var,
            mean_diff = mean_s_Non_Corp - mean_s_Corp,
            mean_diff_se = sqrt(
                var_s_Corp/n_Corp + var_s_Non_Corp/n_Non_Corp
            ),
            t_stat = mean_diff/ mean_diff_se,
            rej_rule = t_stat > 1.645,
            p_val = 1 - pnorm(t_stat) |> round(3),
            stars = case_when(
                p_val < 0.01 ~ "***",
                p_val < 0.05 ~ "**",
                p_val < 0.1 ~ "*",
                TRUE ~ ""
            ),
            test_result = ifelse(
                rej_rule >= 1,
                "Reject H_0: mean_s_Non_Corp > mean_s_Corp",
                "Fail to reject H_0: mean_s_Non_Corp <= mean_s_Corp"
            ),
            .before = "mean_s_Corp"
        )

    # log_D <- coefficients(fs_reg)[[1]]
    # epsilon <- residuals(fs_reg)
    # big_E <- -epsilon |> exp() |> mean()
    # beta <- exp(log_D - log(big_E))
    # mean_epsilon <- mean(-epsilon)
    # variance_epsilon <- var(-epsilon)

    return(tbl)
}

test_ev_2means(313, "log_mats_share", colombia_data_frame)

mean_diff_tst_tbl <- lapply(
    top_20_inds$sic_3,
    test_ev_2means,
    var = "log_mats_share",
    data = colombia_data_frame
) |> bind_rows()

tmp1<-mean_diff_tst_tbl %>%
    mutate(
        coeff = glue::glue("{round(mean_diff,2)}{stars}"),
        CI = glue::glue("({round(mean_diff_se,3)})")
    ) %>%
    pivot_longer(

        cols = c(coeff, CI),
        names_to = "statistic",
        values_to = "value"
    ) %>%
    select(
        sic_3, statistic, value
    ) %>%
    arrange(sic_3)

tests_tbl<- tmp1 %>%
    left_join(
        test_comp_tbl,
        by = c("sic_3", "statistic" = "type")
    )

## %% Save results %% 

save(
    tests_tbl,
    file = "Code/Products/tests_tbl.RData"
)

## %% Running regressions with my eyes %%

r_tbl_1 <- colombia_data_frame %>% 
    mutate(
        treat = ifelse(juridical_organization == 3, "Corp", "Non_Corp")
    ) %>%
    filter(
        # sic_3 == sic,
        sic_3 %in% top_20_inds$sic_3,
        is.finite(log_mats_share),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        is.finite(y),
        log_mats_share > log(threshold_cut)
    ) %>%
    fixest::feols(
        log_mats_share ~ treat*factor(sic_3) | 
            sic_3 + year ,
        data = ., cluster = c("sic_3","year")) 


r_tbl_2 <- colombia_data_frame %>% 
    mutate(
        treat = ifelse(juridical_organization == 3, "Corp", "Non_Corp")
    ) %>%
    filter(
        # sic_3 == sic,
        sic_3 %in% top_20_inds$sic_3,
        is.finite(log_mats_share),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        is.finite(y),
        log_mats_share > log(threshold_cut)
    ) %>%
    fixest::feols(
        log_mats_share ~ treat*factor(sic_3) + log(sales_taxes)| 
            sic_3 + year ,
        data = ., cluster = c("sic_3","year")) 

fixest::etable(r_tbl_1, r_tbl_2,
            #    tex = TRUE, 
            #    file = "Code/Products/regression_results.tex",
               title = "Regression Results for the Effect of Tax Evasion on Input Overreporting",
            #    coef.names = c("Intercept", "Corp", "Non_Corp", "SIC 313", "SIC 314", "SIC 315", "SIC 316", "SIC 317", "SIC 318", "SIC 319"),
            #    dep.var.labels = c("Materials Share"),
               cluster = c("sic_3","year"),
               digits = 3)
