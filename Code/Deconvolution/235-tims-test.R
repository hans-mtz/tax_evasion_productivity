## %% Load libraries and data %% -----------------------
library(tidyverse)
library(parallel)
# load("Code/Products/fs.RData")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")



## %% Tim's test -----------------------------


estimate_fs_eps_epdf <- function(sic, var, r_var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    corp_data <- data %>%
        filter(
            sic_3 == sic,
            juridical_organization == 3,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        )
    fs_reg <- lm(fml, data = corp_data) # %>%
    # fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- 1
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)
    # eps_density <- density(-epsilon, n = 1000)
    eps_density <- -epsilon |> na.omit() |> density(bw="SJ-dpi")
  
    # Create a function for the PDF
    eps_pdf <- approxfun(eps_density$x, eps_density$y, yleft = 0, yright = 0)
    eps_cdf < ecdf(-epsilon)


    corp_data$epsilon <- -epsilon

    ## Deconvolution ------------------------

    tbl <- data %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        left_join(
            corp_data %>% select(plant, year, epsilon),
            by = c("plant", "year")
        ) %>%
        select(!m) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[var]] - log_D,
            m  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(m-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, juridical_organization, cal_V, cal_W, m, k, l, y, epsilon
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        epsilon_pdf = eps_pdf,
        beta = beta,
        big_E = big_E,
        sic_3 = sic,
        inter = r_var
    )
    return(result_list)
}

estimate_fs_eps_epdf(322, "log_mats_share", "materials", colombia_data_frame)$epsilon_pdf(0)

fs_eps_pdf_list <- mclapply(
    top_20_inds$sic_3,
    estimate_fs_eps_epdf,
    var = "log_mats_share",
    r_var = "materials",
    data = colombia_data_frame,
    mc.cores = detectCores() - 2
)

names(fs_eps_pdf_list) <- top_20_inds$sic_3 |> paste0()

fs_eps_pdf_list[["311"]]$epsilon_pdf(fs_eps_pdf_list[["311"]]$data$cal_V)

fs_eps_pdf_list[["311"]]$data$epsilon |> ecdf()

eps_cdf_list <- lapply(
    names(fs_eps_pdf_list),
    \(x) {
        -fs_eps_pdf_list[[x]]$data$epsilon |> ecdf()
    }
)

names(eps_cdf_list) <- names(fs_eps_pdf_list)

eps_cdf_list[["311"]](fs_eps_pdf_list[["311"]]$data$cal_V)
eps_cdf_list[["311"]] |> quantile(c(0.025,0.975))
eps_cdf_list[["311"]] |> quantile(0.95) |> str()
fs_eps_pdf_list[["322"]]$data %>%
    mutate(
        corp = ifelse(
            juridical_organization == 3,
            "Corp",
            "Non-Corp"
        )
    ) %>%
    # filter(corp == "Non-Corp") %>%
    group_by(corp) %>%
    summarise(
        rej_freq = mean(
            eps_cdf_list[["322"]](cal_V) > 0.95,# | eps_cdf_list[["312"]](cal_V) > 0.025,
            na.rm = TRUE
        ),
        mean_test = eps_cdf_list[["322"]](mean(cal_V, na.rm = TRUE)) > 0.95,
        mean_p_val = 1 - eps_cdf_list[["322"]](mean(cal_V, na.rm = TRUE)) |> round(3),
    )


tims_tst_tbl <- lapply(
    names(fs_eps_pdf_list),
    \(x) {
        fs_eps_pdf_list[[x]]$data %>%
        mutate(
            corp = ifelse(
                juridical_organization == 3,
                "Corp",
                "Non-Corp"
            )
        ) %>%
        # filter(corp == "Non-Corp") %>%
        group_by(corp) %>%
        summarise(
            sic_3 = x,
            rej_freq_5p_1t = mean(
                eps_cdf_list[[x]](cal_V) > 0.95,# | eps_cdf_list[["312"]](cal_V) > 0.025,
                na.rm = TRUE
            )*100 |> round(1),
            rej_freq_5p_2t = mean(
                eps_cdf_list[[x]](cal_V) > 0.975 | eps_cdf_list[[x]](cal_V) < 0.025 ,# | eps_cdf_list[["312"]](cal_V) > 0.025,
                na.rm = TRUE
            )*100 |> round(1),
            # avg_p_val = min(1 - eps_cdf_list[[x]](cal_V), eps_cdf_list[[x]](cal_V)) |> mean() |> round(3),
            # stars = case_when(
            #     rej_freq < 0.01 ~ "***",
            #     rej_freq < 0.05 ~ "**",
            #     rej_freq < 0.1 ~ "*",
            #     TRUE ~ ""
            # ),

        )
    }
) %>% 
bind_rows() %>%
mutate(
    across(where(is.numeric), ~ round(.x, 1)),
) %>%
pivot_wider(
    names_from = corp,
    values_from = c(rej_freq_5p_1t, rej_freq_5p_2t),
    names_sep = "_"
)

tims_tst_tbl |> View()

## %% Saving variables ---------------------
save(
    fs_eps_pdf_list, eps_cdf_list, tims_tst_tbl,
    file = "Code/Products/tims-test.RData"
)
