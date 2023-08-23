library(fixest)
library(tidyverse)
library(ggplot2)

# Declaring functions -----------------

## get_beta_diff() --------------
# returns the difference between the non-evaders' and evaders
# beta. For all firms using fixed effects by industry
# and a CD production function

get_beta_diff <- function(
    o = output,
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
        Size = s,
        Quantile = p,
        Relative = FALSE
    )

    if (p >= 0.5) {
        data_temp <- data %>%
            ungroup() %>%
            mutate(
                compliers = ifelse(
                    .data[[s]] >= quant[[1]], 1, 0
                )
            )
    } else {
        data_temp <- data %>%
            ungroup() %>%
            mutate(
                compliers = ifelse(
                    .data[[s]] <= quant[[1]], 1, 0
                )
            )
    }
    model <- data_temp %>%
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

## get_beta_diff_poly()
# Returns the difference between the non-evaders' and evaders
# beta. For all firms using fixed effects by industry
# and a Non-linear production function
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

## get_beta_diff_X_yoy -------------------------------
# get_beta_diff_yoy()
# returns the difference between the non-evaders' and evaders
# beta. For all firms using fixed effects by industry
# and a CD production function.
# Estimates quantile by year, not overall

get_beta_diff_yoy <- function(
    o = output,
    s = size,
    p = probs,
    data = colombia_data_frame) {
    # quant <- data[, s] |>
    #     quantile(
    #         probs = p,
    #         na.rm = TRUE
    #     )
    temp1 <- data.frame(
        Output = o,
        Size = s,
        Quantile = p,
        Relative = FALSE
    )
    data_temp <- data %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(
            quant = quantile(
                .data[[s]],
                probs = p,
                na.rm = TRUE
            )[[1]]
        )
    if (p >= 0.5) {
        data_temp <- data_temp %>%
            ungroup() %>%
            mutate(
                compliers = ifelse(
                    .data[[s]] >= quant, 1, 0
                )
            )
    } else {
        data_temp <- data_temp %>%
            ungroup() %>%
            mutate(
                compliers = ifelse(
                    .data[[s]] <= quant, 1, 0
                )
            )
    }
    model <- data_temp %>%
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

## get_beta_diff_poly_yoy()
# Returns the difference between the non-evaders' and evaders
# beta. For all firms using fixed effects by industry
# and a Non-linear production function
# Estimates quantile by year, not overall

get_beta_diff_poly_yoy <- function(
    o = output,
    s = size,
    p = probs,
    d = 2,
    data = colombia_data_frame) {
    # quant <- data[, s] |>
    #     quantile(
    #         probs = p,
    #         na.rm = TRUE
    #     )
    temp1 <- data.frame(
        Output = o,
        Size = s,
        Quantile = p,
        Relative = FALSE
    )
    data_temp <- data %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(
            quant = quantile(
                .data[[s]],
                probs = p,
                na.rm = TRUE
            )[[1]]
        )
    if (p >= 0.5) {
        data_temp <- data_temp %>%
            ungroup() %>%
            mutate(
                compliers = ifelse(
                    .data[[s]] >= quant, 1, 0
                )
            )
    } else {
        data_temp <- data_temp %>%
            ungroup() %>%
            mutate(
                compliers = ifelse(
                    .data[[s]] <= quant, 1, 0
                )
            )
    }
    model <- data_temp %>%
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

# Collecting results in data.frame --------

collect_in_df <- function(results_list) {
    # Collecting results in a DF
    results_df <- do.call(rbind, results_list)
    # Ordering the DF
    results_df <- results_df[do.call(order, results_df), ]
    return(results_df)
}

## Plot function ----------------------

plot_my_lollypop <- function(
    data = results,
    plot_title = "All firms, industry FE, annual quantile",
    size_description = size_desc,
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "yoy",
        plot_suffix = "png"
    )) {
    # Local vars -----------

    # local_data <- "beta_diff_fe"
    # plot_title <- "All firms industry FE\nColombia"
    # plot_suffix <- "png" # svg, pdf

    # Load data ------------

    # load(paste0(products_dir, local_data, ".RData"))
    # load("Code/Products/beta_diff_poly.RData")

    # Get labels for plot -----------------------
    xlabs <- data.frame(
        q = data[["Quantile"]] |>
            unique() |>
            sort() |> as.numeric()
    ) %>%
        mutate(
            xlabs = case_when(
                q == 0.01 ~ "1%",
                q == 0.09 ~ "9%",
                q == 0.19 ~ "19%",
                q == 0.8 ~ "80%",
                q == 0.9 ~ "90%",
                q == 0.99 ~ "99%",
                .default = ""
            )
        ) %>%
        pull(xlabs)

    # Plotting ---------------
    my_plot <- data %>%
        mutate(
            Quantile_factor = factor(
                Quantile,
                sort(
                    unique(Quantile)
                )
            ),
            tail = case_when(
                Quantile >= 0.5 ~ "Top",
                TRUE ~ "Bottom"
            ),
            Complier = paste(tail, PF)
        ) %>%
        ggplot() +
        geom_segment(
            aes(
                x = Quantile_factor,
                xend = Quantile_factor,
                y = LCI,
                yend = UCI
            ),
            color = my_colors[["gray"]]
        ) +
        geom_point(
            aes(
                x = Quantile_factor,
                y = beta,
                color = PF,
            ),
            size = 2
        ) +
        geom_hline(
            yintercept = 0.0,
            color = my_colors[["gray"]],
            linewidth = 1
        ) +
        scale_x_discrete(labels = xlabs) +
        coord_flip() +
        theme_classic() +
        theme(
            legend.position = "none",
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            axis.ticks.y = element_blank()
        ) +
        ggtitle(
            plot_title
        ) +
        labs(y = "", x = "Quantile") +
        facet_wrap(
            ~Size,
            ncol = 1,
            scale = "free_y",
            labeller = as_labeller(size_description)
        )
    print(my_plot)
    ggsave(
        paste0(
            save_list[["fig_dir"]],
            save_list[["ex_name"]],
            "_",
            save_list[["local_data"]],
            ".",
            save_list[["plot_suffix"]]
        ),
        width = 21,
        height = 32,
        units = "cm"
    )
}

plot_lollypop <- function(
    data = results,
    plot_title = "All firms, industry FE, annual quantile",
    size_description = size_desc,
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "yoy",
        plot_suffix = "png"
    )) {
    # Local vars -----------

    # local_data <- "beta_diff_fe"
    # plot_title <- "All firms industry FE\nColombia"
    # plot_suffix <- "png" # svg, pdf

    # Load data ------------

    # load(paste0(products_dir, local_data, ".RData"))
    # load("Code/Products/beta_diff_poly.RData")
    # data <- data %>%
    #     arrange(beta,desc(PF)) %>%

    # Get labels for plot -----------------------
    x_lev_labs <- data %>%
        arrange(beta, desc(PF)) %>%
        mutate(
            Desc = size_description[Size],
            xlabs = paste0(Desc, " - ", Quantile * 100, " %")
        ) %>%
        pull(xlabs) %>%
        unique()

    # Plotting ---------------
    my_plot <- data %>%
        # arrange(beta, desc(PF)) %>%
        mutate(
            #     Quantile_factor = factor(
            #         Quantile,
            #         sort(
            #             unique(Quantile)
            #         )
            #     ),
            #     tail = case_when(
            #         Quantile >= 0.5 ~ "Top",
            #         TRUE ~ "Bottom"
            #     ),
            #     Complier = paste(tail, PF),
            #     # Size = factor(Size)
            Desc = size_description[Size],
            xlabs = paste0(Desc, " - ", Quantile * 100, " %"),
            x_labs_f = factor(xlabs, x_lev_labs)
        ) %>%
        ggplot() +
        geom_segment(
            aes(
                x = x_labs_f,
                xend = x_labs_f,
                y = LCI,
                yend = UCI
            ),
            color = my_colors[["gray"]]
        ) +
        geom_point(
            aes(
                x = x_labs_f,
                y = beta,
                color = PF,
            ),
            size = 2
        ) +
        geom_hline(
            yintercept = 0.0,
            color = my_colors[["gray"]],
            linewidth = 1
        ) +
        # scale_x_discrete(labels = xlabs) +
        coord_flip() +
        theme_classic() +
        labs(y = "", x = "") +
        ggtitle(
            plot_title
        ) +
        theme(
            legend.position = "bottom",
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = -0.8)
        ) #+
    # facet_wrap(
    #     ~Size,
    #     ncol = 1,
    #     scale = "free_y",
    #     labeller = as_labeller(size_description)
    # )
    print(my_plot)
    ggsave(
        paste0(
            save_list[["fig_dir"]],
            save_list[["ex_name"]],
            "_",
            save_list[["local_data"]],
            ".",
            save_list[["plot_suffix"]]
        ),
        width = 16,
        height = 9,
        units = "cm"
    )
}

## plot_share_means_by_ntile()
plot_share_means_by_ntile <- function(
    running_var = "lag_log_ind_exp_k",
    data = colombia_data_frame,
    colors = my_colors,
    run_var_desc = size_desc,
    ntile = 20,
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "disc",
        plot_suffix = "png"
    )) {
    my_plot <- data %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(
            nthile = ntile(.data[[running_var]], ntile)
        ) %>%
        group_by(nthile) %>%
        ggplot(
            aes(x = nthile, y = log_share, group = nthile)
        ) +
        # geom_boxplot() + #(color='gray')+
        stat_summary(
            fun = "mean",
            na.rm = TRUE,
            geom = "point",
            shape = 18,
            size = 4,
            color = colors[["purple"]]
        ) + # add mean points
        stat_summary(
            fun.data = mean_cl_normal,
            geom = "errorbar",
            width = 0.1,
            color = colors[["gray"]],
            show.legend = FALSE
        ) + # add CI bars
        theme_classic() +
        labs(
            title = "Means by percentile",
            y = "Input's cost share of sales (mean of logs)",
            x = paste0("N-tile of\n", run_var_desc[running_var])
        )
    # run_var_name <- data %>%
    #     ungroup() %>%
    #     select(lag_log_ind_exp_k) %>%
    #     names()
    print(my_plot)
    ggsave(
        paste0(
            save_list[["fig_dir"]],
            save_list[["ex_name"]],
            "_",
            save_list[["local_data"]],
            "_",
            running_var, "_", ntile,
            ".",
            save_list[["plot_suffix"]]
        ),
        width = 16,
        height = 9,
        units = "cm"
    )
}



plot_share_means_by_ntile_year <- function(
    running_var = "lag_log_ind_exp_k",
    colors = my_colors,
    shapes = my_shapes,
    data = colombia_data_frame,
    run_var_desc = size_desc,
    leg_pos = c(0.8, 0.2),
    ntile = 20,
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "disc_byy",
        plot_suffix = "png"
    )) {
    my_plot <- data %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(
            nthile = ntile(.data[[running_var]], ntile)
        ) %>%
        group_by(year, nthile) %>%
        ggplot(
            aes(
                x = nthile,
                y = log_share,
                group = factor(year),
                color = factor(year),
                shape = factor(year)
            )
        ) +
        stat_summary(
            fun = "mean",
            na.rm = TRUE,
            geom = "point",
            # shape = 18,
            size = 4,
            # color = colors[["purple"]]
        ) + # add mean points
        stat_summary(
            fun.data = mean_cl_normal,
            geom = "errorbar",
            width = 0.1,
            # color = colors[["gray"]],
            show.legend = FALSE
        ) + # add CI bars
        theme_classic() +
        scale_shape_manual(
            values = shapes
        ) +
        scale_color_manual(
            values = colors,
            na.value = "transparent"
        ) +
        labs(
            title = "Means by percentile and year",
            y = "Input's cost share of sales (mean of logs)",
            x = paste0("N-tile of\n", run_var_desc[running_var]),
            color = "Year",
            shape = "Year"
        ) +
        theme(
            legend.position = "top"#leg_pos # "right"#,
            # legend.title = element_text()
        )
    # run_var_name <- data %>%
    #     ungroup() %>%
    #     select(lag_log_ind_exp_k) %>%
    #     names()
    print(my_plot)
    ggsave(
        paste0(
            save_list[["fig_dir"]],
            save_list[["ex_name"]],
            "_",
            save_list[["local_data"]],
            "_",
            running_var,
            "_", paste0(names(colors), collapse = "-"),
            "_", ntile,
            ".",
            save_list[["plot_suffix"]]
        ),
        width = 16,
        height = 12,
        units = "cm"
    )
}

plot_mean_by_year <- function(
    y = "share_sales_tax",
    data = colombia_data_frame,
    # colors = my_colors,
    var_desc = size_desc,
    save_list = list(
        fig_dir = fig_dir,
        local_data = "byy",
        plot_suffix = "png"
    )) {
    the_plot <- data %>%
        ungroup() %>%
        mutate(
            share_sale_tax_sales = sales_taxes / sales
        ) %>%
        ggplot(
            aes(
                x = factor(year),
                y = .data[[y]],
                group = factor(year),
                color = factor(year)
            )
        ) +
        geom_vline(
            xintercept = c("83","86","90"),
            colour = "lightgray", #my_colors[["gray"]],
            linetype = "dashed"
        )+
        stat_summary(
            fun = "mean",
            na.rm = TRUE,
            geom = "point",
            shape = 18,
            size = 4,
            color = my_colors[["purple"]]
        ) + # add mean points
        stat_summary(
            fun.data = mean_cl_normal,
            geom = "errorbar",
            width = 0.1,
            color = my_colors[["purple"]],
            show.legend = FALSE
        ) + # add CI bars
        theme_classic() +
        labs(
            title = paste0(var_desc[[y]], ", annual mean"),
            y = "",
            x = ""
        )

    print(the_plot)
    ggsave(
        paste0(
            save_list[["fig_dir"]],
            y,
            "_",
            save_list[["local_data"]],
            # "_",
            # running_var,
            ".",
            save_list[["plot_suffix"]]
        ),
        width = 16,
        height = 9,
        units = "cm"
    )
}

plot_y_means_by_x_ntile <- function(
    running_var = "lag_log_ind_exp_k",
    y = "log_share",
    data = colombia_data_frame,
    colors = my_colors,
    run_var_desc = size_desc,
    ntile = 20,
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "disc",
        plot_suffix = "png"
    )) {
    my_plot <- data %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(
            nthile = ntile(.data[[running_var]], ntile)
        ) %>%
        group_by(nthile) %>%
        ggplot(
            aes(x = nthile, y = .data[[y]], group = nthile)
        ) +
        # geom_boxplot() + #(color='gray')+
        stat_summary(
            fun = "mean",
            na.rm = TRUE,
            geom = "point",
            shape = 18,
            size = 4,
            color = colors[["purple"]]
        ) + # add mean points
        stat_summary(
            fun.data = mean_cl_normal,
            geom = "errorbar",
            width = 0.1,
            color = colors[["gray"]],
            show.legend = FALSE
        ) + # add CI bars
        theme_classic() +
        labs(
            title = "Means by percentile",
            y = paste0(run_var_desc[y]),
            x = paste0("N-tile of\n", run_var_desc[running_var])
        )
    # run_var_name <- data %>%
    #     ungroup() %>%
    #     select(lag_log_ind_exp_k) %>%
    #     names()
    print(my_plot)
    ggsave(
        paste0(
            save_list[["fig_dir"]],
            save_list[["ex_name"]],
            "_",
            save_list[["local_data"]],
            "_",
            running_var, "_", ntile,
            ".",
            save_list[["plot_suffix"]]
        ),
        width = 16,
        height = 9,
        units = "cm"
    )
}
# Save my plot -----------------------

save_plot <- function(
    my_plot,
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "byy",
        plot_suffix = "png"
    )) {
    print(my_plot)
    ggsave(
        paste0(
            save_list[["fig_dir"]],
            save_list[["ex_name"]],
            "_",
            save_list[["local_data"]],
            # "_",
            # running_var,
            ".",
            save_list[["plot_suffix"]]
        ),
        width = 16,
        height = 9,
        units = "cm"
    )
}

# Saving functions -------------------
print("Saving functions")
save(
    list = ls(),
    file = "Code/Products/functions.Rdata"
)
