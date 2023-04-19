library(tidyverse)
library(ggplot2)
load("Code/Products/beta_all_diff.RData")
load("Code/Products/global_vars.RData")

# Plotting vars ----------

# my_colors <- c(
#     purple = "#4F2683", # "purple",
#     green = "#26837D",
#     dark_purple = "#281342",
#     gray = "#807F83",# "gray"
#     darker_purple = "#1E0E31"
# )

# Get labels for plot -----------------------

xlabs <- data.frame(
    q = beta_all_diff[["Quantile"]] |>
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

# beta_all_diff %>%
#     group_by(Quantile) %>%
#     summarise(beta_mean=mean(beta)) %>%
#     mutate(
#         Quantile_factor = factor(
#             Quantile,
#             sort(
#                 unique(Quantile)
#             )
#         ),
#         xlabs= case_when(
#             Quantile == 0.01 ~ "1%",
#             Quantile == 0.10 ~ "10%",
#             Quantile == 0.2 ~ "20%",
#             Quantile == 0.8 ~ "80%",
#             Quantile == 0.95 ~ "95%",
#             Quantile == 0.99 ~ "99%",
#             .default = ""
#         )
#     ) %>%
#     select(Quantile, xlabs)

# Plotting ---------------
beta_all_diff %>%
    # mutate(
    #     Pick = case_when(
    #         Size == "capital" |
    #         Size == "labor_employee_years" |
    #         Size == "lag_sales" |
    #         Size == "lag_taxes" |
    #         Size == "wages" ~ TRUE,
    #         .default = FALSE
    #     )
    # ) %>%
    filter(
        Structurally.related == "Unrelated"#,
        # Pick == TRUE
    ) %>%
    mutate(
        Quantile_factor = factor(
            Quantile,
            sort(
                unique(Quantile)
            )
        ),
        Complier = case_when(
            Quantile >= 0.5 ~ "Top",
            TRUE ~ "Bottom"
        )
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
            color = Complier,
        ),
        size = 2
    ) +
    # scale_color_manual(values = my_colors[[2]]) +
    # scale_color_discrete()+
    geom_hline(
        yintercept = 0.0,
        color = my_colors[["gray"]],
        linewidth = 1
    ) +
    scale_x_discrete(labels = xlabs)+
    coord_flip() +
    theme_classic() +
    theme(
        legend.position = "none",
        panel.border = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        # strip.text.x = element_text(size=8)
    ) +
    labs(y = "Beta", x = "Quantile") +
    facet_wrap(~Size, ncol = 1, scale = "free_y")

# Save plot -----

ggsave(
    paste0(
        fig_folder,
        "beta_diff_all.svg"
    ),
    width = 16,
    height = 10.5,
    units = "cm"
)
