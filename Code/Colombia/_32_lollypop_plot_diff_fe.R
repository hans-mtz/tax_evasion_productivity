library(tidyverse)
library(ggplot2)
load("Code/Products/beta_diff_fe.RData")
load("Code/Products/global_vars.RData")

# Get labels for plot -----------------------

xlabs <- data.frame(
    q = beta_diff_fe[["Quantile"]] |>
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
beta_diff_fe %>%
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
        "All firms with industry fixed effects\nColombia"
    ) +
    labs(y = "", x = "Quantile") +
    facet_wrap(
        ~Size,
        ncol = 1,
        scale = "free_y",
        labeller = as_labeller(size_desc)
    )

# Save plot -----

ggsave(
    paste0(
        fig_dir,
        "beta_diff_fe.png"
    ),
    width = 16,
    height = 32,
    units = "cm"
)
