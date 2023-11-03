library(tidyverse)
library(ggplot2)
load("Code/Products/global_vars.RData")

# Local vars -----------

local_data <- "beta_diff_poly_fe"
plot_title <- "All firms non-linear PF with FE\nColombia"
plot_suffix <- "png" # svg, pdf

# Load data ------------

load(paste0(products_dir, local_data, ".RData"))
# load("Code/Products/beta_diff_poly.RData")

# Get labels for plot -----------------------
xlabs <- data.frame(
    q = get(local_data)[["Quantile"]] |>
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
get(local_data) %>%
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
        plot_title
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
        ex_name,
        "_",
        local_data,
        ".",
        plot_suffix
    ),
    width = 21,
    height = 32,
    units = "cm"
)
