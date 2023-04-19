# Loading packages, vars and data -----

library(tidyverse)
library(ggplot2)

load("Code/Products/global_vars.RData")
load("Code/Products/beta_inds_diff.RData")

# Plotting -----

## Get labels for plot -----------------------

xlabs <- data.frame(
    q = beta_inds_diff[["Quantile"]] |>
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

## Same industry, different sizes ----

beta_inds_diff %>%
    filter(
        Industry == "322"
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
    scale_y_continuous(limits = c(NA, 1)) +
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
    labs(y = "", x = "Quantile") +
    ggtitle(
        paste0(
            industry_description[["322"]],
            "\nColombia"
        )
    ) +
    facet_wrap(
        ~Size,
        ncol = 1,
        scale = "free_y",
        labeller = as_labeller(size_desc)
    )

## Different industries, same size ----

beta_inds_diff %>%
    filter(
        Size == "capital"
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
        ) # ,
        # Industry = factor(
        #     Industry,
        #     sort(
        #         unique(Industry)
        #     ),
        #     labels = industry_description[
        #         sort(
        #             unique(Industry)
        #         )
        #     ]
        # )
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
    labs(y = "", x = "Quantile") +
    ggtitle("Wages") +
    facet_wrap(
        ~Industry,
        ncol = 1,
        scale = "free_y",
        labeller = as_labeller(
            industry_description
        )
    )

# Plotting functions ------

plot_inds_by_size <- function(
    inds,
    data = beta_inds_diff,
    x_labs = xlabs,
    inds_description = industry_description,
    size_description = size_desc) {
    p <- data %>%
        filter(
            Industry == inds
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
        geom_hline(
            yintercept = 0.0,
            color = my_colors[["gray"]],
            linewidth = 1
        ) +
        scale_x_discrete(labels = x_labs) +
        coord_flip() +
        theme_classic() +
        theme(
            legend.position = "none",
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            axis.ticks.y = element_blank()
        ) +
        labs(y = "", x = "Quantile") +
        ggtitle(
            paste0(
                inds_description[[paste0(inds)]],
                "\nColombia"
            )
        ) +
        facet_wrap(
            ~Size,
            ncol = 1,
            scale = "free_y",
            labeller = as_labeller(size_description)
        )
    p
}

plot_size_by_inds <- function(
    size,
    data = beta_inds_diff,
    x_labs = xlabs,
    description = industry_description) {
    p <- data %>%
        filter(
            Size == size
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
        geom_hline(
            yintercept = 0.0,
            color = my_colors[["gray"]],
            linewidth = 1
        ) +
        scale_x_discrete(labels = x_labs) +
        coord_flip() +
        theme_classic() +
        theme(
            legend.position = "none",
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            axis.ticks.y = element_blank()
        ) +
        labs(y = "", x = "Quantile") +
        ggtitle(paste0(size, " by Industry \n Colombia")) +
        facet_wrap(
            ~Industry,
            ncol = 1,
            scale = "free_y",
            labeller = as_labeller(
                description
            )
        )
    p
}

plot_inds_by_size("322")
plot_size_by_inds("wages")

# Lapplying plots ---------------

by_inds_plots <- lapply(big_industries, plot_inds_by_size)
by_size_plots <- lapply(size, plot_size_by_inds)

# Saving plots -----------------

lapply(
    seq_along(big_industries),
    \(x){
        print(by_inds_plots[[x]])
        ggsave(
            paste0(fig_dir, big_industries[[x]], "_by_size_diff.png"),
            height = 32,
            width = 16,
            units = "cm"
        )
    }
)

lapply(
    seq_along(size),
    \(x){
        print(by_size_plots[[x]])
        ggsave(
            paste0(fig_dir, size[[x]], "_by_inds_diff.png"),
            height = 32,
            width = 16,
            units = "cm"
        )
    }
)
