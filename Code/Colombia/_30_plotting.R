library(ggplot2)
library(patchwork)
source("Code/Colombia/20_regression.r")

## Cleaning the workspace ----
# keep_vars <- c("results_df")

# rm(
#     list = setdiff(ls(), keep_vars)
# )
## Declaring vars ----

my_colors <- c(
    "#4F2683", # "purple",
    "#807F83" # "gray"
)

unrelated_size <- results_df %>%
    ungroup() %>%
    select(Size, Structurally.related) %>%
    filter(Structurally.related == "Not Related") %>%
    unique() %>%
    pull(Size)

## Developing plotting code ----

# results_df %>%
#     filter(
#         Quantile != 0.00,
#         Structurally.related == "Not Related",
#         Size == "labor_employee_years"
#     ) %>%
#     ggplot(
#         aes(
#             x = Quantile,
#             y = beta # ,
#         )
#     ) +
#     geom_line(color = my_colors[[1]], linewidth = 1) +
#     geom_ribbon(
#         aes(ymin = LCI..95.., ymax = UCI..95..),
#         alpha = 0.1,
#         fill = my_colors[[1]]
#     ) +
#     geom_hline(
#         yintercept = results_df %>%
#             filter(
#                 Quantile == 0.0,
#                 Size == "labor_employee_years"
#             ) %>%
#             # select(beta, LCI..95..,UCI..95..) %>%
#             # unlist(),
#             pull(beta),
#         # linetype = c("solid","dashed","dashed"),
#         color = my_colors[[2]],
#         linewidth = 1
#     ) +
#     geom_ribbon(
#         aes(
#             ymin = results_df %>%
#                 filter(
#                     Quantile == 0.0,
#                     Size == "labor_employee_years"
#                 ) %>% select(LCI..95..) %>%
#                 pull(),
#             ymax = results_df %>%
#                 filter(
#                     Quantile == 0.0,
#                     Size == "labor_employee_years"
#                 ) %>% select(UCI..95..) %>%
#                 pull()
#         ),
#         alpha = 0.25,
#         fill = my_colors[[2]]
#     ) +
#     theme_classic() +
#     ggtitle("Colombia \nQuantile of labor")

## Plotting function

plot_beta <- function(
    size = "labor_employee_years",
    data = results_df,
    colors = my_colors
    ) {
    naive_beta <- data %>%
        filter(
            Quantile == 0.0,
            Size == size
        ) %>%
        select(beta, LCI..95.., UCI..95..) %>%
        unlist()

    plot <- data %>%
        filter(
            Quantile != 0.00,
            # Structurally.related == "Not Related",
            Size == size
        ) %>%
        ggplot(
            aes(
                x = Quantile,
                y = beta # ,
            )
        ) +
        geom_line(color = colors[[1]], linewidth = 1) +
        geom_ribbon(
            aes(ymin = LCI..95.., ymax = UCI..95..),
            alpha = 0.1,
            fill = colors[[1]]
        ) +
        geom_hline(
            yintercept = naive_beta[[1]],
            color = colors[[2]],
            linewidth = 1
        ) +
        geom_ribbon(
            aes(
                ymin = naive_beta[[2]],
                ymax = naive_beta[[3]]
            ),
            alpha = 0.25,
            fill = colors[[2]]
        ) +
        theme_classic() +
        ggtitle(
            paste0(
                "Colombia \nQuantile of ",
                size
            )
        )
    plot
}

## Plotting Beta by different measures of size ----

beta_plots <- lapply(unrelated_size, plot_beta)
# beta_plots_related <- lapply(
#     setdiff(size, unrelated_size),
#     plot_beta
# )

## Summary ##
# Labor works: employees, employee x years, wages (plots 1-3)
# Capital, Lag of production, sales and taxes don't work

## Saving plots ----

fig_folder <- "Results/Figures/Colombia/"

# lapply(
#     1:3,
#     function(x) {
#         print(beta_plots[[x]])
#         ggsave(
#             paste0(fig_folder, "beta_", unrelated_size[[x]], ".svg"),
#             width = 16,
#             height = 10.5,
#             units = "cm"
#         )
#     }
# )

beta_plots[[4]] +
    labs(x = "", y="",title = "Capital")+
    beta_plots[[5]] +
    labs(x = "", y="",title = "Lag Gross Output")+
    beta_plots[[6]] +
    labs(x = "", y="",title = "Lag Sales")+
    beta_plots[[7]]+
    labs(x = "", y="",title = "Lag Taxes")
ggsave(
    paste0(
        fig_folder,
        "beta_all_inds_related_size.svg"
    ),
    width = 16,
    height = 10.5,
    units = "cm"
)
