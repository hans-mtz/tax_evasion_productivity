library(ggplot2)
library(patchwork)
source("Code/Colombia/25_regression_industry.r")

## Cleaning the workspace ----
# keep_vars <- c("results_df")

# rm(
#     list = setdiff(ls(), keep_vars)
# )
## Declaring vars ----

my_colors <- c(
    purple = "#4F2683", # "purple",
    gray = "#807F83" # "gray"
)

unrelated_size <- results_df %>%
    ungroup() %>%
    select(Size, Structurally.related) %>%
    filter(Structurally.related == "Not Related") %>%
    unique() %>%
    pull(Size)

industry_description <- c(
    "Food Products",
    "Clothing",
    "Metal products",
    "Textiles",
    "Printers"
)
names(industry_description) <- big_industries

## Developing plotting code ----

# results_df %>%
#     filter(
#         Industry == big_industries[[1]],
#         Quantile != 0.0,
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
#         aes(ymin = LCI, ymax = UCI),
#         alpha = 0.1,
#         fill = my_colors[[1]]
#     ) +
#     geom_hline(
#         yintercept = results_df %>%
#             filter(
#                 Industry == big_industries[[1]],
#                 Quantile == 0.0,
#                 Size == "labor_employee_years"
#             ) %>%
#             pull(beta),
#         # linetype = c("solid","dashed","dashed"),
#         color = my_colors[[2]],
#         linewidth = 1
#     ) +
#     geom_ribbon(
#         aes(
#             ymin = results_df %>%
#                 filter(
#                     Industry == big_industries[[1]],
#                     Quantile == 0.0,
#                     Size == "labor_employee_years"
#                 ) %>% select(LCI) %>%
#                 pull(),
#             ymax = results_df %>%
#                 filter(
#                     Industry == big_industries[[1]],
#                     Quantile == 0.0,
#                     Size == "labor_employee_years"
#                 ) %>% select(UCI) %>%
#                 pull()
#         ),
#         alpha = 0.25,
#         fill = my_colors[[2]]
#     ) +
#     theme_classic() +
#     ggtitle(
#         paste0(
#             "Colombia \n",
#             big_industries[1],
#             "\nQuantile of labor"
#         )
#     )

## Plotting function

plot_industry_beta <- function(
    industry = 311,
    size = "labor_employee_years",
    data = results_df,
    colors = my_colors,
    description = industry_description
    ) {
    naive_beta <- data %>%
        filter(
            Industry == industry,
            Quantile == 0.0,
            Size == size
        ) %>%
        select(beta, LCI, UCI) %>%
        unlist()

    plot <- data %>%
        filter(
            Industry == industry,
            Quantile != 0.00,
            Size == size
        ) %>%
        ggplot(
            aes(
                x = Quantile,
                y = beta # ,
            )
        ) +
        geom_line(
            color = colors[["purple"]],
            linewidth = 1
        ) +
        geom_ribbon(
            aes(ymin = LCI, ymax = UCI),
            alpha = 0.1,
            fill = colors[["purple"]]
        ) +
        geom_hline(
            yintercept = naive_beta[["beta"]],
            color = colors[["gray"]],
            linewidth = 1
        ) +
        geom_ribbon(
            aes(
                ymin = naive_beta[["LCI"]],
                ymax = naive_beta[["UCI"]]
            ),
            alpha = 0.25,
            fill = colors[["gray"]]
        ) +
        theme_classic() +
        ggtitle(
            paste0(
                description[[paste0(industry)]],
                "\nColombia - Quantile of ",
                size
            )
        )
    plot
}

# plot_industry_beta()

## Plotting Beta by industry AND different measures of size ----

# industry_beta_plots <- lapply(
#     big_industries,
#     function(x){
#         lapply(
#             unrelated_size,
#             function(y) plot_industry_beta(industry=x,size=y)
#         )
#     }
# )

# Generate all combinations of big_industries and unrelated_size
combinations <- expand.grid(big_industries, unrelated_size)

# Apply the plot_industry_beta() function to each combination
industry_beta_plots_chatgpt <- lapply(seq_len(nrow(combinations)), function(i) {
  with(combinations[i, ], plot_industry_beta(industry = Var1, size = Var2))
})


## Summary ##
# 311 - Food products
# Labor works: employees, employee x years, wages (less)
# Non-labor not: Capital, Lag of production, sales and taxes don't work

# 322 - Clothing
# Labor works: employees, employee x years, wages 
# Non-labor not: Capital, Lag of production, sales and taxes don't work

# 381 - Metal products
# Labor shows no evidence of evasion (cool)

# 321 - Textiles
# Labor works: employees, employee x years, wages (less)
# Non-labor not

# 342 - Printers
# Labor shows top firms display a larger beta
# 

# ## Saving plots ----

# fig_folder <- "Results/Figures/Colombia/"

# lapply(
#     1:15,
#     function(x){
#         print(industry_beta_plots_chatgpt[[x]])
#         with(
#             combinations[x,],
#             ggsave(
#                 paste0(fig_folder, Var1,"_beta_", Var2, ".svg"),
#                 width = 16,
#                 height = 9,
#                 units = "cm"
#             )

#         )
#     }
# )


industry_beta_plots_chatgpt[[10]] +
    labs(title = "Printers, editorials, and related industries",
    x = "Labor (employees x years)") +
    industry_beta_plots_chatgpt[[15]] +
    labs(title = "", y = "", x = "Wages") +
    industry_beta_plots_chatgpt[[20]] +
    labs(title = "", x = "Capital") +
    industry_beta_plots_chatgpt[[25]] +
    labs(title = "", y = "", x = "Lag Gross Ouput")
ggsave(
    paste0(
        fig_folder,
        "342_beta.svg"
    ),
    width = 16,
    height = 10.5,
    units = "cm"
)



industry_beta_plots_chatgpt[[6]] +
    labs(x = "", title = "Food products")+
    industry_beta_plots_chatgpt[[7]] +
    labs(x = "", y="",title = "Clothing")+
    industry_beta_plots_chatgpt[[8]] +
    labs(x = "",title = "Metal products")+
    industry_beta_plots_chatgpt[[9]] +
    labs(x = "", y="",title = "Textiles")
ggsave(
    paste0(
        fig_folder,
        "beta_by_inds_labor.svg"
    ),
    width = 16,
    height = 10.5,
    units = "cm"
)

