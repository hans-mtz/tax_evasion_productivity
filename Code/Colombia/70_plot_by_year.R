library(tidyverse)
library(ggplot2)

load("Code/Products/colombia_data.RData")
# load("Code/Products/global_vars.RData")
load("Code/Products/functions.RData")

##

my_vars <- c(
    "share_sales_tax",
    "log_share",
    "sales",
    "log_sales"
)

plot_mean_by_year("log_sales")

lapply(
    my_vars,
    plot_mean_by_year
)

## Paid sales taxes share of sales
# share_sales_tax <- colombia_data_frame %>%
#     ungroup() %>%
#     mutate(
#         share_sale_tax_sales = sales_taxes / sales
#     ) %>%
#     ggplot(
#         aes(
#             x = factor(year),
#             y = share_sale_tax_sales,
#             group = factor(year),
#             color = factor(year)
#         )
#     ) +
#     stat_summary(
#         fun = "mean",
#         na.rm = TRUE,
#         geom = "point",
#         shape = 18,
#         size = 4,
#         color = my_colors[["purple"]]
#     ) + # add mean points
#     stat_summary(
#         fun.data = mean_cl_normal,
#         geom = "errorbar",
#         width = 0.1,
#         color = my_colors[["gray"]],
#         show.legend = FALSE
#     ) + # add CI bars
#     theme_classic() +
#     labs(
#         title = "Sales taxes as a share of sales, annual mean",
#         y = "",
#         x = ""
#     )

# save_plot(share_sales_tax,
#     save_list = list(
#         fig_dir = fig_dir,
#         ex_name = "sale_tax_share",
#         local_data = "byy",
#         plot_suffix = "png"
#     )
# )


# ## Paid sales taxes share of sales
# # log_share_byy<-

# colombia_data_frame %>%
#     ungroup() %>%
#     mutate(
#         share_sale_tax_sales = sales_taxes / sales
#     ) %>%
#     ggplot(
#         aes(
#             x = factor(year),
#             y = log_share,
#             group = factor(year),
#             color = factor(year)
#         )
#     ) +
#     stat_summary(
#         fun = "mean",
#         na.rm = TRUE,
#         geom = "point",
#         shape = 18,
#         size = 4,
#         color = my_colors[["purple"]]
#     ) + # add mean points
#     stat_summary(
#         fun.data = mean_cl_normal,
#         geom = "errorbar",
#         width = 0.1,
#         color = my_colors[["gray"]],
#         show.legend = FALSE
#     ) + # add CI bars
#     theme_classic() +
#     labs(
#         title = "Intermediates' cost share of revenue, annual mean of logs",
#         y = "",
#         x = ""
#     )

# save_plot(log_share_byy,
#     save_list = list(
#         fig_dir = fig_dir,
#         ex_name = "log_share",
#         local_data = "byy",
#         plot_suffix = "png"
#     )
# )

# ## Paid sales taxes share of sales
# colombia_data_frame %>%
#     ungroup() %>%
#     mutate(
#         share_sale_tax_sales = sales_taxes / sales
#     ) %>%
#     ggplot(
#         aes(
#             x = factor(year),
#             y = log_sales,
#             group = factor(year),
#             color = factor(year)
#         )
#     ) +
#     stat_summary(
#         fun = "mean",
#         na.rm = TRUE,
#         geom = "point",
#         shape = 18,
#         size = 4,
#         color = my_colors[["purple"]]
#     ) + # add mean points
#     stat_summary(
#         fun.data = mean_cl_normal,
#         geom = "errorbar",
#         width = 0.1,
#         color = my_colors[["gray"]],
#         show.legend = FALSE
#     ) + # add CI bars
#     theme_classic() +
#     labs(
#         title = "Intermediates' cost share of revenue, annual mean of logs",
#         y = "",
#         x = ""
#     )

# save_plot(log_share_byy,
#     save_list = list(
#         fig_dir = fig_dir,
#         ex_name = "log_share",
#         local_data = "byy",
#         plot_suffix = "png"
#     )
# )

# ## Paid sales taxes share of sales
# colombia_data_frame %>%
#     ungroup() %>%
#     mutate(
#         total_tax = consumption_taxes,
#         my_var = sales_taxes/sales
#     ) %>%
#     ggplot(
#         aes(
#             x = factor(year),
#             y = my_var,
#             group = factor(year),
#             color = factor(year)
#         )
#     ) +
#     stat_summary(
#         fun = "mean",
#         na.rm = TRUE,
#         geom = "point",
#         shape = 18,
#         size = 4,
#         color = my_colors[["purple"]]
#     ) + # add mean points
#     stat_summary(
#         fun.data = mean_cl_normal,
#         geom = "errorbar",
#         width = 0.1,
#         color = my_colors[["gray"]],
#         show.legend = FALSE
#     ) + # add CI bars
#     theme_classic() +
#     labs(
#         title = "Intermediates' cost share of revenue, annual mean of logs",
#         y = "",
#         x = ""
#     )

# colombia_data_frame %>%
#     ungroup() %>%
#     mutate(
#         total_tax = sales_taxes+imex_taxes,
#         v = total_tax == indirect_taxes
#     ) %>%
#     summarise(
#         trues = sum(v, na.rm = TRUE),
#         missing = sum(is.na(v)),
#         n = n()
#     )

