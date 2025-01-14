library(tidyverse)
# Load data, global vars, and functions ---------
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/functions.RData")

#
lapply(
    size,
    plot_share_means_by_ntile
)
# plot_share_means_by_ntile("share_exports")

plot_share_means_by_ntile(
    "lag_log_sales_tax",
    ntile = 100
)

# Plot share by ntile, colored by year

# ## Sales taxes
# plot_share_means_by_ntile_year(
#     "lag_log_sales",
#     color_year,
#     shape_year#,
#     # ntile = 40
# )

lapply(
    size,
    function(x){
        plot_share_means_by_ntile_year(
            x,
            color_year,
            shape_year
        )
    }
)


lapply(
    size,
    function(x){
        plot_share_means_by_ntile_year(
            x,
            color_year_2,
            shape_year_2
        )
    }
)

lapply(
    size,
    function(x){
        plot_share_means_by_ntile_year(
            x,
            color_year_3,
            shape_year_3
        )
    }
)
# ## Industrial expenditure
# plot_share_means_by_ntile_year(
#     "lag_log_ind_exp_k",
#     color_year_2
# )

# Doin' it

# comb_color <- expand.grid(
#     vars = size,
#     color = c("color_year", "color_year_2", "color_year_3")
# )

# comb_shape <- expand.grid(
#     vars = size,
#     shape = c("shape_year", "shape_year_2", "shape_year_3")
# )

# comb <- merge(comb_color, comb_shape)

# plotitos <- lapply(
#     seq_len(nrow(comb)),
#     function(i) {
#         with(
#             comb[i, ],
#             plot_share_means_by_ntile_year(
#                 vars,
#                 get(color),
#                 get(shape) # ,

#             )
#         )
#     }
# )
