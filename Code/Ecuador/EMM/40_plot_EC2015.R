library(ggplot2)
library(patchwork)
library(forcats)
library(RColorBrewer)

## Declaring variables ----
fig_folder <- "Results/Figures/Ecuador/"

my_colors <- c(
    "#4F2683", #"purple",
    "#807F83" #"gray"
)

inds_key <- inds_ec_15 |> select(des_wciiu2d) |> pull()
names(inds_key) <- inds_ec_15 |> select(wciiu2d) |> pull()

results_df %>%
    filter(
        Industry==inds_ec[1],
        Quantile!=0.00
    ) %>%
    ggplot(
        aes(
            x = Quantile,
            y = beta,
            color = `Struct. Rel.`,
            linetype = Size
        )
    ) +
    geom_line() +
    geom_text(
        aes(
            x = 0.90,
            y = beta,
            label = Size
        ),
        data = ~ filter(.x, Quantile == 0.90),
        # color = "black",
        vjust = 0,
        # nudge_y = -0.003,
        check_overlap = TRUE
    ) +
    geom_hline(
        yintercept = results_df %>% 
            filter(
            Industry==inds_ec[1],
            Quantile==0.0,
            Size==size_vars[1]
            ) %>% pull(beta),
        linetype = "dashed",
        color = "black",
        size = 1
    ) +
    scale_linetype(guide = FALSE) +
    scale_colour_manual(
        values = my_colors,
        guide = FALSE
    ) +
    theme_classic() +
    ggtitle(paste0(inds_key[1],"\nEcuador"))

## Formula ----
# to produce same plot for different inds

plot_inds_EC <- function(
    inds_chr = "10",
    data = results_df,
    inds_desc = inds_key,
    col = my_colors
    ) {
    avg <- data %>%
        filter(
            Industry == inds_chr,
            Quantile == 0.0,
            Size == max(Size)
        ) %>% pull(beta)

    temp <- data %>%
        filter(
            Industry == inds_chr,
            Quantile != 0.00
        ) %>%
        ggplot(
            aes(
                x = Quantile,
                y = beta,
                color = `Struct. Rel.`,
                linetype = Size
            )
        ) +
        geom_line() +
        geom_text(
            aes(
                x = 0.90,
                y = beta,
                label = Size
            ),
            data = ~ filter(.x, Quantile == 0.90),
            # color = "black",
            vjust = 0,
            # nudge_y = -0.003,
            check_overlap = TRUE
        ) +
        geom_hline(
            yintercept = avg,
            linetype = "dashed",
            color = "black",
            size = 1
        ) +
        scale_linetype(guide = FALSE) +
        scale_colour_manual(
            values = col,
            guide = FALSE
        ) +
        theme_classic() +
        # theme(legend.position = "top") +
        ggtitle(
            paste0(inds_chr,"-",inds_desc[inds_chr],"\nEcuador")
        )
    temp
}


## Plotting big industries ----
# All industries together
plot_all <- plot_inds_EC(inds_chr="All", data = results_df_all, inds_desc="All")

# By industry, ranking across all-industries
plot_ls_EC <- lapply(inds_ec, plot_inds_EC)

# By industry, ranking by industries
plot_ls_EC_rel <- lapply(inds_ec, plot_inds_EC, data=results_df_rel)

## Saving plots ----
plot_all + labs(title="All industries", subtitle="Ecuador 2015")
ggsave(
    paste0(fig_folder,"ec_all.png"),
    width = 16,
    height = 10.5,
    units = "cm"
)

### By compare ----
# plot_ls_EC_rel[[1]] + plot_ls_EC[[1]]

plot_ls_EC_rel[[1]] + labs(title="Food Products",
    subtitle = "Relative") + ylim(0.35,0.7) +
plot_ls_EC[[1]] + labs(title = "", subtitle = "Absolute",
    y = NULL)+
    ylim(0.35,0.7)
ggsave(
    paste0(fig_folder,"ec_foodp.png"),
    width = 16,
    height = 10.5,
    units = "cm"
)

# plot_ls_EC_rel[[2]] + plot_ls_EC[[2]]

plot_ls_EC_rel[[2]] + labs(title="Rubber and Plastic",
    subtitle = "Relative") + ylim(0.4,0.62) +
plot_ls_EC[[2]] + labs(title = "", subtitle = "Absolute",
    y = NULL)+
    ylim(0.4,0.62)
ggsave(
    paste0(fig_folder,"ec_rubp.png"),
    width = 16,
    height = 10.5,
    units = "cm"
)

# plot_ls_EC_rel[[3]] + plot_ls_EC[[3]]
plot_ls_EC_rel[[3]] + labs(title="Chemical Products",
    subtitle = "Relative") +
plot_ls_EC[[3]] + labs(title = "", subtitle = "Absolute",
    y = NULL)
ggsave(
    paste0(fig_folder,"ec_chemp.png"),
    width = 16,
    height = 10.5,
    units = "cm"
)
