library(ggplot2)
library(patchwork)
library(forcats)
library(RColorBrewer)

## Estimation ----
source("../data/Estimation/estimation.R")

## Plotting ----
my_colors <- c(
    "#4F2683", #"purple",
    "#807F83" #"gray"
)

# colors <- c(
#     "gray",
#     rep("purple", 5),
#     rep("gray",4)
# )

# palette <- c(
#     rep("#38a3a5",4),
#     "#e54f6d",
#     rep("#4f2679",3),
#     "#e59500",
#     "#baa5ff"
# )

results_df %>%
    left_join(
        data.frame(
            Size = c(
            "tot_income", 
            "tot_sales", 
            "matprima", 
            "Vbp", 
            "tot_capital_val",
            "tot_wages", 
            "totremun", 
            "totalpeoc", 
            "cant_ener", 
            "cant_agua"),
        labels = c(
            "Income",
            "Sales",
            "Raw Materials",
            "Production",
            "Capital",
            "Wages",
            "Wages+Benefits",
            "Workers",
            "Energy", 
            "Water")
        )
    ) %>% 
    filter(
        Industry==industries[1], 
        Output=="total_valor_prod",
        Quantile!=0.0
    ) %>% 
    ggplot(
        aes(
            x=Quantile,
            y=beta,
            color = Independence,
            linetype = Size 
            # color=fct_reorder(Size,beta,max)
        )
    ) +
    geom_line() +
    geom_text(
        aes(
            x = 0.90,
            y = beta,
            label = labels
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
            Industry==industries[1], 
            Output=="total_valor_prod",
            Quantile==0.0,
            Size=="tot_income"
            ) %>% pull(beta),
        linetype = "dashed",
        color = "black",
        size = 1
    ) +
    scale_linetype(
        guide=FALSE
    ) +
    scale_colour_manual(
        values = my_colors,
        guide = FALSE,
        # palette = "Set3", 
        # name = "",
    ) +
    theme_classic() +
    # theme(legend.position = "top") +
    ggtitle("Food products elaboration")

## Formula ----
# to produce same plot for different inds

plot_inds <- function(
    inds_chr="C10", 
    data = results_df, 
    inds_description=inds_des,
    col = palette
    ) {
    # plot_title <- inds_df %>% 
    #     filter(cod_ciiu2d==inds_chr) %>% 
    #     select(des_ciiu2d) %>% 
    #     unique(.) %>% 
    #     pull()
    avg <- data %>% 
        filter(
        Industry==inds_chr, 
        Output=="total_valor_prod",
        Quantile==0.0,
        Size=="tot_income"
        ) %>% pull(beta)

    temp <- data %>% 
        left_join(
            data.frame(
                Size = c(
                "tot_income", 
                "tot_sales", 
                "matprima", 
                "Vbp", 
                "tot_capital_val",
                "tot_wages", 
                "totremun", 
                "totalpeoc", 
                "cant_ener", 
                "cant_agua"),
            labels = c(
                "Income",
                "Sales",
                "Raw Materials",
                "Production",
                "Capital",
                "Wages",
                "Wages+Benefits",
                "Workers",
                "Energy", 
                "Water")
            )
        ) %>% 
        filter(
            Industry==inds_chr, 
            Output=="total_valor_prod",
            Quantile!=0.0
        ) %>% 
        ggplot(
            aes(
                x=Quantile,
                y=beta,
                color = Independence,
                linetype = Size 
                # color=fct_reorder(Size,beta,max)
            )
        ) +
        geom_line() +
        geom_text(
            aes(
                x = 0.90,
                y = beta,
                label = labels
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
        scale_linetype(
        guide=FALSE
        ) +
        scale_color_manual(
            values = my_colors,
            guide = FALSE
        ) +
        theme_classic() +
        # theme(legend.position = "top") +
        ggtitle(
            paste0(inds_chr," ",inds_description[[inds_chr]])
        )
    temp
}

## Plotting big industries ----

plot_ls <- lapply(industries, plot_inds)

## Saving plots ----
fig_dir <- "../data/Figures/"
mapply(
    function(x,y){
        print(x)
        ggsave(
            paste0(fig_dir,y,".svg"),
            width = 16,
            height = 10.5,
            units = "cm"
        )
    },
    plot_ls[1:5],
    industries[1:5]
)

## retouching ----

plot_ls[[1]] + labs(subtitle = "Ecuador (2019)")
ggsave(
    paste0(fig_dir,"C10.svg"),
    width = 16,
    height = 10.5,
    units = "cm"
)
plot_ls[[2]] +
    # labs(title = "") +
    plot_ls[[3]] +
    # labs(title = "") +
    plot_ls[[4]] +
    # labs(title = "") +
    plot_ls[[5]] #+
    # labs(title = "")
    
ggsave(
    paste0(
        fig_dir,"ECweird.svg"),
    width = 16,
    height = 10.5,
    units = "cm"
)
