library(ggplot2)
library(patchwork)
library(forcats)
library(RColorBrewer)


## Declaring variables ----
fig_folder <- "Results/Figures/Colombia/"

my_colors <- c(
    "#4F2683", #"purple",
    "#807F83" #"gray"
)

inds_desc_COL <- c(
    "FABRICACIÓN DE PRODUCTOS ALIMENTICIOS EXCEPTO BEBIDAS",
    "FABRICACIÓN DE PRENDAS VESTIR EXCEPTO CALZADO",
    "FABRICACIÓN DE PRODUCTOS METÁLICOS, EXCEPTO MAQUINARIA Y EQUIPO",
    "FABRICACIÓN DE TEXTILES",
    "IMPRENTAS, EDITORIALES E INDUSTRIAS CONEXAS",
    "CONSTRUCCIÓN DE MAQUINARIA EXCEPTO ELÉCTRICA",
    "PRODUCTOS DEL PLASTICOS N.E.P.\n(Juguetes, bolsas de nylon, etc.)",
    "FABRICACIÓN DE OTROS PRODUCTOS MINERALES NO METALICOS\n(Cemento, cal y yeso)",
    "FABRICACIÓN DE OTROS PRODUCTOS QUIMICOS\n(Pinturas, farmaceuticos, de limpieza)",
    "FABRICACIÓN DE CALZADO"
)
names(inds_desc_COL)<-c(
    "311",
    "322",
    "381",
    "321",
    "342",
    "382",
    "356",
    "369",
    "352",
    "324"
)
# palette <- c(
#     rep("#38a3a5",4),
#     "#e54f6d",
#     rep("#4f2679",3),
#     "#e59500",
#     "#baa5ff"
# )

## Estimation ----
source("Code/Colombia/estimation.r")

## Testing plot ----

# res_col_df %>% filter(
#     Industry==inds_col[1], 
#     # Output==output_col[1],
#     Quantile==0.80
# )
res_col_df %>% 
    filter(
        Industry==inds_col[1],
        Output==output_col[1],
        Quantile!=0.00
    ) %>% 
    ggplot(
        aes(
            x = Quantile,
            y = beta,
            color = Independence,
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
        yintercept = res_col_df %>% 
            filter(
            Industry==inds_col[1], 
            Output==output_col[1],
            Quantile==0.0,
            Size==size_col[1]
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
    # theme(legend.position = "top") +
    ggtitle(paste0(inds_col[1]," Industry - Colombia"))

## Formula ----
# to produce same plot for different inds

plot_inds_COL <- function(
    inds_chr = "311",
    data = res_col_df,
    inds_desc = inds_desc_COL,
    col = my_colors
    ) {
    # plot_title <- inds_df %>% 
    #     filter(cod_ciiu2d==inds_chr) %>% 
    #     select(des_ciiu2d) %>% 
    #     unique(.) %>% 
    #     pull()
    avg <- data %>%
        filter(
            Industry == inds_chr,
            Output == output_col[1],
            Quantile == 0.0,
            Size == max(Size)
        ) %>% pull(beta)

    temp <- data %>%
        filter(
            Industry == inds_chr,
            Output == output_col[1],
            Quantile != 0.00
        ) %>%
        ggplot(
            aes(
                x = Quantile,
                y = beta,
                color = Independence,
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
        ggtitle(paste0(inds_chr," ",inds_desc[[paste0(inds_chr)]]))
    temp
}

## Plotting big industries ----

plot_ls_COL <- lapply(inds_col, plot_inds_COL)
# OK: 1- 311, 2- 322, 7- 356
# Not OK: 5-342, 6-382, 8- 369, 9-352, 10-324
# Weird: 3- 381, 4- 321


## Saving figures ----

mapply(
    function(x,y){
        print(x)
        ggsave(
            paste0(fig_folder,"COL",y,".svg"),
            width = 16,
            height = 10.5,
            units = "cm"
        )
    },
    plot_ls_COL,
    inds_col
)

## Retouching ----
# 311
plot_ls_COL[[1]]+labs(subtitle = "Colombia (1981-1991)")
ggsave(
    paste0(fig_folder,"COL311.svg"),
    width = 16,
    height = 10.5,
    units = "cm"
)
# 322
plot_ls_COL[[2]] + ylim(0.3,0.75)+labs(subtitle = "Colombia (1981-1991)")
ggsave(paste0(fig_folder,"COL322.svg"), width=16,height=10.5,units="cm")
# 356
plot_ls_COL[[7]]+labs(subtitle = "Colombia (1981-1991)")
ggsave(
    paste0(fig_folder,"COL356.svg"),
    width = 16,
    height = 10.5,
    units = "cm"
)
# Grouping weird ones and NOT OK
plot_ls_COL[[3]] +
    ylim(0.5,0.7) + 
    plot_ls_COL[[4]] +
    ylim(0.5,0.65)
ggsave(
    paste0(
        fig_folder,
        "COL381-321.svg"
    ),
    width = 16,
    height = 10.5,
    units = "cm"
)

plot_ls_COL[[5]] +
    plot_ls_COL[[6]] +
    plot_ls_COL[[8]] +
    plot_ls_COL[[9]]
ggsave(
    paste0(
        fig_folder,
        "COLweird.svg"
    ),
    width = 16,
    height = 10.5,
    units = "cm"
)
