## Plot epsilon and V densities by industry

## %% load packages and data
library(tidyverse)
library(ggplot2)

load("Code/Products/fs.RData")
load("Code/Products/bs_mle_data.RData")

select_fs_l <- grep("log_mats",names(fs_list), value = TRUE) # Get all industries with log_mats_share


## %% Plot epsilon and V densities together by industry ## 

xmin <- density(fs_list[[select_fs_l[3]]]$data$cal_V)$x |> min()
xmax <- density(fs_list[[select_fs_l[3]]]$data$cal_V)$x |> max()

plot(density(fs_list[[select_fs_l[3]]]$data$cal_V),col="orange", ann=FALSE)
curve(eps_pdf_list[[select_fs_l[3]]](x),xmin,xmax,col="navy", add=TRUE, ann=FALSE)
title(
    paste0("V and Epsilon Densities\nIndustry ",
    sub("(\\d{3}).*","\\1",x=select_fs_l[3])
    )
)
legend(
    "left",
    c("V","eps"),
    fill = c("orange","navy")
)



## %% Plot selected industries in a grid

png(
    "Paper/images/280-plot-V-n-eps.png",
    width = 640, height = 480
)

par(
    family = "serif",
    mar = c(2,1,2,1),
    oma = c(2,1,4,1)
)
layout(
    matrix(c(1,2,3,4,5,5,6,6),4,2,byrow=T), height = c(1,1,1,0.1), width = c(1,1)
)
# layout.show(5)

for( i in seq_along(select_fs_l)) {
    xmin <- density(fs_list[[select_fs_l[i]]]$data$cal_V)$x |> min()
    xmax <- density(fs_list[[select_fs_l[i]]]$data$cal_V)$x |> max()
    ymax <- max(
        density(fs_list[[select_fs_l[i]]]$data$cal_V)$y,
        eps_pdf_list[[select_fs_l[i]]](seq(xmin,xmax,length.out=100)) |> max()
    )
    plot(density(fs_list[[select_fs_l[i]]]$data$cal_V),col="orange", ann=FALSE, ylim = c(0, ymax))
    curve(eps_pdf_list[[select_fs_l[i]]](x),xmin,xmax,col="navy", add=TRUE, ann=FALSE)
    title(
        paste0("Industry ",
        sub("(\\d{3}).*","\\1",x=select_fs_l[i])
        )
    )
}

par(
    family = "serif",
    mar = c(0,0,0,0)
)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend(
        "center",
        c("V","eps"),
        fill = c("orange","navy"),
        horiz = TRUE,
        bty = "n",
        cex = 1.5

    )
title(
    "V and Epsilon Densities by Industry",
    outer = TRUE,
    line = 1,
    cex.main = 1.5
)

dev.off()

## %% TODO: add legend at the bottom of the plot grid


