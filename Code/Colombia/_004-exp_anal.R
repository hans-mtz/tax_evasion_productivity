library(tidyverse)
library(ggplot2)
library(modeest)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

## Distribution of log_share ---------

plot(density(colombia_data_frame$share_fem_owners, na.rm = TRUE))
plot(colombia_data_frame$share_fem_owners, na.rm = TRUE)
hist(colombia_data_frame$share_fem_owners, na.rm = TRUE)
hist(colombia_data_frame$log_share, na.rm = TRUE)


colombia_data_frame %>%
    group_by(year) %>%
    mutate(
        q = quantile(log_lag_sales, probs = 0.2, na.rm = TRUE)[[1]],
        compliers = ifelse(
            log_lag_sales <= q, 1, 0
        )
    ) %>%
    # View()
    filter(!is.na(compliers)) %>%
    ggplot(aes(x=log_share, y=factor(compliers), fill= factor(compliers)))+
    geom_boxplot(alpha = 0.5)+
    coord_flip()

colombia_data_frame %>%
    mutate(
        q = quantile(lag_k, probs = 0.2, na.rm = TRUE)[[1]],
        compliers = ifelse(
            lag_k > q, 0, 1
        )
    ) %>%
    filter(compliers == 1) %>% pull(log_share) %>%
    hist(., na.rm = TRUE)

colombia_data_frame %>%
    mutate(
        q = quantile(lag_m, probs = 0.2, na.rm = TRUE)[[1]],
        compliers = ifelse(
            lag_m > q, 0, 1
        )
    ) %>%
    filter(!is.na(compliers)) %>%
    ggplot(aes(x=log_share, y=factor(compliers), fill= factor(compliers)))+
    geom_boxplot(alpha = 0.5)+
    coord_flip()

colombia_data_frame %>%
    mutate(
        q = quantile(lag_m, probs = 0.2, na.rm = TRUE)[[1]],
        compliers = ifelse(
            lag_m > q, 0, 1
        )
    ) %>%
    filter(!is.na(compliers)) %>%
    ggplot(aes(x=log_share, fill= factor(compliers)))+
    geom_density(alpha = 0.5)+
    geom_vline(
        xintercept = 
        )

colombia_data_frame %>%
            mutate(
                q = quantile(lag_m, probs = 0.2, na.rm = TRUE)[[1]],
                compliers = ifelse(
                    lag_m > q, 0, 1
                )
            ) %>%
            group_by(compliers) %>%
            filter(sales>0) %>%
            summarise(
                across(
                    log_share,
                    list(
                        mean=~mean(.x, na.rm=TRUE),
                        median=~median(.x, na.rm=TRUE),
                        mode=~mlv(.x, na.rm = TRUE, method = "parzen", kernel ="gaussian"),
                        N = ~n()
                    )
                )
            ) %>% exp()

colombia_data_frame %>%
    mutate(
        fem_owned = ifelse(share_fem_owners>=0.5, 1, 0)
    ) %>%
    ggplot(aes(x=log_share, y=as.factor(fem_owned), fill = as.factor(fem_owned)))+
    geom_violin() +
    coord_flip()

colombia_data_frame %>%
    mutate(
        exporter = ifelse(share_exports>0.5, 1, 0)
    ) %>%
    ggplot(aes(x=log_share, y=as.factor(exporter), fill= as.factor(exporter)))+
    geom_violin() +
    coord_flip()

colombia_data_frame %>%
    mutate(
        fem_owned = ifelse(share_fem_owners>=0.5, 1, 0)
    ) %>%
    ggplot(aes(x=log_share, y=log(lag_M), colour = as.factor(fem_owned)))+
    geom_point() +
    geom_smooth(method = lm)+
    coord_flip()#+
    # facet_wrap(~as.factor(fem_owned))

colombia_data_frame %>%
    mutate(
        fem_owned = ifelse(share_fem_owners>=0.5, 1, 0)
    ) %>%
    ggplot(aes(x=log_share, y=log(lag_M), colour = as.factor(fem_owned)))+
    geom_point() +
    geom_smooth(method = lm)+
    coord_flip()#+
    # facet_wrap(~as.factor(fem_owned))

colombia_data_frame %>%
    mutate(
        fem_owned = ifelse(share_fem_owners>=0.6, 1, 0)
    ) %>%
    filter( sales > 0) %>%
    group_by(fem_owned) %>%
    summarise(
        across(
            log_share,
            list(
                mean = ~exp(mean(.x, na.rm = TRUE)),
                median = ~exp(median(.x, na.rm =TRUE)),
                N = ~n()
            )

        )
    )

colombia_data_frame %>%
    mutate(
        fem_owned = ifelse(share_fem_owners>=0.6, 1, 0),
        exporters = ifelse(share_exports >=0.7, 1, 0)
    ) %>%
    fixest::feols(log_share ~ fem_owned +factor(sic_3), data =.) %>%
    coef() %>%
    exp()


colombia_data_frame %>%
    ggplot(aes(x=log(interest_payments),y=log_share))+
    geom_point()

colombia_data_frame %>%
    ggplot(aes(x=log(sales_taxes)))+
    geom_bar()

hist(
    log(colombia_data_frame$sales_taxes),
    breaks =30000
)

hist(
    colombia_data_frame$log_lag_sales_tax,
    breaks =30000
)

hist(
    log(colombia_data_frame$interest_payments),
    breaks =30000
)

hist(
    log(colombia_data_frame$labor_employees),
    breaks =30000
)

hist(
    colombia_data_frame$k,
    breaks =1000
)

# Revenue by measure of size
with(
    colombia_data_frame,
    hist(
        log(sales_taxes/capital),
        breaks =1000
    )
)
with(
    colombia_data_frame,
    hist(
        log(sales_taxes/labor_employees),
        breaks =1000
    )
)
with(
    colombia_data_frame,
    hist(
        log(sales/labor_employees),
        breaks =1000
    )
)
with(
    colombia_data_frame,
    hist(
        log(sales_taxes),
        breaks =1000
    )
)
with(
    colombia_data_frame,
    hist(
        log(sales/capital),
        breaks =1000
    )
)
with(
    colombia_data_frame,
    hist(
        log(interest_payments)-lag_k,
        breaks =1000
    )
)
with(
    colombia_data_frame,
    plot(
        log(lag_sales)-l,
        log_share
        # breaks =1000
    )
)
with(
    colombia_data_frame,
    plot(
        log(sales),
        log(sales_taxes)
        # breaks =1000
    )
)
with(
    colombia_data_frame,
    hist(
        log(sales_taxes)-
        log(sales),
        breaks =1000
    )
)
with(
    colombia_data_frame,
    plot(
        log_lag_sales_tax-lag_k,
        log_share
        # breaks =1000
    )
)

with(
    colombia_data_frame,
    plot(
        log(interest_payments),
        lag_k
        # breaks =1000
    )
)

with(
    colombia_data_frame,
    plot(
        log_lag_sales_tax-lag_l,
        log_share
        # breaks =1000
    )
)

with(
    colombia_data_frame,
    plot(
        log_lag_sales_tax-lag_m,
        log_share
        # breaks =1000
    )
)

with(
    colombia_data_frame,
    plot(
        log(interest_payments)-lag_k,
        log_share
        # breaks =1000
    )
)
with(
    colombia_data_frame,
    plot(
        log_lag_sales-lag_k,
        log_share
        # breaks =1000
    )
)

with(
    colombia_data_frame,
    plot(
        log_lag_sales-lag_l,
        log_share
        # breaks =1000
    )
)

with(
    colombia_data_frame,
    plot(
        log_lag_sales-log(labor_employees),
        log_share
        # breaks =1000
    )
)

with(
    colombia_data_frame,
    plot(
        log_lag_sales-lag_m,
        log_share
        # breaks =1000
    )
)
with(
    colombia_data_frame,
    plot(
        lag_m-log_lag_sales,
        log_share
        # breaks =1000
    )
)

with(
    colombia_data_frame,
    plot(
        log(interest_payments),
        log_share
        # breaks =1000
    )
)
with(
    colombia_data_frame,
    plot(
        log_,
        log(sales)
        # breaks =1000
    )
)

##

plot_lollypop <- function(
    data = results,
    plot_title = "All firms, industry FE, annual quantile",
    size_description = size_desc,
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "yoy",
        plot_suffix = "png"
    )) {
    # Local vars -----------

    # local_data <- "beta_diff_fe"
    # plot_title <- "All firms industry FE\nColombia"
    # plot_suffix <- "png" # svg, pdf

    # Load data ------------

    # load(paste0(products_dir, local_data, ".RData"))
    # load("Code/Products/beta_diff_poly.RData")

    # Get labels for plot -----------------------
    xlabs <- data %>%
        mutate(
            Desc = size_description[Size],
            xlabs = paste0(Desc, "-", Quantile * 100, " quantile")
        ) %>%
        pull(xlabs)

    # Plotting ---------------
    my_plot <- data %>%
        mutate(
            Quantile_factor = factor(
                Quantile,
                sort(
                    unique(Quantile)
                )
            ),
            tail = case_when(
                Quantile >= 0.5 ~ "Top",
                TRUE ~ "Bottom"
            ),
            Complier = paste(tail, PF)#,
            # Desc = size_description[Size],
            # xlabs = paste0(Desc, "-", Quantile * 100, " quantile")
        ) %>%
        ggplot() +
        geom_segment(
            aes(
                x = Size,
                xend = Size,
                y = LCI,
                yend = UCI
            ),
            color = my_colors[["gray"]]
        ) +
        geom_point(
            aes(
                x = Size,
                y = beta,
                color = PF,
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
        labs(y = "", x = "Quantile") # +
    # facet_wrap(
    #     ~Size,
    #     ncol = 1,
    #     scale = "free_y",
    #     labeller = as_labeller(size_description)
    # )
    print(my_plot)
    ggsave(
        paste0(
            save_list[["fig_dir"]],
            save_list[["ex_name"]],
            "_",
            save_list[["local_data"]],
            ".",
            save_list[["plot_suffix"]]
        ),
        width = 21,
        height = 32,
        units = "cm"
    )
}

plot_lollypop()


with(
    colombia_data_frame,
    hist(
        share_fem_managers,
        breaks =1000
    )
)

with(
    colombia_data_frame,
    quantile(
        share_fem_managers,
        probs = 0.05,
        na.rm = TRUE
    )

)
with(
    colombia_data_frame,
    hist(
        share_exports,
        breaks =1000
    )
)

# ~50% of sales are exported
with(
    colombia_data_frame,
    quantile(
        share_exports,
        probs = 0.98,
        na.rm = TRUE
    )

)
# 60 years old
with(
    colombia_data_frame,
    quantile(
        age,
        probs = 0.99,
        na.rm = TRUE
    )

)

#        15%        20%        25%        30%        50%        75%        90% 
#   71.3310   215.1829   359.5549   508.0000  1295.4181  4672.7844 18864.5601 
with(
    colombia_data_frame,
    quantile(
        sales_taxes,
        probs = c(0.15,0.2, 0.25, 0.3,0.5,0.75,0.9),
        na.rm = TRUE
    )

)
##

##-- Simple didactical  ecdf  example :
x <- rnorm(12)
Fn <- ecdf(x)
Fn     # a *function*
Fn(x)  # returns the percentiles for x
tt <- seq(-2, 2, by = 0.1)
10 * Fn(tt) # Fn is a 'simple' function {with values k/12}
ntile(tt,10)

summary(Fn)
##--> see below for graphics
knots(Fn)  # the unique data values {12 of them if there were no ties}

colombia_data_frame %>%
    ungroup() %>%
    mutate(
        nthile = ntile(lag_log_ind_exp_k, 10)
    ) %>%
    group_by(nthile) %>%
    mutate(
        mean_log_share = mean(log_share, na.rm = TRUE)
    ) %>%
    ggplot(
        aes(x=nthile, y=log_share)
    )+
    geom_violin(
        aes(fill=factor(nthile)),
        draw_quantiles = c(0.5),
        trim = TRUE
    )

colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        my_var = exp(lag_log_sales_tax)/sum(exp(lag_log_sales_tax), na.rm = TRUE),
        log_share_tfe = log_share-sum(log_share, na.rm = TRUE),
        nthile = ntile(my_var, 50),
        share_sale_tax_sales = sales_taxes/sales
    ) %>%
    group_by(nthile, year) %>%
    ggplot(
        aes(
            x=factor(nthile), 
            y=log_share, 
            # group = factor(year), 
            # color=factor(year)
        )
    )+
    # geom_point() + #(color='gray')+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        shape = 18,
        size = 4,
        color = my_colors[["purple"]]
    )+ # add mean points
    stat_summary(
        fun.data = mean_cl_normal, 
        geom = "errorbar", 
        width = 0.1,
        # fill = year,
        color = my_colors[["gray"]], 
        show.legend = FALSE
    ) +# add CI bars
    theme_classic()#+
    # labs(
    #     title= "Means by percentile",
    #     y="Input's cost share of revenue (mean of log)",
    #     x="Percentile of (log) industrial expenditure/capital last period"
    # )

colombia_data_frame %>%
    ungroup() %>%
    select(lag_log_ind_exp_k) %>% 
    names()

color_year <- c(
    "82" = "chartreuse3",
    "83" = "blue",
    "84" = "red"#,
    # "86"= "green",
    # "87"= "orange"
)

color_year <- c(
    "82" = 'red',
    "83" = 'blue',
    "84" = 'lightgreen'#,my_colors[[3]]#,
    # "86"= "green",
    # "87"= "orange"
)

BK11 = c(
    "#E69F00", 
    "#56B4E9", 
    "#009E73", 
    "#F0E442", 
    "#0072B2", 
    "#D55E00", 
    "#CC79A7", 
    "grey"
)


colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(lag_log_sales_tax, 20),
        share_sale_tax_sales = sales_taxes/sales
    ) %>%
    group_by(year,nthile) %>%
    # filter(year %in% c("1983","1985","1986","1990","1991")) %>%
    ggplot(
        aes(
            x=nthile, 
            y=log_share, 
            # group = c(factor(year),factor(nthile)), 
            color=factor(year)
        )
    )+
    # geom_point() + #(color='gray')+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        shape = 18,
        size = 4,
        # color = factor(year)
    )+ # add mean points
    stat_summary(
        fun.data = mean_cl_normal, 
        geom = "errorbar", 
        width = 0.1,
        # fill = year,
        # color = my_colors[["gray"]], 
        show.legend = FALSE
    ) +# add CI bars
    theme_classic()+
    scale_color_manual(
        values = color_year,
        na.value = "lightgray"
    ) +
    labs(
        title= "Means by percentile and year",
        y="Input's cost share of revenue (mean of log)",
        x="Percentile of (log) sales taxes last period",
        color = "Year"
    ) +
    theme(
        legend.position = c(0.6,0.2)#,
        # legend.box = 1
    )

color_year_2 <- c(
    # "83" = "blue",
    # "84" = "red"#,
    "86"= "green",
    "88"= "orange"
)

color_nthile <- c(
    "1"= color_palette[[4]],
    "5"= color_palette[[1]],
    "10"= color_palette[[3]],
    "15"= color_palette[[2]],
    "20" = color_palette[[5]]
)

colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(lag_log_sales_tax, 20),
        share_sale_tax_sales = sales_taxes/sales
    ) %>%
    group_by(year,nthile) %>%
    # filter(year %in% c("1983","1985","1986","1990","1991")) %>%
    ggplot(
        aes(
            x=factor(year), 
            y=log_share, 
            group = factor(nthile),
            color=factor(nthile)
        )
    )+
    geom_vline(
        xintercept = c("83","86","90"),
        colour = "lightgray", #my_colors[["gray"]],
        linetype = "dashed"
    )+
    # geom_point() + #(color='gray')+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "line",
        linewidth = 2
        # shape = 18,
        # size = 4,
        # color = factor(year)
    )+ # add mean points
    stat_summary(
        fun.data = mean_cl_normal, 
        geom = "errorbar", 
        width = 0.1,
        # fill = year,
        # color = my_colors[["gray"]], 
        show.legend = FALSE
    ) +# add CI bars
    theme_classic()+
    scale_color_manual(
        values = color_nthile,
        na.value = "transparent"
    )
    # labs(
    #     title= "Means by percentile",
    #     y="Input's cost share of revenue (mean of log)",
    #     x="Percentile of (log) industrial expenditure/capital last period"
    # )

colombia_data_frame %>%
    ungroup() %>%
    group_by(metro_area_code)%>%
    summarise(
        n = n(),
        missing = sum(is.na(metro_area_code))
    )

color_metro_code<- c(
    "1"= color_palette[[1]],
    "2"= color_palette[[3]],
    "3"= color_palette[[5]],
    "7"= color_palette[[4]]#,
    # "100" = color_palette[[5]]
)

color_metro_code_2 <- c(
    "4"= color_palette[[1]],
    "5"= color_palette[[3]],
    "6"= color_palette[[5]]#,
    # # "10"= color_palette[[4]],
    # "100" = color_palette[[5]]
)

color_metro_code_3 <- c(
    # "7"= color_palette[[1]],
    "8"= color_palette[[3]],
    "9"= color_palette[[5]]#,
    # # "10"= color_palette[[4]],
    # "100" = color_palette[[5]]
)

colombia_data_frame %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
        nthile = ntile(age, 100),
        share_sale_tax_sales = sales_taxes/sales,
        my_var = subsidies_exp/exports
    ) %>%
    group_by(metro_area_code, year) %>%
    # filter(year %in% c("1983","1985","1986","1990","1991")) %>%
    ggplot(
        aes(
            x=factor(year), 
            y=my_var, 
            # group = factor(metro_area_code),
            # color=factor(metro_area_code)
        )
    )+
    geom_vline(
        xintercept = c("83","86","90"),
        colour = "lightgray", #my_colors[["gray"]],
        linetype = "dashed"
    )+
    # geom_point() + #(color='gray')+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        # shape = 18,
        size = 4,
        # color = factor(year)
    )+ # add mean points
    stat_summary(
        fun.data = mean_cl_normal, 
        geom = "errorbar", 
        width = 0.1,
        # fill = year,
        # color = my_colors[["gray"]], 
        show.legend = FALSE
    ) +# add CI bars
    theme_classic()#+
    # scale_color_manual(
    #     values = color_metro_code,
    #     na.value = "transparent"
    # )
    # labs(
    #     title= "Means by percentile",
    #     y="Input's cost share of revenue (mean of log)",
    #     x="Percentile of (log) industrial expenditure/capital last period"
    # )

colombia_data_frame %>%
    group_by(year) %>%
    # filter(year==83) %>%
    ggplot(
        aes(
            x = sales,
            # after_stat(density),
            fill = factor(year)
        )
    ) +
    geom_histogram(
        bins = 2000,
        # binwidth = 100
    ) +# add CI bars
    theme_classic()+
    scale_fill_manual(
        values = color_year_2,
        na.value = "transparent"
    ) #+
    # xlim(0,35000)

a_save_list <- list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "yoy",
        plot_suffix = "png"
    )

a_save_list[["ex_name"]] = "hola"
a_save_list


colombia_data_frame %>%
    ungroup() %>%
    ggplot(
        aes(
            x=lag_log_ind_exp_k,
            y=log_share
        )
    )+
    geom_point()
