library(tidyverse)
library(ggplot2)
library(modeest)
library(fixest)


load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/functions.RData")

### ------
colombia_data_frame %>%
    # filter(year==84) %>%
    pull(share_sales_tax) %>%
    quantile(., probs=seq(0.05,0.9,by=0.05), na.rm = TRUE)

colombia_data_frame %>%
    filter(
        year==83,
        # sales<6000
    ) %>%
    pull(sales) %>%
    hist(., 
    breaks=300000,
    xlim=c(0,5000))
abline(v=real_tax_tresh_1983)

colombia_data_frame %>%
    filter(
        year<84,
        juridical_organization==0
        # sales<6000
    ) %>%
    pull(sales) %>%
    hist(., 
    breaks=100)
    
    ,
    xlim=c(0,40000))
abline(v=real_tax_tresh_1983)


colombia_data_frame %>%
    filter(year==83) %>%
    pull(sales) %>%
    quantile(., probs=seq(0.05,0.9,by=0.05), na.rm = TRUE)

colombia_data_frame %>%
    filter(year==83, sales>0, sales<8000) %>%
    pull(sales) %>%
    hist(., breaks=50)

with(
    colombia_data_frame %>%
    filter(year==83, sales>0, sales<8000, share_sales_tax<=1),
    plot(
        share_sales_tax,
        sales
    )
)

## Check model's prediction -----

# 1) Higher tax -> higher evasion depending on shape of 
# prob of detection -> TRUE, but why firms pay different sales taxes
# 2) Higher prob of detection -> lower evasion (change of regime)
# 3) Bunching around thresholds

## Colombia's tax system questions ------
# Why are firms reporting different sales taxes?
# who is paying more? 
summary(
    feols(
        share_sales_tax~age+lag_log_sales+total_owners|factor(year)+factor(metro_area_code)+factor(sic_3),
        data=colombia_data_frame
    )
)
# Main factors are industry, metro area code and year
# Age is somewhat significant
# It is related to size: 
plot_y_means_by_x_ntile("lag_log_sales","share_sales_tax")
#   -> firms with larger revenues pay less taxes,
# is it newer vs older firms?
#   ->log_share decreasing with age;
plot_share_means_by_ntile("age")
#   -> sales tax rate decreasing with age!
#   -> Older firms pay less sales taxes
plot_y_means_by_x_ntile("age","share_sales_tax")
# is it profits?
# Is it invididulas vs societies?
#   -> Firms with 7 or more owners pay less sales taxes
#       than firm with 4 or less owners

# frequency of number of owners per firm
colombia_data_frame %>%
    ggplot()+
    geom_bar(aes(x=factor(total_owners)))
# Table total owners and year
with(
    colombia_data_frame,
    table(
        total_owners,
        year
    )
)

with(
    colombia_data_frame,
    table(
        juridical_organization,
        sic_3
    )
)

colombia_data_frame %>%
    # filter(year==83) %>%
    mutate(
        owners = case_when(
            total_owners < 9 ~ as.character(total_owners),
            .default = "9 or more"
        )
    ) %>%
    ggplot(
        aes(x=factor(owners), y=share_sales_tax)
    )+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        shape = 18,
        size = 4,
        color = my_colors[["purple"]]
    ) +# add mean points
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar",
        width = 0.1,
        color = my_colors[["gray"]],
        show.legend = FALSE
    ) + # add CI bars
    theme_classic()

colombia_data_frame %>%
    # filter(year==83) %>%
    # mutate(
    #     owners = case_when(
    #         total_owners < 9 ~ as.character(total_owners),
    #         .default = "9 or more"
    #     )
    # ) %>%
    ggplot(
        aes(
            x=factor(juridical_organization),
            y=log_share,
            color = factor(sic_3))
    )+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        shape = 18,
        size = 4,
        # color = my_colors[["purple"]]
    ) +# add mean points
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar",
        width = 0.1,
        # color = my_colors[["gray"]],
        show.legend = FALSE
    ) + # add CI bars
    theme_classic()

# is it industry sector?
#   -> Yes, some industries pay less taxes, 
#       some industries pay more
colombia_data_frame %>%
    group_by(sic_3) %>%
    summarise(
        n=n()
    ) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    mutate(perc=cumsum(n)/sum(n)*100) %>%
    # group_by(year) %>%
    ggplot(aes(x=reorder(sic_3,-n)))+
    geom_bar(
        aes(y=n), 
        stat = "identity",
        fill = "blue")+
    geom_line(
        aes(y=perc*1e2, group=1),
        color = "red"
    )+
    scale_y_continuous(
    # Features of the first axis
    name = "Frequency",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1e4, name="Percentge")
    ) +
    theme_classic()

colombia_data_frame %>%
    # filter(year==86) %>%
    group_by(sic_3) %>%
    mutate(
        n=n(),
        mean_sales_tax = mean(share_sales_tax, na.rm=TRUE)
    ) %>%
    ggplot(
        aes(x=reorder(sic_3,mean_sales_tax), y=share_sales_tax)
    )+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        shape = 18,
        size = 4,
        color = my_colors[["purple"]]
    ) +# add mean points
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar",
        width = 0.1,
        color = my_colors[["gray"]],
        show.legend = FALSE
    ) + # add CI bars
    theme_classic()

# is it metropolitan area? 
#   -> yes, there are variances by metro area code
colombia_data_frame %>%
    filter(year==81) %>%
    ggplot(
        aes(x=factor(metro_area_code), y=share_sales_tax)
    )+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        shape = 18,
        size = 4,
        color = my_colors[["purple"]]
    ) +# add mean points
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar",
        width = 0.1,
        color = my_colors[["gray"]],
        show.legend = FALSE
    ) + # add CI bars
    theme_classic()

#   -> There are variances by industry and metro area
xtabs(share_sales_tax~.,
    stats::aggregate(
        share_sales_tax~sic_3+metro_area_code,
        colombia_data_frame %>%
        filter(
            sales>0,
            ),
        mean)
)

xtabs(share_sales_tax~.,
    stats::aggregate(
        share_sales_tax~sic_3+year,
        colombia_data_frame %>%
        filter(
            sales>0,
            year <= 84
            ),
        mean)
)

colombia_data_frame %>%
    filter(
        sales>0,
        year==81
    ) %>%
    group_by(sic_3, metro_area_code) %>%
    summarise(
        mean_tax_rate = mean(share_sales_tax)
    ) %>%
    pivot_wider(
        names_from = metro_area_code,
        values_from = mean_tax_rate
    )

# Get the tax schedule for individuals

colombia_data_frame %>%
    filter(year==84) %>%
    ggplot(
        aes(x=metro_area_code, y=share_sales_tax)
    )+
    geom_point()
    # +
    # coord_cartesian(
    #     xlim = c(0,1e07),
    #     ylim = c(0,10)
    # )


plot_share_means_by_ntile("lag_log_sales")
plot_share_means_by_ntile("age")
plot_y_means_by_x_ntile("age","share_sales_tax")


summary(
    feols(
        log_share~polym(m, k, l, degree = 2, raw = TRUE) +log(share_sales_tax)+log(age)+lag_log_sales+factor(juridical_organization)|factor(metro_area_code)^factor(sic_3)^factor(year),
        data=colombia_data_frame
    )
)

summary(
    feols(
        log(share_sales_tax)~log(age)+log(lag_sales)+factor(juridical_organization)|factor(sic_3)^factor(metro_area_code)^factor(year),
        data=colombia_data_frame
    )
)


1 500,001 1,000,001 1,500,001 2,000,001 2,500,001 3,000,001 3,500,001 4,000,001 4,500,001 5,000,001 5,500,001 6,000,001
thresholds_1983 <- "a 500,000 a 1,000,000 a 1,500,000 a 2,000,000 a 2,500,000 a 3,000,000 a 3,500,000 a 4,000,000 a 4,500,000 a 5,000,000 a 5,500,000 a 6,000,000 a 6,500,000"

grep("^(a\\s)?[0-9]*", thresholds_1983, value=T)

pattern <- "[[:space:]]*a[[:space:]]"
tax_tresh <- gregexpr(pattern,thresholds_1983) |> regmatches(thresholds_1983,m=_, invert = T)

tax_tresh_1983<-as.numeric(gsub(",","",tax_tresh[[1]]))[2:length(tax_tresh[[1]])]

deflators <-colombia_data_frame %>% ungroup() %>% select(year, p_gdp) %>% unique()
real_tax_tresh_1983 <- tax_tresh_1983/(deflators[deflators$year==83, "p_gdp"][[1]]*1000)

tax_rates_1983_text <- "11.54 19.62 27.11 32.11 35.45 37.55 39.19 40.59 41.60 42.39 43.02 43.54 43.97"
tax_rates_1983<- gregexpr("[[:digit:]]+\\.[[:digit:]]+",tax_rates_1983_text) |> regmatches(tax_rates_1983_text,m=_,invert = F)
tax_ratex_1983<- as.numeric(tax_rates_1983[[1]])


colombia_data_frame %>%
    group_by(sic_3,year,metro_area_code) %>%
    summarise( mean_tax_rate = mean(share_sales_tax), n=n()) %>%
    filter(sic_3=="342") %>%
    print(n='all')
