library(tidyverse)
library(dplyr)
# library(data.table)
# load("Code/Products/col_df.RData")
## Load packages and data ####
library(tidyverse)
library(ggplot2)
library(modeest)
library(fixest)
library(stringr)
# library(vtable)
library(tinytable)
library(modelsummary)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
# load("Code/Products/functions.RData")
# jo_class <- tibble(
#     JO_code = 0:9,
#     JO_class = c(
#         "Proprietorship",
#         "Ltd. Co.",
#         "Partnership",
#         "Corporation",
#         "Partnership",
#         "Partnership",
#         "Corporation",
#         # "Stock Co. (Corp.)",
#         "Other",
#         "Other",
#         "Other"
#     )
# )


# ciiu_data <- read.csv("Data/Colombia/ciiu-rev2-en.csv", skip = 3)


## Records vs Data ####

# sales_tax_change<-tribble(
#     ~sic_3, ~Change, ~Change_Year, ~Perry, 
#     311, "exempt", NA, " ",# Exempt in data less than 0.3 %  food prods
#     312, "exempt", NA, " ",# Exempt in data less than 0.3 % food prods
#     313, "increased", 85, "- to 35;10",# Beverages in data 5 -> 9 (6 -> 10)
#     314, "decreased", 84, "- to 35;10",# NO: Tobacco in data not significant decrease, maybe 82-83 vs 85 (not top 20)
#     321, "increased", 84, "6 to 10",# @Perry1990 6 -> 10 Textiles
#     322, "increased", 84, "6 to 10",# @Perry1990 6 -> 10 Textiles
#     323, "increased", 85, " ",# Leather products in data 8%, no change
#     324, "increased", 84, " ",# Footwear in data 6->10
#     331, "increased", 84, " ",# Wood and cork products
#     332, "decreased", 84, " ",# Furniture, in data small decrease 11->9
#     341, "increased", 84, "15 to 10",# Paper, data increase 7->9, Perry decreased 15->10
#     342, "decreased", 84, " ",# Printing in small decrease 8->7
#     351, "decreased", 84, "(Soap) 6;15 to 10",# Industrial Chemicals @Perry1990 data 8->6
#     352, "decreased", 84, "15 to 10",# Chemical Products, no change in data 
#     353, "no change", NA, " ",# Petroleum refineris NO: huge variation.  from 30% to 8% 83 and 2% 84
#     354, "no change", NA, "10 to 14",# Oil and Coal Derivatives NO:  4%
#     355, "increased", 85, " ",# Rubber prods. No change, maybe 82 vs 86-91 
#     356, "no change", NA, "15 to 10",# Plastic products 
#     361, "increased", 85, " ",# Pottery, china, earthware. NO: Increase 81-83 vs 85-88
#     362, "increased", 84, " ",# Glass and glass products: In data significant increase 81-83 vs 84-91
#     369, "decreased", 84, " ",# Non-Metallic mineral products; data significant decrease 81-83 vs 84-90
#     371, "increased", 84, "6;15 to 10",# Iron and Steel basic industries; significant increase indata 81-83 vs 84-91
#     372, "no change", NA, " ",# Metal basic inds: NO. No significant change in data
#     381, "decreased", 84, " ",# Metal prods. no machinery. Minor significant increase 81-84 vs 85-91
#     382, "increased", 84, "6 to 10",# Machinery mfg no electrical from 8% to 11% in 84, to 14% in 85
#     383, "increased", 84, "6 to 10",# Electrical machinery, increase, from 81-83 vs 84-91
#     384, "no change", 85, "6 to 10",# Transport equipment, significant increase, 81-84 vs 85-91
#     385, "increased", 85, " ",# Prof equipment, gradual increase, 81-83 vs 86-91
#     390, "decreased", 84, " ",# Other mfg equipment, gradual decrease, 81-83 vs 88-90
# )


## Data ####


### Selecting top 20 industries

# candidate_inds<-
colombia_data_frame %>%
    filter(
        sales > 0,
        # !is.na(capital),
        !is.na(k),!is.na(l), !is.na(m)
        # n_sic > 500
    ) %>%
    group_by(sic_3) %>%
    summarise(
        n_sic = n(),
        revenues = sum(sales, na.rm = TRUE),
        corps_n = sum(juridical_organization==3, na.rm = TRUE)
    ) %>%
    arrange(desc(revenues)) %>%
    mutate(
        n_cum = cumsum(n_sic),
        n_perc = n_sic / sum(n_sic) * 100,
        perc_acc = n_cum / sum(n_sic) * 100,
        market_share = revenues/ sum(revenues)*100,
        # sic_3 = as.numeric(str_sub(as.character(sic), 1, 3)),
        corps_share = corps_n/n_sic * 100
    ) %>%
    select(sic_3, n_sic, corps_n, corps_share, market_share, n_perc) %>%
    arrange(desc(corps_share)) %>%
    filter(
        # sales > 0,
        # capital > 0, 
        n_sic > 500
    )


ciiu_3 <- ciiu_data %>%
    filter(
        Nivel == "Agrupaciones"
    )
ciiu_4 <- ciiu_data %>%
    filter(
        Nivel == "Grupos"
    )

top_20_inds <- candidate_inds %>%
    left_join(
        ciiu_3[,2:3],
        by = join_by(sic_3 == `Código`)
    ) %>%
    left_join(sales_tax_change) %>%
    mutate(
        description = str_remove(`Descripción`,"Manufacture of |Manufacture ot "),
        description = str_replace_all(
            str_to_title(description),
            c(" And " = " and ", " Of " = " of ", " Or | Ord " = " or ")
        ),
        across(
            corps_share:n_perc,
            ~round(.x, digits = 2)
        ),
        Change = factor(Change, levels = c("increased","decreased","exempt","no change"))
    ) %>%
    arrange(Change) %>%
    select(!`Descripción`)

top_20_inds %>%
    View()

## Sales tax data by industry

(xtabs(share_sales_tax~.,
    stats::aggregate(
        share_sales_tax~sic_3+year,
        colombia_data_frame %>%
        filter(
            sales>0,
            year <= 86
            ),
        mean)
) -> avg_sales_tax)

annual_sales_tax_df <- colombia_data_frame %>%
    filter(
        sales > 0,
        year %in% c(82,83,84,85,86)
    ) %>%
    group_by(sic_3,year) %>%
    summarise(
        annual_avg_sales_tax_share = mean(share_sales_tax, na.rm=TRUE)*100,
        
    ) %>%
    pivot_wider(
        values_from = annual_avg_sales_tax_share,
        names_from = year,
        names_prefix = "sales_tax_share_"
    )

## Building the table ####

table<-top_20_inds %>%
    left_join(sales_tax_change) %>%
    left_join(annual_sales_tax_df) %>%
    View()

# Table is ok, but the means do not inform if the change is statistically
# significant or not

### Plot: Average per year by industry

inds_highlight <- c("354"="blue")

colombia_data_frame %>%
    filter(sic_3!="353") %>%
        mutate(
            sic_3_alpha = ifelse(sic_3 == names(inds_highlight), 1, 0.5)#,
            # sic_3_color_group = ifelse(sic_3 == inds_highlight, 1, 0)
        ) %>%
        group_by(sic_3,year) %>%
        mutate(
            share_sale_tax_sales = sales_taxes / sales
        ) %>%
        ggplot(
            aes(
                x = factor(year),
                y = share_sales_tax,
                group = factor(sic_3),
                color = factor(sic_3),
                alpha = sic_3_alpha
            )
        ) +
        geom_vline(
            xintercept = c("83","86","90"),
            colour = "lightgray", #my_colors[["gray"]],
            linetype = "dashed"
        )+
        stat_summary(
            fun = "mean",
            na.rm = TRUE,
            geom = "point",
            shape = 18,
            size = 4#,
            # color = my_colors[["purple"]]
        ) + # add mean points
        stat_summary(
            fun.data = mean_cl_normal,
            geom = "errorbar",
            width = 0.1,
            # color = my_colors[["purple"]],
            show.legend = FALSE
        ) + # add CI bars
        scale_color_manual(values = inds_highlight)+
        theme_classic() +
        theme(legend.position = 'none')

### Plot individually by years 83-86


colombia_data_frame %>%
    # filter(sic_3!="353") %>%
    # mutate(
    #     sic_3_alpha = ifelse(sic_3 == names(inds_highlight), 1, 0.5)#,
    #     # sic_3_color_group = ifelse(sic_3 == inds_highlight, 1, 0)
    # ) %>%
    # group_by(sic_3,year) %>%
    # mutate(
    #     share_sale_tax_sales = sales_taxes / sales
    # ) %>%
    filter(
        sic_3=="390",
        # year %in% 82:86
    ) %>%
    ggplot(
        aes(
            x = factor(year),
            y = share_sales_tax,
            # group = factor(sic_3),
            color = factor(sic_3),
            # alpha = sic_3_alpha
        )
    ) +
    # geom_vline(
    #     xintercept = c("84","85"),
    #     colour = "lightgray", #my_colors[["gray"]],
    #     linetype = "dashed"
    # )+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        shape = 18,
        size = 4#,
        # color = my_colors[["purple"]]
    ) + # add mean points
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar",
        width = 0.1,
        # color = my_colors[["purple"]],
        show.legend = FALSE
    ) + # add CI bars
    scale_color_manual(values = "blue")+
    theme_void() +
    theme(legend.position = 'none')

### Creating function for Tinytable

f <- function(d,...){
    d %>%
        filter(
            # sic_3=="313",
            year %in% 82:86
        ) %>%
        ggplot(
            aes(
                x = factor(year),
                y = share_sales_tax,
                color = factor(sic_3),
            )
        ) +
        geom_vline(
            xintercept = c("84","85"),
            colour = "lightgray", 
            linetype = "dotted",
            size = 0.2
        )+
        stat_summary(
            fun = "mean",
            na.rm = TRUE,
            geom = "point",
            shape = 18,
            size = 1
        ) + # add mean points
        stat_summary(
            fun.data = mean_cl_normal,
            geom = "errorbar",
            width = 0.2,
            size = 0.1,
            show.legend = FALSE
        ) + # add CI bars
        scale_color_manual(values = "blue")+
        theme_void() +
        theme(legend.position = 'none')
}

## Tinytable ##### 

#### Top 20 #### 

plot_data <- colombia_data_frame %>%
    ungroup() %>%
    filter(
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    mutate(
        sic_3 = factor(sic_3, levels = top_20_inds$sic_3)
    ) %>%
    select(plant, juridical_organization, sic_3, year, share_sales_tax)

split_plot_data <- plot_data %>%
    split(plot_data$sic_3)

top_20_inds[,c(1,7,2:6)] |>
    cbind(
        data.frame(
            "Annual Sales Tax" = ""#,
            # check.names = FALSE
        )
    ) |>
    tt() |>
    plot_tt(j=8, fun = f, data = split_plot_data, height = 4) |>
    style_tt(j=8, width = 24)

 top_20_inds[,c(1,10,7:9)] %>%
    cbind(
        data.frame(
            "Annual Sales Tax (82-86)" = "",
            check.names = FALSE
        )
    ) |>
    tt() |>
    plot_tt(j=6, fun = f, data = split_plot_data, height = 4) |>
    style_tt(j=6, align = "c", width = 20) |>
    format_tt()

#### All ####

plot_data_all <- colombia_data_frame %>%
    ungroup() %>%
    # filter(
    #     sic_3 %in% top_20_inds$sic_3
    # ) %>%
    select(plant, juridical_organization, sic_3, year, share_sales_tax) |>
    split(colombia_data_frame$sic_3)

sales_tax_change %>%
    cbind(
        data.frame(
            "Annual Sales Tax" = "",
            check.names = FALSE
        )
    ) |>
    tt() |>
    plot_tt(j=6, fun = f, data = plot_data_all, height = 4) |>
    style_tt(j=6, align = "c", width = 20)

## D-in-D #####

## diff-in-diff
# y_jt =  \theta_i + \lambda_t + 11[after]*11[treat] + \phi(k,l,m) + epsilon_jt
    colombia_data_frame %>%
        ungroup() %>%
        left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
        left_join(sales_tax_change) %>%
        filter(
            # year <= 86,
            JO_class != "Other",
            juridical_organization != 6,
            # juridical_organization %in% c(0, 1, 3)
            # year >= 83
        ) %>%
        mutate(
            juridical_organization = factor(juridical_organization),
            metro_area_code = factor(metro_area_code),
            JO_class = factor(
                JO_class, 
                levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
            ),
            time = case_when(
                year <= 83 ~ "before",
                .default = as.character(year),
            ),
            time = factor(time, levels = c("before","84","85","86")),
            treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
            treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
            treat = factor(treat, levels = c("Corp","Non-Corp")),
            treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
            year = relevel(factor(year), ref="82")
        ) %>%
        filter(
            # Change == "decreased",
            sic_3 == 322
        ) %>%
        feols( share_sales_tax~ year*treat+polym(m, k, l, degree = 2, raw = TRUE)
            | plant,
            data = ., cluster=~year#~plant+year
        ) %>%
        etable()
    colombia_data_frame %>%
        ungroup() %>%
        left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
        left_join(sales_tax_change) %>%
        filter(
            # year <= 86,
            JO_class != "Other",
            juridical_organization != 6,
            # juridical_organization %in% c(0, 1, 3)
            # year >= 83
        ) %>%
        mutate(
            juridical_organization = factor(juridical_organization),
            metro_area_code = factor(metro_area_code),
            JO_class = factor(
                JO_class, 
                levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
            ),
            time = case_when(
                year <= 83 ~ "before",
                .default = as.character(year),
            ),
            time = factor(time, levels = c("before","84","85","86")),
            treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
            treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
            treat = factor(treat, levels = c("Corp","Non-Corp")),
            treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
            year = relevel(factor(year), ref="82")
        ) %>%
        filter(
            # Change == "decreased",
            sic_3 == 322
        ) %>%
        feols( sw(log_share,log(mats_serv/sales),log((materials+deductible_expenses)/sales),log(materials/sales))~ year*treat+share_sales_tax+polym(k, l, degree = 2, raw = TRUE)
            | plant,
            data = ., cluster=~year#~plant+year
        ) %>%
        etable()

colombia_data_frame %>%
    ungroup() %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    left_join(sales_tax_change) %>%
    filter(
        # year <= 86,
        JO_class != "Other",
        juridical_organization != 6,
        # juridical_organization %in% c(0, 1, 3)
        # year >= 83
        sic_3 %in% c(356,311)
    ) %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        metro_area_code = factor(metro_area_code),
        JO_class = factor(
            JO_class, 
            levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
        ),
        time = case_when(
            year <= 83 ~ "before",
            .default = as.character(year),
        ),
        time = factor(time, levels = c("before","84","85","86")),
        treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
        treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
        treat = factor(treat, levels = c("Corp","Non-Corp")),
        treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
        year = relevel(factor(year), ref="82"),
        sic_3 = relevel(factor(sic_3), ref="311")
    ) %>%
    feols( sw(log_share,log(mats_serv/sales),log((materials+deductible_expenses)/sales),log(materials/sales))~ year*treat*sic_3+share_sales_tax+polym(k, l, degree = 2, raw = TRUE)
        | plant,
        data = ., cluster=~year#~sic_3+year##~plant#
    ) %>%
    etable(
        keep = c("^share","^year\\d+ x treatNon-Corp x sic_\\d{4}","^treatNon-Corp$")
    )
### Diff-in-diff by industry #####

tax_changes_inds_regs<-lapply(
    unique(colombia_data_frame$sic_3),
    \(x)
    colombia_data_frame %>%
        ungroup() %>%
        left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
        left_join(sales_tax_change) %>%
        filter(
            # year <= 86,
            JO_class != "Other",
            juridical_organization != 6,
            # juridical_organization %in% c(0, 1, 3)
            # year >= 83
        ) %>%
        mutate(
            juridical_organization = factor(juridical_organization),
            metro_area_code = factor(metro_area_code),
            JO_class = factor(
                JO_class, 
                levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
            ),
            time = case_when(
                year <= 83 ~ "before",
                .default = as.character(year),
            ),
            time = factor(time, levels = c("before","84","85","86")),
            treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
            treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
            treat = factor(treat, levels = c("Corp","Non-Corp")),
            treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
            year = relevel(factor(year), ref="83")
        ) %>%
        filter(
            # Change == "decreased",
            sic_3 == x
        ) %>%
        feols( share_sales_tax~ year*treat+polym(m, k, l, degree = 2, raw = TRUE)
            | plant,
            data = ., cluster=~year
        )

)
etable(
    tax_changes_inds_regs,
    keep = c("^treatNon-Corp|year8(4|5|6) x treatNon-Corp","year8(4|5|6)"),
    headers = list(
            Industry=paste0(unique(colombia_data_frame$sic_3))#,
            # `Tax Change`=paste0(top_20_inds$Change)
        )
)

reg_ta_tbls<-lapply(
    seq(1,16,4),
    \(x) etable(
    tax_changes_inds_regs[x:(x+3)],
    keep = c("%year8(4|5|6)","%^treatNon-Corp"),
    headers = list(
            Industry=paste0(top_20_inds$sic_3)[x:(x+3)],
            `Tax Change`=paste0(top_20_inds$Change)[x:(x+3)]
    )#,
    # div.class = "table"
    )
)
reg_ta_tbls[[1]]
log_s_inds_regs<-lapply(
    unique(colombia_data_frame$sic_3),
    \(x)
    colombia_data_frame %>%
        ungroup() %>%
        left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
        left_join(sales_tax_change) %>%
        filter(
            # year <= 86,
            JO_class != "Other",
            juridical_organization != 6,
            # juridical_organization %in% c(0, 1, 3)
            # year >= 83
        ) %>%
        mutate(
            juridical_organization = factor(juridical_organization),
            metro_area_code = factor(metro_area_code),
            JO_class = factor(
                JO_class, 
                levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
            ),
            time = case_when(
                year <= 83 ~ "before",
                .default = as.character(year),
            ),
            time = factor(time, levels = c("before","84","85","86")),
            treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
            treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
            treat = factor(treat, levels = c("Corp","Non-Corp")),
            treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
            year = relevel(factor(year), ref="82")
        ) %>%
        filter(
            # Change == "decreased",
            sic_3 == x
        ) %>%
        feols( sw(log_share,log(mats_serv/sales),log((materials+deductible_expenses)/sales),log(materials/sales))~ year*treat+share_sales_tax+polym(k, l, degree = 2, raw = TRUE)
            | plant,
            data = ., cluster=~year#~plant+year
        )
)
# etable(
#     log_s_inds_regs,
#     keep = c("year8(3|4|5|6)")
# )
lapply(
    1:length(unique(colombia_data_frame$sic_3)[1:10]),
    \(x) etable(
    c(tax_changes_inds_regs[x],log_s_inds_regs[x]),
    keep = c("year8(3|4|5|6)","%^share"),
    headers = list(
            Industry=paste0(unique(colombia_data_frame$sic_3))[x]#,
            # `Tax Change`=paste0(top_20_inds$Change)[x]
        )
    )
)
lapply(
    seq(1,16,2),
    \(x) etable(
    c(tax_changes_inds_regs[x],log_s_inds_regs[x], tax_changes_inds_regs[x+1],log_s_inds_regs[x+1]),
    keep = c("year8(3|4|5|6)","%^share"),
    headers = list(
            Industry=rep(paste0(top_20_inds$sic_3)[x],paste0(top_20_inds$sic_3)[x+1], each=2),
            `Tax Change`=rep(paste0(top_20_inds$Change)[x],paste0(top_20_inds$Change)[x+1], each=2)
        )
    )
)



# top_20_inds %>% View()
did_3_inds <- lapply(
    setdiff(top_20_inds$sic_3,c(311,312)),
    \(x)
    colombia_data_frame %>%
    ungroup() %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    left_join(sales_tax_change) %>%
    filter(
        # year <= 86,
        JO_class != "Other",
        juridical_organization != 6,
        # juridical_organization %in% c(0, 1, 3)
        # year >= 83
        sic_3 %in% c(x,311)#c(x,311,312) #
    ) %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        metro_area_code = factor(metro_area_code),
        JO_class = factor(
            JO_class, 
            levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
        ),
        time = case_when(
            year <= 83 ~ "before",
            .default = as.character(year),
        ),
        time = factor(time, levels = c("before","84","85","86")),
        treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
        treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
        treat_3 = ifelse(sic_3==x,"tax_treat","exempt_ctrl"),
        treat = factor(treat, levels = c("Corp","Non-Corp")),
        treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
        year = relevel(factor(year), ref="83"),
        sic_3 = relevel(factor(sic_3), ref="311"),
        treat_3 = factor(treat_3, levels = c("exempt_ctrl","tax_treat"))
    ) %>%
    feols( sw(log_share,log(mats_serv/sales),log((materials+deductible_expenses)/sales),log(materials/sales))~ year*treat*sic_3+share_sales_tax+polym(k, l, degree = 2, raw = TRUE)
        | plant+year,
        data = ., cluster=~year#~plant+year
    )
)

names(did_3_inds)<-paste0(setdiff(top_20_inds$sic_3,c(311,312)))
lapply(
   paste0(setdiff(industries$sic_3,c(311,312)))[1:5], #11:15,#6:10,#1:5,#26:27,#21:25,#16:20,#
    \(x) etable(
        did_3_inds[x],
        keep = c(
            "^share",
            "%^year8(4|5|6):treatNon-Corp:sic_\\d{4}",
            # "%^year8(4|5|6):treatNon-Corp:treat_3\\w+",
            "^treatNon-Corp$"
        ),
        headers = list(
            Industry=x,#paste0(setdiff(unique(colombia_data_frame$sic_3),c(311,312)))[x]#,
            `Tax Change`=paste0(top_20_inds[top_20_inds$sic_3==x,'Change'][[1]])
        )
    )
)
### Diff-in-Diff by tax rate change ######
colombia_data_frame %>%
    ungroup() %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    left_join(sales_tax_change) %>%
    filter(
        # year <= 86,
        JO_class != "Other",
        juridical_organization != 6,
        # year >= 82
    ) %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        metro_area_code = factor(metro_area_code),
        JO_class = factor(
            JO_class, 
            levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
        ),
        time = case_when(
            year <= 83 ~ "before",
            .default = as.character(year),
        ),
        time = factor(time, levels = c("before","84","85","86")),
        treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
        treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
        treat = factor(treat, levels = c("Corp","Non-Corp")),
        treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
        year = relevel(factor(year), ref="82")
    ) %>%
    filter(
        Change == "decreased",
        # sic_3 == 324 
    ) %>%
    feols( sw(log_share, share_sales_tax)~ year*JO_class+polym(m, k, l, degree = 2, raw = TRUE)
        | sic_3+metro_area_code,
        data = ., cluster=~plant+year
    ) %>% 
    etable(
    )
## Diff-in-diff by industry #####
log_s_JO_regs<-lapply(
    unique(
        top_20_inds$Change
    ),
    \(x)
    colombia_data_frame %>%
        ungroup() %>%
        left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
        left_join(sales_tax_change) %>%
        filter(
            # year <= 86,
            JO_class != "Other",
            juridical_organization != 6,
            # year >= 82
        ) %>%
        mutate(
            juridical_organization = factor(juridical_organization),
            metro_area_code = factor(metro_area_code),
            JO_class = factor(
                JO_class, 
                levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
            ),
            time = case_when(
                year <= 83 ~ "before",
                .default = as.character(year),
            ),
            time = factor(time, levels = c("before","84","85","86")),
            treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
            treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
            treat = factor(treat, levels = c("Corp","Non-Corp")),
            treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
            year = relevel(factor(year), ref="82")
        ) %>%
        filter(
            Change == x,
            # sic_3 == x
        ) %>%
        feols( log_share~ year*JO_class+share_sales_tax+polym(m, k, l, degree = 2, raw = TRUE)
            | sic_3+metro_area_code,
            data = ., cluster=~plant+year
        ) #%>% 
        # etable(
        # )
)
# etable(
#     log_s_JO_regs,
#     keep = c("sales","year8(3|4|5|6)")
# )
log_s_tax_change_regs<-lapply(
    unique(
        top_20_inds$Change
    ),
    \(x)
    colombia_data_frame %>%
        ungroup() %>%
        left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
        left_join(sales_tax_change) %>%
        filter(
            # year <= 86,
            JO_class != "Other",
            juridical_organization != 6,
            # year >= 82
        ) %>%
        mutate(
            juridical_organization = factor(juridical_organization),
            metro_area_code = factor(metro_area_code),
            JO_class = factor(
                JO_class, 
                levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
            ),
            time = case_when(
                year <= 83 ~ "before",
                .default = as.character(year),
            ),
            time = factor(time, levels = c("before","84","85","86")),
            treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
            treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
            treat = factor(treat, levels = c("Corp","Non-Corp")),
            treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
            year = relevel(factor(year), ref="82")
        ) %>%
        filter(
            Change == x,
            # sic_3 == x
        ) %>%
        feols( log_share~ year*treat+share_sales_tax+polym(m, k, l, degree = 2, raw = TRUE)
            | sic_3+metro_area_code,
            data = ., cluster=~plant+year
        ) #%>% 
        # etable(
        # )
)
# etable(
#     log_s_tax_change_regs,
#     keep = c("sales","year8(3|4|5|6)")
# )
## Diff-in-Diff-in-Diff

DiDiD<-colombia_data_frame %>%
    ungroup() %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    left_join(sales_tax_change) %>%
    filter(
        # year <= 86,
        # year >= 82,
        juridical_organization != 6,
        JO_class != "Other",
    ) %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        # sic_3 = factor(sic_3),
        metro_area_code = factor(metro_area_code),
        JO_class = factor(JO_class, levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")),
        # year = factor(year),
        time = case_when(
            year <= 83 ~ "before",
            .default = as.character(year),
        ),
        time = factor(time, levels = c("before","84","85","86")),
        treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
        treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
        treat = factor(treat, levels = c("Corp","Non-Corp")),
        treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
        treat_3 = factor(Change, levels = c("decreased","no change","exempt", "increased")),
        year = relevel(factor(year), ref="82")
    ) %>%
    filter(
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    feols( 
        sw(
            log_share,log(mats_serv/sales),
            log((materials+deductible_expenses)/sales),
            log(materials/sales)
        )~ year*treat*treat_3+csw(share_sales_tax)
        +polym(m, k, l, degree = 2, raw = TRUE)
        | sic_3+metro_area_code,
        data = ., cluster=~plant+year
    )
etable(
    DiDiD#,
    # keep = c("%^share","^year8(3|4|5|6) x treatNon-Corp x treat_3(exempt|nochange|increased)")#,    #,"^treatNon-Corp")
)

etable(
    DiDiD,
    keep = c("%^share","^year8(3|4|5|6) . treatNon-Corp x"),
    div.class = "table"
)
DiDiD_jo<-colombia_data_frame %>%
    ungroup() %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    left_join(sales_tax_change) %>%
    filter(
        # year <= 86,
        # year >= 82,
        juridical_organization != 6,
        JO_class != "Other",
    ) %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        # sic_3 = factor(sic_3),
        metro_area_code = factor(metro_area_code),
        JO_class = factor(JO_class, levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")),
        # year = factor(year),
        time = case_when(
            year <= 83 ~ "before",
            .default = as.character(year),
        ),
        time = factor(time, levels = c("before","84","85","86")),
        treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
        treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
        treat = factor(treat, levels = c("Corp","Non-Corp")),
        treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
        treat_dec = factor(Change, levels = c("decreased","no change","exempt", "increased")),
        treat_exe = factor(Change, levels = c("exempt","decreased","no change", "increased")),
        treat_noc = factor(Change, levels = c("no change","exempt","decreased", "increased")),
        year = relevel(factor(year), ref="82")
    ) %>%
    filter(
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    feols( sw(log_share,log(mats_serv/sales),log(materials/sales))~ year*JO_class*treat_dec+share_sales_tax+polym(m, k, l, degree = 2, raw = TRUE)
        | plant, #sic_3+metro_area_code,
        data = ., cluster=~plant+year
    )


etable(
    DiDiD_jo#,
    # keep = c("sales","^year8(3|4|5|6) x JO_class(\\w+|Ltd.Co.) x treat_\\w+"),
    # headers = list(
    #     Industry=paste0(top_20_inds[1:5,"sic_3"]),
    #     `Tax Change`=top_20_inds[1:5,"Change"]
    #     ),
)

colombia_data_frame |> names()

    
##

sales_tax_change %>%
    left_join(
        ciiu_3[,2:3],
        by = join_by(sic_3 == `Código`)
    ) %>% 
    mutate(
        description = str_remove(`Descripción`,"Manufacture of |Manufacture ot "),
        description = str_replace_all(
            str_to_title(description),
            c(" And " = " and ", " Of " = " of ", " Or | Ord " = " or ")
        ),
    ) %>%
    filter(
        Perry != " "
    ) %>%
    select(
        Perry_inds_desc, Perry,sic_3, description
    )

income_tax_reforms <- tribble(
    ~JO, ~Year,~Change,
    "Individuals", 1983, "8% increase in most scales; Max was reduced from 56 to 49%",
    "Ltd. Co.", 1983, "Reduction from 20% to 18%; Now subject to presumptive income",
    "Individuals" , 1986, "The top tax rate applied to individual income was reduced from 49 to 30%.",
    "Ltd. Co.", 1986, "Increased from 18 to 30%",
    "Corporations", 1986, "Decreased from 40 to 30%"
)

## Tidy function to sum rows and exlcude NAs -- didn't work
sum_na_rows_tidy <- function(data, vars, expr){
    data %>%
        ungroup() %>% rowwise() %>%
        mutate(
            "sum_{{expr}}" := sum(c_across(all_of(vars)), na.rm = TRUE)
        ) %>%
        ungroup()
}

sum_rows_tidy <- function(data, vars){
    data %>%
        rowwise() %>%
        mutate(total = sum(c_across(all_of(vars)), na.rm = TRUE)) %>%
        ungroup()
}

colombia_data_frame %>%
    mutate(
        test = rowSums(cbind(deductible_expenses,-non_deductible_expenses), na.rm = TRUE),
        test_2 = rowSums(cbind(deductible_expenses,non_deductible_expenses), na.rm = TRUE)
    ) %>%
    select(
        test, test_2
    )
colombia_data_frame %>%
    sum_na_rows_tidy(c("deductible_expenses", "non_deductible_expenses"),expenses)
# Uso de la función
df <- sum_rows_tidy(df, c("rland_80", "rbldg_80", "rmach_80", "rtrans_80", "roffice_80"))