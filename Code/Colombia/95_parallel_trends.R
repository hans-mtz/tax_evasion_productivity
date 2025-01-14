# Loading packages ####
library(tidyverse)
library(fixest)

# Loading data ####

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

# Parallel Trends ####

## Corp. vs. Non-Corp ####

parallel_trends<-lapply(
    setdiff(top_20_inds$sic_3,c(311,312)),
    # top_20_inds$sic_3,
    \(x)
    colombia_data_frame %>%
    left_join(sales_tax_change) %>%
    filter(
        # year <= 86,
        JO_class != "Other",
        juridical_organization != 6,
        # juridical_organization %in% c(0, 1, 3)
        # year >= 83
        # sic_3 %in% c(x,311)#c(x,311,312) #
        sic_3 == x
    ) %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        metro_area_code = factor(metro_area_code),
        mats_deduct_share = rowSums(cbind(materials,deductible_expenses), na.rm = TRUE)/sales,
        energy_non_deduct_share = rowSums(cbind(consumed_energy,non_deductible_expenses), na.rm = TRUE)/sales,
        JO_class = factor(
            JO_class, 
            levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")
        ),
        time = case_when(
            year <= 83 ~ "before",
            .default = as.character(year),
        ),
        time = factor(time, levels = c("before","84","85","86")),
        treat = ifelse(juridical_organization==3,"Corp","Non-Corp"), #Only Corporations
        treat_num = ifelse(juridical_organization==3,0,1),
        treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"), #Corporations and Stock Companies
        treat_3 = ifelse(sic_3==x,"tax_treat","exempt_ctrl"),
        treat = factor(treat, levels = c("Corp","Non-Corp")),
        treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
        # year = relevel(factor(year), ref="83"),
        # sic_3 = relevel(factor(sic_3), ref="311"),
        treat_3 = factor(treat_3, levels = c("exempt_ctrl","tax_treat"))
    ) %>%
    ggplot(
            aes(
                x = factor(year),
                y = mats_deduct_share,
                group = factor(treat),
                color = factor(treat)#,
                # alpha = sic_3_alpha
            )
        ) +
        geom_vline(
            xintercept = c("84","85","90"),
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
        # scale_color_manual(values = 'blue')+
        theme_classic()# +
        # theme(legend.position = 'none')
)

parallel_trends[[2]]
# 311 and 312 not signficant different
# Yes: 1, 3, 7, 10, 14, 15
# What about conditional differences

##

did_3_inds <- lapply(
    setdiff(top_20_inds$sic_3,c(311,312)),
    \(x)
    colombia_data_frame %>%
    ungroup() %>%
    # left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    left_join(sales_tax_change) %>%
    filter(
        # year <= 86,
        JO_class != "Other",
        juridical_organization != 6,
        # juridical_organization %in% c(0, 1, 3)
        # year >= 83
        sic_3 %in% c(x,311,312) #c(x,311)#
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
    feols( sw(log_share,log(mats_serv/sales),log((materials+deductible_expenses)/sales),log(materials/sales))~ year*treat*treat_3+share_sales_tax+polym(k, l, degree = 2, raw = TRUE)
        | plant+year,
        data = ., cluster=~year#~plant+year
    )
)

names(did_3_inds)<-paste0(setdiff(top_20_inds$sic_3,c(311,312)))
lapply(
#    paste0(setdiff(industries$sic_3,c(311,312)))[1:10], #11:15,#6:10,#1:5,#26:27,#21:25,#16:20,#
    names(did_3_inds),
    \(x) etable(
        did_3_inds[x],
        keep = c(
            "%^share",
            # "%^year8(4|5|6):treatNon-Corp:sic_\\d{4}",
            # "%^year8(4|5|6):treatNon-Corp:treat_3\\w+",
            # "%^year8(1|2|4|5|6):treatNon-Corp:treat_3\\w+_\\w+",
            "%^year\\d{2}:treatNon-Corp:treat_3\\w+_\\w+",
             "%^year\\d{2}:treatNon-Corp$",
            "%^treatNon-Corp$"
        ),
        headers = list(
            Industry=paste0(
                str_sub(
                    paste0(
                        top_20_inds[top_20_inds$sic_3==x,'description'][[1]]
                    ),
                    1,25
                ),
                "-"
            ),
            # Industry=x,#paste0(setdiff(unique(colombia_data_frame$sic_3),c(311,312)))[x]#,
            `Tax Change`=paste0(top_20_inds[top_20_inds$sic_3==x,'Change'][[1]])
        )
    )
)
coefplot(did_3_inds["322"][[1]], keep="%^year\\d{2}:treatNon-Corp:treat_3\\w+_\\w+")
etable(did_3_inds[[1]], keep="%^year\\d{2}:treatNon-Corp:treat_3\\w+_\\w+")
lapply(
   paste0(setdiff(industries$sic_3,c(311,312)))[1:10], #11:15,#6:10,#1:5,#26:27,#21:25,#16:20,#
    # names(did_3_inds),
    function(x) {
        # png(paste0("Paper/images/graphs/p_trends_",x,".png"), width=10*300, height=6*300, res=300)
        coefplot(
            did_3_inds[x][[1]],
            # x = NULL,#rep(c(-2,-1,1:8), each=4),
            # horiz=TRUE,
            keep = "%^year\\d{2}:treatNon-Corp:treat_3\\w+_\\w+",
            main = paste0(
                    str_sub(
                        paste0(
                            top_20_inds[top_20_inds$sic_3==x,'description'][[1]]
                        ),
                        1,25
                    ),
                    "-"
                )
        )
        legend("topright", col=1:4,lwd=2,legend=c("share","m+serv","m+ded","m"))
        # dev.off()
    }
)