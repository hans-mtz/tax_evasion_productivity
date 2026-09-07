## %% Load data and packages ------------------------------------------------

library(tidyverse)
library(fixest)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData") # top_20_inds
load("Code/Products/deconv_funs.Rdata")

## %% Defining vars ------------------------------------------------

upper_cut <- 0.75

## %% 384, 382 --------------------------------------------------
# According to some records, these industries were exempted from 
# sales taxes before 1984. The data shows sales tax rates increasing
# significantly in 1984, but they are not zero. 
# 

colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        # sic_3 %in% top_20_inds$sic_3,
        sic_3 %in% 381:384
    ) %>%
    mutate(
        # total_gross_output = sum(gross_output, na.rm = TRUE),
        total_sales = sum(sales, na.rm = TRUE),
        total_deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE),
        total_materials = sum(materials, na.rm = TRUE),
        total_share_exports = mean(share_exports, na.rm = TRUE),
        total_share_imports = mean(share_imports, na.rm = TRUE),
        total_share_imports_materials = mean(share_imports_materials, na.rm = TRUE),
        total_share_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        importer = ifelse(share_imports > 0, 1, 0),
        exporter = ifelse(share_exports > 0, 1, 0),
        importer_mats = ifelse(share_imports_materials > 0, 1, 0),
        Corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        
    ) %>%
    mutate(
        avg_age_plant = mean(age, na.rm = TRUE),
        # y1983 = factor(ifelse(year<=83, "before","after"), levels = c("before","after")),
        y1984 = factor(ifelse(year<=84, "before","after"), levels = c("before","after")),
        y1985 = factor(ifelse(year<=85, "before","after"), levels = c("before","after")),
        y1986 = factor(ifelse(year<=86, "before","after"), levels = c("before","after")),
        .by = c(plant)
    ) %>%
    group_by(sic_3, year) %>%
    summarise(
        n = unique(plant) |> length(),
        n_Corp = unique(plant*as.numeric(juridical_organization==3)) |> length()-1L,
        # avg_age = mean(age, na.rm = TRUE),
        sales_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        purchases_sales_tax = mean(sales_tax_purchases, na.rm = TRUE),
        # gross_output = sum(gross_output, na.rm = TRUE)/max(total_gross_output, na.rm = TRUE),
        # sales = sum(sales, na.rm = TRUE)/max(total_sales, na.rm = TRUE),
        # # deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE)/max(total_deductible_intermediates, na.rm = TRUE),
        # # materials = sum(materials, na.rm = TRUE)/max(total_materials, na.rm = TRUE),
        # exporters = mean(exporter, na.rm = TRUE),
        # share_exports = mean(share_exports, na.rm = TRUE),
        # # share_imports = mean(share_imports, na.rm = TRUE),
        # importers_mats = mean(importer_mats, na.rm = TRUE),
        # share_imports_materials = mean(share_imports_materials, na.rm = TRUE)
        # importers = mean(importer, na.rm = TRUE),
    ) %>%
    mutate(
        across(where(is.double), ~ round(.x*100, 1)),
        # avg_age = round(avg_age, 0),
    ) |> View()

tax_380s_tbl <-colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        # sic_3 %in% top_20_inds$sic_3,
        sic_3 %in% 381:384
    ) %>%
    mutate(
        # total_gross_output = sum(gross_output, na.rm = TRUE),
        total_sales = sum(sales, na.rm = TRUE),
        total_deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE),
        total_materials = sum(materials, na.rm = TRUE),
        total_share_exports = mean(share_exports, na.rm = TRUE),
        total_share_imports = mean(share_imports, na.rm = TRUE),
        total_share_imports_materials = mean(share_imports_materials, na.rm = TRUE),
        total_share_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        importer = ifelse(share_imports > 0, 1, 0),
        exporter = ifelse(share_exports > 0, 1, 0),
        importer_mats = ifelse(share_imports_materials > 0, 1, 0),
        Corp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp")),
        
    ) %>%
    mutate(
        avg_age_plant = mean(age, na.rm = TRUE),
        # y1983 = factor(ifelse(year<=83, "before","after"), levels = c("before","after")),
        y1984 = factor(ifelse(year<=84, "before","after"), levels = c("before","after")),
        y1985 = factor(ifelse(year<=85, "before","after"), levels = c("before","after")),
        y1986 = factor(ifelse(year<=86, "before","after"), levels = c("before","after")),
        .by = c(plant)
    ) %>%
    group_by(sic_3, y1984) %>%
    summarise(
        n = unique(plant) |> length(),
        n_Corp = unique(plant*as.numeric(juridical_organization==3)) |> length()-1L,
        # avg_age = mean(age, na.rm = TRUE),
        sales_sales_tax = mean(share_sales_tax, na.rm = TRUE),
        purchases_sales_tax = mean(sales_tax_purchases, na.rm = TRUE),
        # gross_output = sum(gross_output, na.rm = TRUE)/max(total_gross_output, na.rm = TRUE),
        # sales = sum(sales, na.rm = TRUE)/max(total_sales, na.rm = TRUE),
        # # deductible_intermediates = sum(deductible_intermediates, na.rm = TRUE)/max(total_deductible_intermediates, na.rm = TRUE),
        # # materials = sum(materials, na.rm = TRUE)/max(total_materials, na.rm = TRUE),
        # exporters = mean(exporter, na.rm = TRUE),
        # share_exports = mean(share_exports, na.rm = TRUE),
        # # share_imports = mean(share_imports, na.rm = TRUE),
        # importers_mats = mean(importer_mats, na.rm = TRUE),
        # share_imports_materials = mean(share_imports_materials, na.rm = TRUE)
        # importers = mean(importer, na.rm = TRUE),
    ) %>%
    mutate(
        across(where(is.double), ~ round(.x*100, 1)),
        # avg_age = round(avg_age, 0),
    )%>%
    pivot_wider(
        id_cols = sic_3,
        names_from = y1984,
        values_from = sales_sales_tax:purchases_sales_tax,
        names_sep = ":"
    )

tax_380s_tbl 

colombia_data_frame %>%
        filter(
            is.finite(y),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            log_mats_share > log(threshold_cut),
            # sic_3 %in% top_20_inds$sic_3,
            # sic_3 %in% 381:384
            sic_3 %in% c(311, 382)
            # sic_3 == 382,
            # year > 84
        ) %>%
        mutate(
            nonCorp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp"), levels=c("Corp","Non-Corp")),
            y1984 = case_when(
                year <= 84 ~ "before",
                year > 84 ~ "after",
                TRUE ~ NA_character_
            ) |> factor(levels = c("before","after")),
            sic_3 = factor(sic_3, labels = c("Exempt", "Treated") ),
            JO_class = factor(
                JO_class, 
                levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership")),
                year = relevel(factor(year), ref = "84")
        ) %>%
        fixest::feols(
            log_mats_share ~ y1984*JO_class*sic_3|year+factor(plant),#+factor(year)+factor(plant),
            cluster = ~year+factor(plant),
            data = .
        ) |> summary()

reg_380_tbl <- lapply(381:384, function(x){
    reg_temp <- colombia_data_frame %>%
        filter(
            is.finite(y),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            log_mats_share > log(threshold_cut),
            # sic_3 %in% top_20_inds$sic_3,
            # sic_3 %in% 381:384
            # sic_3 %in% c(311, 384)
            sic_3 == x,
            # year > 84
        ) %>%
        mutate(
            nonCorp = factor(ifelse(juridical_organization==3,"Corp","Non-Corp"), levels=c("Corp","Non-Corp")),
            y1984 = case_when(
                year <= 84 ~ "before",
                year > 84 ~ "after",
                TRUE ~ NA_character_
            ) |> factor(levels = c("before","after")),
            # sic_3 = factor(sic_3, labels = c("Exempt", "Treated") ),
            JO_class = factor(
                JO_class, 
                levels = c("Corporation", "Proprietorship", "Ltd. Co.", "Partnership"))
        ) %>%
        fixest::feols(
            log_mats_share ~ y1984*nonCorp,#+factor(year)+factor(plant),
            cluster = ~year+plant,
            data = .
        )
    return(reg_temp)
})

etable(reg_380_tbl, 
       dict = c(
        y1984after = "Post-1984", 
       `nonCorpNon-Corp` = "Non-Corp"),
       title = "Sales tax incidence, industries 381-384"#,
    #    fitstat = c("n", "r2"),
    #    se = "cluster"
       ) # |> 


## %% Save results ------------------------------------------------

save(tax_380s_tbl, reg_380_tbl, file = "Code/Products/251-380s-inds.RData")


## %% 369 --------------------------------------------------

colombia_data_frame %>%
        filter(
            is.finite(y),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            log_mats_share > log(threshold_cut),
            # log_mats_share < log(1.5),
            # sic_3 %in% top_20_inds$sic_3,
            # sic_3 %in% 381:384
            # sic_3 %in% c(311, 384)
            sic_3 == 369,
            # year > 84
        ) %>%
        # mutate(
        #     sic_4 = factor(ifelse(juridical_organization==3,"Corp","Non-Corp"))
        # ) %>%
        ggplot(aes(x=materials_share))+
        geom_density(aes(fill=TRUE, color=TRUE), alpha=0.2)+
        geom_histogram(aes(y=after_stat(density), fill=TRUE), bins=100, alpha=0.5)+
        theme_classic()+
        theme(legend.position = "none")+
        labs(
            title = "Industry 369 Non-Metallic Mineral Products",
            subtitle = "Materials share density and histogram; Trimming observations below 0.05",
            x = "Materials share",
            y = "Density"
        )

ggsave("Code/Products/density-369-under-trim.png", width = 6, height = 4)

colombia_data_frame %>%
        filter(
            is.finite(y),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            log_mats_share > log(threshold_cut),
            log_mats_share < log(upper_cut),
            # sic_3 %in% top_20_inds$sic_3,
            # sic_3 %in% 381:384
            # sic_3 %in% c(311, 384)
            sic_3 == 369,
            # year > 84
        ) %>%
        # mutate(
        #     sic_4 = factor(ifelse(juridical_organization==3,"Corp","Non-Corp"))
        # ) %>%
        ggplot(aes(x=materials_share))+
        geom_density(aes(fill=TRUE, color=TRUE), alpha=0.2)+
        geom_histogram(aes(y=..density.., fill=TRUE), bins=100, alpha=0.5)+
        theme_classic()+
        theme(legend.position = "none")+
        labs(
            title = "Industry 369 Non-Metallic Mineral Products",
            subtitle = "Materials share density and histogram;\nTrimming observations not in (0.05,0.7)",
            x = "Materials share",
            y = "Density",
            escape = FALSE
        )

ggsave("Code/Products/density-369-under-over-trim.png", width = 6, height = 4)


# colombia_data_frame %>%
#         filter(
#             is.finite(y),
#             is.finite(k),
#             is.finite(l),
#             is.finite(m),
#             log_mats_share > log(threshold_cut),
#             # log_mats_share < log(1.5),
#             # sic_3 %in% top_20_inds$sic_3,
#             # sic_3 %in% 381:384
#             # sic_3 %in% c(311, 384)
#             sic_3 == 369,
#             # year > 84
#         ) %>%
#         mutate(
#             sic_4 = factor(ifelse(juridical_organization==3,"Corp","Non-Corp"))
#         ) %>%
#         ggplot(aes(x=materials_share, color=sic_4, fill=sic_4))+
#         geom_density(aes(fill=sic_4, color=sic_4), alpha=0.5)+
#         geom_histogram(aes(y=..density..), bins=100, alpha=0.2)+
#         theme_classic()

sum_stats_369 <- colombia_data_frame %>%
        filter(
            is.finite(y),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            log_mats_share > log(threshold_cut),
            # log_mats_share < log(0.85),
            # sic_3 %in% top_20_inds$sic_3,
            # sic_3 %in% 381:384
            # sic_3 %in% c(311, 384)
            sic_3 == 369,
            # year > 84
        ) %>%
        summarise(
            obs = n(),
            n = unique(plant) |> length(),
            n_Corp = unique(plant*as.numeric(juridical_organization==3)) |> length()-1L,
            Min = min(materials_share, na.rm = TRUE),
            Q1 = quantile(materials_share, 0.25, na.rm = TRUE)[1],
            Median = median(materials_share, na.rm = TRUE),
            Q3 = quantile(materials_share, 0.75, na.rm = TRUE)[1],
            P90 = quantile(materials_share, 0.9, na.rm = TRUE)[1],
            P95 = quantile(materials_share, 0.95, na.rm = TRUE)[1],
            P99 = quantile(materials_share, 0.99, na.rm = TRUE)[1],
            P99.5 = quantile(materials_share, 0.995, na.rm = TRUE)[1],
            Max = max(materials_share, na.rm = TRUE)
        )


sum_stats_369_2trim <- colombia_data_frame %>%
        filter(
            is.finite(y),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            log_mats_share > log(threshold_cut),
            log_mats_share < log(upper_cut),
            # sic_3 %in% top_20_inds$sic_3,
            # sic_3 %in% 381:384
            # sic_3 %in% c(311, 384)
            sic_3 == 369,
            # year > 84
        ) %>%
        summarise(
            obs = n(),
            n = unique(plant) |> length(),
            n_Corp = unique(plant*as.numeric(juridical_organization==3)) |> length()-1L,
            Min = min(materials_share, na.rm = TRUE),
            Q1 = quantile(materials_share, 0.25, na.rm = TRUE)[1],
            Median = median(materials_share, na.rm = TRUE),
            Q3 = quantile(materials_share, 0.75, na.rm = TRUE)[1],
            P90 = quantile(materials_share, 0.9, na.rm = TRUE)[1],
            P95 = quantile(materials_share, 0.95, na.rm = TRUE)[1],
            P99 = quantile(materials_share, 0.99, na.rm = TRUE)[1],
            P99.5 = quantile(materials_share, 0.995, na.rm = TRUE)[1],
            Max = max(materials_share, na.rm = TRUE)
        )

# colombia_data_frame %>%
#         filter(
#             is.finite(y),
#             is.finite(k),
#             is.finite(l),
#             is.finite(m),
#             log_mats_share > log(threshold_cut),
#             log_mats_share < log(0.8)
#         ) %>%
#         first_stage_panel_me(369, "log_mats_share", "materials_share", data=.)[[1]]

save(
    sum_stats_369, sum_stats_369_2trim,
    file = "Code/Products/251-369-sum-stats.RData"
)
