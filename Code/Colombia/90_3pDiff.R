# Loading packages ####
library(tidyverse)
library(fixest)

# Loading data ####

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

# Triple Diff-in-Diff ####

did_3_inds <- lapply(
    setdiff(top_20_inds$sic_3,c(311,312)),
    \(x)
    colombia_data_frame %>%
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
        treat = ifelse(juridical_organization==3,"Corp","Non-Corp"),
        treat_2 = ifelse(JO_class=="Corporation","Corp","Non-Corp"),
        treat_3 = ifelse(sic_3==x,"tax_treat","exempt_ctrl"),
        treat = factor(treat, levels = c("Corp","Non-Corp")),
        treat_2 = factor(treat_2, levels = c("Corp","Non-Corp")),
        year = relevel(factor(year), ref="83"),
        sic_3 = relevel(factor(sic_3), ref="311"),
        treat_3 = factor(treat_3, levels = c("exempt_ctrl","tax_treat"))
    ) %>%
    feols( sw(log_share,log(energy_non_deduct_share),log(mats_deduct_share))~ year*treat*sic_3+share_sales_tax+polym(k, l, degree = 2, raw = TRUE)
        | plant+year,
        data = ., cluster=~year#~plant+year
    )
)

names(did_3_inds)<-paste0(setdiff(top_20_inds$sic_3,c(311,312)))
invisible(
for (x in paste0(setdiff(industries$sic_3,c(311,312)))[1:10]){
    etable(
        did_3_inds[[x]],
        keep = c(
            # "%^share",
            "%^year8(4|5|6):treatNon-Corp:sic_\\d{4}"#,
            # "%^year8(4|5|6):treatNon-Corp:treat_3\\w+",
            # "%^treatNon-Corp$"
        ),
        headers = list(
            Industry=paste0(
                str_sub(
                    paste0(
                        top_20_inds[top_20_inds$sic_3==x,'description'][[1]]
                    ),
                    1,30
                ),
                "..."
            ),#x,#paste0(setdiff(unique(colombia_data_frame$sic_3),c(311,312)))[x]#,
            `Tax Change`=str_to_title(
                paste0(
                    top_20_inds[top_20_inds$sic_3==x,'Change'][[1]]
                )
            )
        ),
        div.class = "table"
    ) |> print()
}
)
