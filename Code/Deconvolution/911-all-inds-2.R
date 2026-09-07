## %% load packages and data ---------------
library(tidyverse)
library(fixest)

load("Code/Products/test_data.RData")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

## %% Test is basically differences in means ---------------------
# Indicator variable by industry
# Indicator variable by Non-Corps (value should be positive if there is tax evasion)
# Interacitions between industry and Non-Corps would be test for overreporting due to tax evasion
# Other interesting variables, who is evading more?
#   - Smaller firms (measured in terms of capital, sales, employees, )
#   - Proprietorships, LLCs
#   - Exporters, importers
# These are also indicator variables. I would have interactions between these variables and industry indicators
# to see if there are differences in means between these groups and the rest of the firms

## %% David's suggestions ---------------------
# 1) Size across industries not within industries
# 2) Importers vs non-importers: Even though importers pay sales tax rate for their
# imported inputs, overreporting them is harder becuase there exists a
# greater paper trail.
# 3) By condition, whether a firms exports/imports or not and by amount, how much a firm
# exports/imports. 
# 4) Try size by quartiles or deciles instead of median split.

wip_df <- colombia_data_frame %>%
  filter(
    is.finite(y),
    is.finite(k),
    is.finite(l),
    is.finite(m),
    is.finite(log_mats_share),
    # log_mats_share < log(0.75),
    log_mats_share > log(threshold_cut),
    # sic_3 %in% top_20_inds
  ) %>%
  # group_by(sic_3) %>%
  mutate(
    corp = factor(ifelse(juridical_organization == 3, "Corp", "Other"), levels = c("Corp", "Other")),
    small_k = factor(ifelse(k < median(k, na.rm = TRUE), "Small K", "Large K"), levels = c("Large K","Small K")),
    small_l = factor(ifelse(l < median(l, na.rm = TRUE), "Small L", "Large L"), levels = c("Large L","Small L")),
    small_lag_m = factor(ifelse(lag_m < median(lag_m, na.rm = TRUE), "Small Lag M", "Large Lag M"), levels = c("Large Lag M","Small Lag M")),
    small_rev = factor(ifelse(gross_output < median(gross_output, na.rm = TRUE), "Small GO", "Large GO"), levels = c("Large GO", "Small GO")),
    jo = factor(JO_class, levels = c("Corporation", "Ltd. Co.", "Proprietorship", "Partnership", "Other")),
    jo = forcats::fct_collapse(
      jo,
      "Other" = c("Partnership", "Other")),
    exporter = factor(ifelse( share_exports > 0, "Exporter", "Non-Exporter"), levels = c("Exporter", "Non-Exporter")),
    importer = factor(ifelse( imported_inputs > 0, "Importer", "Non-Importer"), levels = c("Importer", "Non-Importer")),
    sic_3 = as.factor(sic_3),
    year = as.factor(year),
    plant = as.factor(plant),
    size_labor = case_when(
      l < quantile(l, 0.25, na.rm = TRUE) ~ "Smallest 25%",
      l >= quantile(l, 0.25, na.rm = TRUE) & l < quantile(l, 0.5, na.rm = TRUE) ~ "25-50%",
      l >= quantile(l, 0.5, na.rm = TRUE) & l < quantile(l, 0.75, na.rm = TRUE) ~ "50-75%",
      l >= quantile(l, 0.75, na.rm = TRUE) ~ "Largest 25%"
    ),
    size_labor = factor(size_labor, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%")),
    l_pct = percent_rank(l),
    k_pct = percent_rank(k),
    lag_rev_pct = percent_rank(lag_log_sales),
    lag_2_rev_pct = percent_rank(lag_2_log_sales),
    size_capital = case_when(
      k < quantile(k, 0.25, na.rm = TRUE) ~ "Smallest 25%",
      k >= quantile(k, 0.25, na.rm = TRUE) & k < quantile(k, 0.5, na.rm = TRUE) ~ "25-50%",
      k >= quantile(k, 0.5, na.rm = TRUE) & k < quantile(k, 0.75, na.rm = TRUE) ~ "50-75%",
      k >= quantile(k, 0.75, na.rm = TRUE) ~ "Largest 25%"
    ),
    size_capital = factor(size_capital, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%")),
    size_exports = case_when(
      share_exports < 0.25 ~ "Smallest 25%",
      share_exports >= 0.25 & exports < 0.5 ~ "25-50%",
      share_exports >= 0.5 & exports < 0.75 ~ "50-75%",
      share_exports >= 0.75 ~ "Largest 25%"
    ),
    size_exports = factor(size_exports, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%")),
    size_imports = case_when(
      share_imports < 0.25 ~ "Smallest 25%",
      share_imports >= 0.25 & share_imports < 0.5 ~ "25-50%",
      share_imports >= 0.5 & share_imports < 0.75 ~ "50-75%",
      share_imports >= 0.75 ~ "Largest 25%"
    ),
    size_imports = factor(size_imports, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%")),
    non_corp_small = interaction(corp,small_l, sep = ":"),
    non_corp_small = forcats::fct_collapse(
      non_corp_small,
      "Base" = c("Corp:Small L", "Corp:Large L","Other:Large L")
    ),
    non_corp_non_exporter = interaction(corp,exporter, sep = ":"),
    non_corp_non_exporter = forcats::fct_collapse(
      non_corp_non_exporter,
      "Base" = c("Corp:Exporter", "Corp:Non-Exporter","Other:Exporter")
    ),
    jo_small = interaction(jo,small_l, sep = ":"),
    jo_small = forcats::fct_collapse(
      jo_small,
      "Base" = c("Corporation:Small L", "Corporation:Large L","Ltd. Co.:Large L", "Proprietorship:Large L", "Other:Large L")
    ),
    jo_non_exporter = interaction(jo,exporter, sep = ":"),
    jo_non_exporter = forcats::fct_collapse(
      jo_non_exporter,
      "Base" = c("Corporation:Exporter", "Corporation:Non-Exporter","Ltd. Co.:Exporter", "Proprietorship:Exporter", "Other:Exporter")
    )
  )

## %% Regressions ---------------------

# Non-Corps by dichotomic characteristic:
# Small L and K, 
# Non-Exporter, Non-Importer

feols(
  log_mats_share ~ csw(corp,corp+i(corp,small_l, ref="Corp", ref2="Large L"),corp+i(corp,small_k, ref="Corp", ref2="Large K"),corp+i(corp, exporter, ref="Corp",ref2="Exporter"), corp+i(corp,importer,ref="Corp", ref2="Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

reg_ncrp_dic <- feols(
  log_mats_share ~ sw(corp, corp + i(corp, small_lag_m, ref="Corp", ref2="Large Lag M"), corp+i(corp,small_l, ref="Corp", ref2="Large L"),corp+i(corp,small_k, ref="Corp", ref2="Large K"),corp+i(corp, exporter, ref="Corp",ref2="Non-Exporter"), corp+i(corp,importer,ref="Corp", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) #|> etable(dict = dict)

feols(
  log_mats_share ~ csw(corp, corp + i(corp, small_lag_m, ref="Corp", ref2="Large Lag M"), corp+i(corp,small_l, ref="Corp", ref2="Large L"),corp+i(corp,small_k, ref="Corp", ref2="Large K"),corp+i(corp, exporter, ref="Corp",ref2="Non-Exporter"), corp+i(corp,importer,ref="Corp", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

feols(
  log_mats_share ~ csw(i(corp,year,ref="Corp"),i(corp,small_l, ref="Corp", ref2="Large L")+i(corp,year,ref="Corp"),i(corp,small_k, ref="Corp", ref2="Large K")+i(corp,year,ref="Corp"),i(corp, exporter, ref="Corp",ref2="Non-Exporter")+i(corp,year,ref="Corp"), i(corp,importer,ref="Corp", ref2="Non-Importer")+i(corp,year,ref="Corp"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

## Two measures of sieze: Labor and Capital. Preferred. Explain prob of detection independent of size.

feols(
  log_mats_share ~ csw(corp, corp+i(corp,small_l, ref="Corp", ref2="Large L"),corp+i(corp,small_k, ref="Corp", ref2="Large K"),corp+i(corp, exporter, ref="Corp",ref2="Non-Exporter"), corp+i(corp,importer,ref="Corp", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

## Choosing only measure of size, Capital is the candidate

feols(
  log_mats_share ~ csw(corp, corp+i(corp,small_k, ref="Corp", ref2="Large K"),corp+i(corp, exporter, ref="Corp",ref2="Non-Exporter"), corp+i(corp,importer,ref="Corp", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

feols(
  log_mats_share ~ sw(corp, corp+i(corp,small_l, ref="Corp", ref2="Large L"),corp+i(corp, exporter, ref="Corp",ref2="Non-Exporter"), corp+i(corp,importer,ref="Corp", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()


# reg_ncrp_dic |> etable(dict = dict)

reg_ncrp_dic_cum <- feols(
  log_mats_share ~ csw(corp, corp+i(corp,small_k, ref="Corp", ref2="Large K"),corp+i(corp, exporter, ref="Corp",ref2="Non-Exporter"), corp+i(corp,importer,ref="Corp", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) 

# reg_ncrp_dic_cum |> etable(dict = dict)

reg_ncrp_dic_1 <- feols(
  log_mats_share ~ sw(
    corp,
    corp+i(corp,small_l, ref="Corp", ref2="Large L"),#+i(corp, exporter, ref="Corp",ref2="Non-Exporter")+i(corp,importer,ref="Corp", ref2="Non-Importer"),
    corp+i(corp,small_k, ref="Corp", ref2="Large K"), #+i(corp, exporter, ref="Corp",ref2="Non-Exporter")+i(corp,importer,ref="Corp", ref2="Non-Importer"), 
    corp+i(corp, exporter, ref="Corp",ref2="Non-Exporter")+i(corp,importer,ref="Corp", ref2="Non-Importer")
    )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) #|> etable()


reg_ncrp_dic_2 <- feols(
  log_mats_share ~ sw(
    corp+i(corp,small_l, ref="Corp", ref2="Large L")+i(corp, exporter, ref="Corp",ref2="Non-Exporter")+i(corp,importer,ref="Corp", ref2="Non-Importer"),
    corp+i(corp,small_k, ref="Corp", ref2="Large K")+i(corp, exporter, ref="Corp",ref2="Non-Exporter")+i(corp,importer,ref="Corp", ref2="Non-Importer"), 
    corp+i(corp,small_l, ref="Corp", ref2="Large L")+i(corp,small_k, ref="Corp", ref2="Large K")+i(corp, exporter, ref="Corp",ref2="Non-Exporter")+i(corp,importer,ref="Corp", ref2="Non-Importer")
    )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) #|> etable()

etable(
  reg_ncrp_dic_1[-1],
  reg_ncrp_dic_2,
  dict = dict
)

# What makes sense:
# - Small firms in terms of labor overreport more. The larger the number of employees
# the harder to overreport because 1) more likely for someont to whistleblow 2) accounting departments
# might be larger and thus more people is involved in keeping double books, so the marginal
# cost of overreporting is larger.
# - Small firms in terms of capital overreport less. In Colombia, JOs are subjet to presumptive income,
# firms cannot report income below 8 percent of their assets (capital). Therefore, the smaller the capital,
# the lower the presumptive income, the lower the incentive to overreport.
# - Non-exporters overreport more. Exports are not subject to sales tax, so the larger the share of exports,
# the lower the incentive to overreport.
# - Non-importers overreport less. In addition to import tariffs, imports are subject to sales tax. Imports
# are also harder to overreport because of the larger paper trail. The larger the share of imports, the larger the
# the incentive to overreport local materials to compensate for the import tariffs paid.

# Non-Corps by size quartiles

feols(
  log_mats_share ~ csw(corp,corp+i(corp,size_labor, ref="Corp", ref2="Largest 25%"),corp+i(corp,size_capital, ref="Corp", ref2="Largest 25%"),corp+i(corp, size_exports, ref="Corp",ref2="Largest 25%"), corp+i(corp,size_imports,ref="Corp", ref2="Largest 25%"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

# Non-Corps by continuous size percentiles and export/import shares
feols(
  log_mats_share ~ sw(
    corp,
    corp+i(corp,lag_2_rev_pct, ref="Corp"),
    corp+i(corp,l_pct, ref="Corp"),
    corp+i(corp,k_pct, ref="Corp"),corp+i(corp, share_exports, ref="Corp"), corp+i(corp,share_imports,ref="Corp"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

feols(
  log_mats_share ~ csw(corp,i(corp,l_pct, ref="Corp"),corp+i(corp,k_pct, ref="Corp"),corp+i(corp, share_exports, ref="Corp")+i(corp, share_exports^2, ref="Corp"), corp+i(corp,share_imports,ref="Corp"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) 

feols(
  log_mats_share ~ sw(
    corp,
    corp+i(corp, lag_2_rev_pct, ref="Corp")+i(corp, lag_2_rev_pct^2, ref="Corp"),
    corp+i(corp,l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp"),
    corp+i(corp,k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp"),
    corp+i(corp, share_exports, ref="Corp")+i(corp, share_exports^2, ref="Corp")+i(corp,share_imports,ref="Corp")+i(corp,share_imports^2,ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

feols(
  log_mats_share ~ csw(
    corp,
    corp+i(corp,l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp"),
    corp+i(corp,k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp"),
    corp+i(corp, lag_rev_pct, ref="Corp")+i(corp, lag_rev_pct^2, ref="Corp"),
    corp+i(corp, lag_2_rev_pct, ref="Corp")+i(corp, lag_2_rev_pct^2, ref="Corp"),
    corp+i(corp, share_exports, ref="Corp")+i(corp, share_exports^2, ref="Corp")+i(corp,share_imports,ref="Corp")+i(corp,share_imports^2,ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

reg_ncrp_lev_1 <- feols(
  log_mats_share ~ sw(
    corp,corp+i(corp,l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp"),
    corp+i(corp,k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp"),
    corp+i(corp, share_exports, ref="Corp")+i(corp, share_exports^2, ref="Corp")+i(corp,share_imports,ref="Corp")+i(corp,share_imports^2,ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)

reg_ncrp_lev_2 <-feols(
  log_mats_share ~ sw(
    corp,corp+i(corp,l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp")+i(corp, share_exports, ref="Corp")+i(corp, share_exports^2, ref="Corp")+i(corp,share_imports,ref="Corp")+i(corp,share_imports^2,ref="Corp"),
    corp+i(corp,k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp")+i(corp, share_exports, ref="Corp")+i(corp, share_exports^2, ref="Corp")+i(corp,share_imports,ref="Corp")+i(corp,share_imports^2,ref="Corp"),
    corp+i(corp,l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp")+i(corp,k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp")+i(corp, share_exports, ref="Corp")+i(corp, share_exports^2, ref="Corp")+i(corp,share_imports,ref="Corp")+i(corp,share_imports^2,ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)

etable(
  reg_ncrp_lev_1[-1],
  reg_ncrp_lev_2[-1],
  dict = dict
)

reg_ncrp_lev |> etable()

reg_ncrp_lev_cum <- feols(
  log_mats_share ~ csw(corp,corp+i(corp,k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp"),corp+i(corp, share_exports, ref="Corp")+i(corp, share_exports^2, ref="Corp"), corp+i(corp,share_imports,ref="Corp"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) #|> etable()

reg_ncrp_lev_cum |> etable()




# What makes sense
# Similar story as before,
# - Increasing your size measured by labor by one percentile decreases overreporting
# - Increasing your by one percentile in size measured by capital increases overreporting
# - Increasing your share of imports increases overreporting
# - Increasing your share of exports decreases overreporting at a decreasing rate
# 97 percents of firms export 38% or less of their output. So the increase in overreoporting by 
# increasing your share of exports above 38% is true only for 3% percent of firms.


# JO by dichotomic characteristic:

feols(
  log_mats_share ~ csw(jo,jo+i(jo,small_l, ref="Corporation", ref2="Large L"),jo+i(jo,small_k, ref="Corporation", ref2="Large K"),jo+i(jo, exporter, ref="Corporation",ref2="Exporter"), jo+i(jo,importer,ref="Corporation", ref2="Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) 

reg_jo_dic <- feols(
  log_mats_share ~ sw(jo,jo+i(jo,small_l, ref="Corporation", ref2="Large L"),jo+i(jo,small_k, ref="Corporation", ref2="Large K"),jo+i(jo, exporter, ref="Corporation",ref2="Non-Exporter"), jo+i(jo,importer,ref="Corporation", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)#|> etable()

reg_jo_dic |> etable()

feols(
  log_mats_share ~ sw(jo,jo+i(jo,small_l, ref="Corporation", ref2="Large L"),jo+i(jo,small_k, ref="Corporation", ref2="Large K"),jo+i(jo, exporter, ref="Corporation",ref2="Non-Exporter"), jo+i(jo,importer,ref="Corporation", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)#|> etable()

reg_jo_dic_cum <- feols(
  log_mats_share ~ csw(jo,jo+i(jo,small_k, ref="Corporation", ref2="Large K"),jo+i(jo, exporter, ref="Corporation",ref2="Non-Exporter"), jo+i(jo,importer,ref="Corporation", ref2="Non-Importer"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)# |> etable()

# it looks like, LLCs are driving all the differences in overrreporting between the different characteristics.
# Proprietorships and Others do not seem to overreport more than Corporations.
# The signs for LLCs are in line with our priors:
# - Small LLCs in terms of capital overreport less because of presumptive income
# - Non-exporting LLCs overreport more because the larger the share of exports, 
#the lower the effective sales tax rate (exports are exempt of sales tax), the lower the incentive to overreport
# - Non-importing LLCs overreport less because imports are subject to sales tax and import tariffs. In addition imports
# have a larger paper trail. So the larger the share of imports, the lower the incentive to overreport local materials.

feols(
  log_mats_share ~ csw(jo,jo+i(jo,size_labor, ref="Corporation", ref2="Largest 25%"),jo+i(jo,size_capital, ref="Corporation", ref2="Largest 25%"),jo+i(jo, size_exports, ref="Corporation",ref2="Largest 25%"), jo+i(jo,size_imports,ref="Corporation", ref2="Largest 25%"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable()

reg_jo_lev<-feols(
  log_mats_share ~ sw(jo,jo+i(jo,l_pct, ref="Corporation")+i(jo,l_pct^2, ref="Corporation"),jo+i(jo,k_pct, ref="Corporation")+i(jo,k_pct^2, ref="Corporation"),jo+i(jo, share_exports, ref="Corporation")+i(jo, share_exports^2, ref="Corporation"), jo+i(jo,share_imports,ref="Corporation")+i(jo,share_imports^2,ref="Corporation"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) 


reg_jo_lev_cum <-feols(
  log_mats_share ~ csw(jo,jo+i(jo,l_pct, ref="Corporation")+i(jo,l_pct^2, ref="Corporation"),jo+i(jo,k_pct, ref="Corporation")+i(jo,k_pct^2, ref="Corporation"),jo+i(jo, share_exports, ref="Corporation")+i(jo, share_exports^2, ref="Corporation"), jo+i(jo,share_imports,ref="Corporation")+i(jo,share_imports^2,ref="Corporation"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) 

reg_jo_lev |> etable()


# reg_jo <- feols(
#   log_mats_share ~ sw(jo, jo+i(jo_small,"Base"), jo+i(jo_non_exporter,"Base"))| sic_3,
#   cluster = ~ plant + year,
#   data = wip_df
# ) 

# reg_jo |> etable()

dict <- c(
  "corpCorp" = "Corp",
  "corpOther" = "Non-Corp",
  "joCorporation" = "Corporation",
  "joLtd.Co." = "LLC",
  "joProprietorship" = "Proprietorship",
  "joPartnership" = "Partnership",
  "joOther" = "Other",
  "jo::Corporation" = "Corporation",
  "jo::Ltd.Co." = "LLC",
  "jo::Proprietorship" = "Proprietorship",
  "jo::Partnership" = "Partnership",
  "jo::Other" = "Other",
  "small_kSmallK" = "Small K",
  "small_kLargeK" = "Large K",
  "small_lSmallL" = "Small L",
  "small_lLargeL" = "Large L",
  "small_revSmallGO" = "Small GO",
  "small_revLargeGO" = "Large GO",
  "exporterExporter" = "Exporter",
  "exporterNon-Exporter" = "Non-Exporter",
  "importerImporter" = "Importer",
  "importerNon-Importer" = "Non-Importer",
  "as.factor(year)" = "Year (base 1981)",
  "year" = "Year",
  "log_mats_share" = "Materials Share of Revenue (Logs)",
  "sic_3" = "Industry (3-digit SIC)",
  "plant" = "Plant",
  "small_l::Small L" = "Small (L)",
  "small_k::Small K" = "Small (K)",
  "small_l=SmallL" = "Small (L)",
  "small_k=SmallK" = "Small (K)",
  "exporter::Non-Exporter" = "Non-Exporter",
  "importer::Non-Importer" = "Non-Importer",
  "jo_small::Ltd. Co." = "LLC",
  "jo_small::Proprietorship" = "Proprietorship",
  "jo_small::Partnership" = "Partnership",
  "jo_small::Other" = "Other",
  "jo_non_exporter::Ltd.Co." = "LLC",
  "jo_non_exporter::Proprietorship" = "Proprietorship",
  "jo_non_exporter::Partnership" = "Partnership",
  "jo_non_exporter::Other" = "Other",
  "non_corp_small::Other:SmallL" = "Non-Corp x Small",
  "non_corp_small::Other" = "Non-Corp",
  "non_corp_non_exporterOther:Non-Exporter" = "Non-Corp x Non-Exporter",
  "corp::Other" = "Non-Corp",
  "non_corp_non_exporter::Other" = "Non-Corp",
  `i(factor_var = non_corp_non_exporter, ref = "Base")` = "Non-Corp x Non-Exporter",
  `i(factor_var = non_corp_small, var = "Base", ref_special = TRUE)` = "Non-Corp x Small",
  `i(factor_var=non_corp_non_exporter,var="Base",ref_special=TRUE)` = "Non-Corp x Non-Exporter",
  `i(factor_var=corp,var=small_l,ref="Corp",ref2="LargeL")` = "Non-Corp x Small (L)",
  `i(factor_var=corp,var=small_k,ref="Corp",ref2="LargeK")` = "Non-Corp x Small (K)",
  `i(factor_var=corp,var=exporter,ref="Corp",ref2="Exporter")` = "Non-Corp x Non-Exporter",
  `i(factor_var=corp,var=importer,ref="Corp",ref2="Importer")` = "Non-Corp x Non-Importer",
  `i(factor_var=corp,var=exporter,ref="Corp",ref2="Non-Exporter")` = "Non-Corp x Exporter",
  `i(factor_var=corp,var=importer,ref="Corp",ref2="Non-Importer")` = "Non-Corp x Importer",
  "l_pct" = "Labor Percentile",
  "k_pct" = "Capital Percentile",
  "share_exports" = "Share of Exports",
  "share_imports" = "Share of Imports",
  `corp::Other:l_pct` = "Non-Corp x Labor Percentile",
  `i(factor_var=corp,var=k_pct,ref="Corp")` = "Non-Corp x Capital Percentile",
  `i(factor_var = corp, var = share_exports, ref = "Corp")` = "Non-Corp x Share of Exports",
  `i(factor_var = corp, var = I(share_exports^2), ref = "Corp")` = "Non-Corp x Share of Exports $^2$",
  `i(factor_var = corp, var = share_imports, ref = "Corp")` = "Non-Corp x Share of Imports",
  `I(share_exports^2) ` = "Share of Exports $^2$",
  `jo::Proprietorship:l_pct` = "Proprietorship x Labor Percentile",
  `jo::Proprietorship:k_pct` = "Proprietorship x Capital Percentile",
  `jo::Ltd. Co.:l_pct` = "LLC x Labor Percentile",
  `jo::Ltd. Co.:k_pct` = "LLC x Capital Percentile",
  `jo::Proprietorship:share_exports` = "Proprietorship x Share of Exports",
  `jo::Ltd. Co.:share_exports` = "LLC x Share of Exports",
  `jo::Proprietorship:I(share_exports^2)` = "Proprietorship x Share of Exports $^2$",
  `jo::Ltd. Co.:I(share_exports^2)` = "LLC x Share of Exports $^2$",
  `jo::Proprietorship:share_imports` = "Proprietorship x Share of Imports",
  `jo::Ltd. Co.:share_imports` = "LLC x Share of Imports",
  `jo::Ltd. Co.:small_l::Small L` = "LLC x Small (L)",
  `jo::Proprietorship:small_l::Small L` = "Proprietorship x Small (L)",
  `jo::Ltd. Co.:small_k::Small K` = "LLC x Small (K)",
  `jo::Proprietorship:small_k::Small K` = "Proprietorship x Small (K)"
)

reg_ncrp_dic |> etable(dict = dict)
reg_ncrp_lev |> etable(dict = dict)
reg_jo_dic[5] |> etable(dict = dict)
reg_jo_lev |> etable(dict = dict)

etable(
  reg_ncrp_dic[5], 
  reg_jo_dic[5],
  reg_ncrp_lev[5],
  reg_jo_lev[5],
  dict = dict
)

## %% Save results ---------------------

save(
  reg_ncrp_dic, reg_ncrp_lev, reg_jo_dic, reg_jo_lev,
  dict, wip_df, reg_jo_dic_cum, reg_jo_lev_cum,
  reg_ncrp_dic_cum, reg_ncrp_lev_cum,
  file = "Code/Products/911-all_inds-2.RData"
)

## %% Plotting the results ---------------------

# load("Code/Products/911-all_inds-2.RData")

# png(
#   file="Code/Products/911-all-dic-plot.png",
#   width=620, height = 620#,
#   # pointsize = 12
# )

# coefplot(
#   c(reg_ncrp_dic[5],
#   reg_jo_dic[5]),
#   drop = "Other",
#   order = c("Non-Corp","LLC","Ltd. Co.","Proprietorship", "Other"),
#   dict = dict,
#   # ylim = c(-1,1),
#   group = list(
#     "Non-Corp" = "^^(Non-Corp x|Non-Corp)",
#     "LLC" = "^^(jo::Ltd. Co..*:|LLC x|LLC)",
#     "Proprietorship" = "^^(jo::Proprietorship.*:|Proprietorship x|Proprietorship)"#,
#     # "Other" = "^^jo::Other.*:"
#   ),
#   horiz = TRUE#,
#   # ci_level = 0.90
# )

# dev.off()


# png(
#   file="Code/Products/911-all-lev-plot.png",
#   width=620, height = 620
# )

# coefplot(
#   c(reg_ncrp_lev[5],
#   reg_jo_lev[5]),
#   drop = "Other",
#   order = c("Non-Corp","LLC","Ltd. Co.","Proprietorship", "Other"),
#   dict = dict,
#   col = 1:5,
#   group = list(
#     "Non-Corp" = "^^(Non-Corp x|Non-Corp)",
#     "LLC" = "^^(jo::Ltd. Co..*:|LLC x|LLC)",
#     "Proprietorship" = "^^(jo::Proprietorship.*:|Proprietorship x|Proprietorship)"#,
#     # "Other" = "^^jo::Other.*:"
#   ),
#   horiz = TRUE#,
#   # ylim = c(-1,1)
#   # ci_level = 0.90
# )

# dev.off()

## %% Review Dec 19, 2025 ---------------------

# load("Code/Products/911-all_inds-2.RData")

# etable(
#   reg_ncrp_dic[-c(2,3)], reg_ncrp_dic_cum[4],
#   dict = dict
# )
# etable(
#   reg_ncrp_dic, reg_ncrp_dic_cum,
#   dict = dict
# )

# etable(
#   reg_ncrp_lev, reg_ncrp_lev_cum[length(reg_ncrp_lev_cum)],
#   dict = dict
# )
