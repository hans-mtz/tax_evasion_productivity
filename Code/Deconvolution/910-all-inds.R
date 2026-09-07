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




rdf <- colombia_data_frame %>%
  filter(
    is.finite(y),
    is.finite(k),
    # is.finite(l),
    is.finite(m),
    # log_mats_share < log(0.75),
    log_mats_share > log(threshold_cut),
    # sic_3 %in% top_20_inds
  ) %>%
  group_by(sic_3) %>%
  mutate(
    corp = factor(ifelse(juridical_organization == 3, "Corp", "Other"), levels = c("Corp", "Other")),
    small_k = factor(ifelse(k < median(k, na.rm = TRUE), "Small K", "Large K"), levels = c("Large K","Small K")),
    small_l = factor(ifelse(l < median(l, na.rm = TRUE), "Small L", "Large L"), levels = c("Large L","Small L")),
    small_rev = factor(ifelse(gross_output < median(gross_output, na.rm = TRUE), "Small GO", "Large GO"), levels = c("Large GO", "Small GO")),
    jo = factor(JO_class, levels = c("Corporation", "Ltd. Co.", "Proprietorship", "Partnership", "Other")),
    exporter = factor(ifelse( exports > 0, "Exporter", "Non-Exporter"), levels = c("Exporter", "Non-Exporter")),
    importer = factor(ifelse( imported_inputs > 0, "Importer", "Non-Importer"), levels = c("Importer", "Non-Importer")),
    sic_3 = as.factor(sic_3),
    year = as.factor(year),
    plant = as.factor(plant)
  ) %>%
  ungroup()


reg_313 <- feols(
    log_mats_share ~ sw(corp*small_l, corp*exporter, corp*importer)| sic_3,
    data = rdf,
    cluster = ~plant
  ) |> etable( dict = dict, keep = c("%^corpOther$","%corpOther:sic_3313","%corpOther:small_lSmallL", "%corpOther:exporterNon-Exporter", "%corpOther:importerNon-Importer"))

reg_313_2 <- feols(
    log_mats_share ~ sw( corp*sic_3*small_l, corp*sic_3*exporter, corp*sic_3*importer)| sic_3+as.factor(year),
    data = rdf,
    cluster = ~sic_3+as.factor(year)
) |> etable( dict = dict, keep = c("%^corpOther$","%corpOther:sic_3313","%corpOther:small_lSmallL", "%corpOther:exporterNon-Exporter", "%corpOther:importerNon-Importer"))

reg1 <-   feols(
    log_mats_share ~ sw(corp*sic_3*small_l, corp*sic_3*exporter, corp*sic_3*importer)| sic_3,
    data = rdf,
    cluster = ~sic_3
  )  |> etable( dict = dict, drop = "%sic")

reg2 <-   feols(
    log_mats_share ~ sw(corp*sic_3*small_l, corp*sic_3*exporter, corp*sic_3*importer)| sic_3+as.factor(year),
    data = rdf,
    cluster = ~sic_3+as.factor(year)
  )  |> etable( dict = dict, drop = "%sic")

reg3 <- feols(
    log_mats_share ~ sw( jo*sic_3*small_l, jo*sic_3*exporter, jo*sic_3*importer)| sic_3,
    data = rdf,
    cluster = ~sic_3
  ) |> etable( dict = dict, drop = "%sic")

reg4 <- feols(
    log_mats_share ~ sw( jo*sic_3*small_l, jo*sic_3*exporter, jo*sic_3*importer)| sic_3 + as.factor(year),
    data = rdf,
    cluster = ~sic_3 + as.factor(year)
  ) |> etable( dict = dict, drop = "%sic")

## %% Reg Tables ---------------------

dict <- c(
  "corpCorp" = "Corp",
  "corpOther" = "Non Corp",
  "joCorporation" = "Corporation",
  "joLtd.Co." = "LLC",
  "joProprietorship" = "Proprietorship",
  "joPartnership" = "Partnership",
  "joOther" = "Other",
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
  "plant" = "Plant"
)

# etable(reg1, drop = "%sic", dict = dict)
# etable(reg2, drop = "%sic", dict = dict)

## %% Save results ---------------------

# save(
#   reg1, reg2, reg3, reg4, dict, reg_313, rdf,
#   file = "Code/Products/910-reg-results.RData"
# )

## Exploratory analysis --------------------------

sic_sales_by_client <- read.csv("Paper/tbls/sic_distribution.csv") 
names(sic_sales_by_client) <- c("sic_3","Sample Size", "Retailers","Public", "Retailers and Public","Government", "Wholesalers")


## %% Regressions by top 20 industries ---------------------

reg5 <- rdf %>%
  filter(
    sic_3 %in% top_20_inds$sic_3
  ) %>%
  feols(
    log_mats_share ~ corp*small_l+corp*small_k+corp*exporter+corp*importer| plant + year,
    cluster = ~ plant + year,
    data = .
  )

reg_l <- lapply(
  c("corp*small_l", "corp*small_k", "corp*exporter", "corp*importer"),
  function(interaction_term) {
    fml <- as.formula(
      paste0("log_mats_share ~ ", interaction_term, " | plant + year")
    )
    feols(
      fml,
      cluster = ~ plant + year,
      data = rdf
    )
  }
)
  
etable(reg_l, reg5, dict = dict , keep = "%:")

## %% Save results 2 ---------------------

# save(
#   reg_l, reg5, dict,
#   file = "Code/Products/910-reg-results.RData"
# )


## %% By Juridical Organization ---------------------

reg6 <- rdf %>%
  filter(
    sic_3 %in% top_20_inds$sic_3
  ) %>%
  feols(
    log_mats_share ~ jo*small_l+jo*small_k+jo*exporter+jo*importer| plant + year,
    cluster = ~ plant + year,
    data = .
  ) #|> etable( dict = dict , keep = "%:")

reg_jo_l <- lapply(
  c("jo*small_l", "jo*small_k", "jo*exporter", "jo*importer"),
  function(interaction_term) {
    fml <- as.formula(
      paste0("log_mats_share ~ ", interaction_term, " | plant + year")
    )
    feols(
      fml,
      cluster = ~ plant + year,
      data = rdf
    )
  }
)

etable(reg_jo_l, reg6, dict = dict , keep = "%:")

## %% Save results 2 ---------------------

# save(
#   reg_l, reg5, dict, reg_jo_l, reg6,
#   file = "Code/Products/910-reg-results.RData"
# )




etable(
    reg_jo_l, reg6, 
    dict = dict , 
    keep = "%:", 
    # page.width = "15cm, 1cm",
    # adjustbox = "0.9 th",
    tex = TRUE
    # file = "Paper/tbls/910-who-evades-jo.tex"
)


## %% Tax Evasion Heterogeneity ---------------------

# - Small firms might evade more. Why? Informality can be viewed as the extreme
# form of tax evasion. The literature has documented that smaller firms are
# more likely to be informal. Therefore, it is likely that smaller firms also
# evade more. 
# In my base model with random auditing, it could be that smaller firms 
# face lower marginal cost of evasion (lower lambda_0). 
# In the model with strategic auditing, smaller firms could face lower
# probability of being audited (lower p_t).
# - Exporters might evade less. Why? Exporters do no pay sales taxes on their exports.
# Therefore, they have less incentives to overreport their inputs, as their sales tax 
# liabity is lower.
# In either model, the effective sales tax rate \tau is lower for exporters
# - Proprietorships may evade more. Proprietorships's owners are subject to double taxation
# on the firm's profits (corporate income tax and personal income tax on dividends).
# Therefore, proprietorships may have higher incentives to evade taxes. 
# In either model, proprietorships may face higher effective sales tax rate \tau

wip_df <- colombia_data_frame %>%
  filter(
    is.finite(y),
    is.finite(k),
    # is.finite(l),
    is.finite(m),
    # log_mats_share < log(0.75),
    log_mats_share > log(threshold_cut),
    # sic_3 %in% top_20_inds
  ) %>%
  group_by(sic_3) %>%
  mutate(
    corp = factor(ifelse(juridical_organization == 3, "Corp", "Other"), levels = c("Corp", "Other")),
    small_k = factor(ifelse(k < median(k, na.rm = TRUE), "Small K", "Large K"), levels = c("Large K","Small K")),
    small_l = factor(ifelse(l < median(l, na.rm = TRUE), "Small L", "Large L"), levels = c("Large L","Small L")),
    small_rev = factor(ifelse(gross_output < median(gross_output, na.rm = TRUE), "Small GO", "Large GO"), levels = c("Large GO", "Small GO")),
    jo = factor(JO_class, levels = c("Corporation", "Ltd. Co.", "Proprietorship", "Partnership", "Other")),
    exporter = factor(ifelse( share_exports > 0.1, "Exporter", "Non-Exporter"), levels = c("Exporter", "Non-Exporter")),
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
    size_capital = case_when(
      k < quantile(k, 0.25, na.rm = TRUE) ~ "Smallest 25%",
      k >= quantile(k, 0.25, na.rm = TRUE) & k < quantile(k, 0.5, na.rm = TRUE) ~ "25-50%",
      k >= quantile(k, 0.5, na.rm = TRUE) & k < quantile(k, 0.75, na.rm = TRUE) ~ "50-75%",
      k >= quantile(k, 0.75, na.rm = TRUE) ~ "Largest 25%"
    ),
    size_capital = factor(size_capital, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%")),
    # non_corp_small = interaction(corp,small_l, sep = ":"),
    non_corp_small = paste0(corp,small_l, sep=":"),
    non_corp_small = replace(non_corp_small, corp == "Corp", "Corp"),
    # non_corp_non_exporter = interaction(corp,exporter, sep = ":"),
    non_corp_non_exporter = paste0(corp,exporter, sep=":"),
    non_corp_non_exporter = replace(non_corp_non_exporter, corp == "Corp", "Corp"),
    # jo_small = interaction(jo,small_l, sep = ":"),
    jo_small = paste0(jo,small_l, sep=":"),
    jo_small = replace(jo_small, jo == "Corporation", "Corporation"),
    # jo_non_exporter = interaction(jo,exporter, sep = ":"),
    jo_non_exporter = paste0(jo,exporter, sep=":"),
    jo_non_exporter = replace(jo_non_exporter, jo == "Corporation", "Corporation")
  ) %>%
  ungroup() %>%
  mutate(
        size_exports = case_when(
      share_exports < 0.25 ~ "Smallest 25%",
      share_exports >= 0.25 & exports < 0.5 ~ "25-50%",
      share_exports >= 0.5 & exports < 0.75 ~ "50-75%",
      share_exports >= 0.75 ~ "Largest 25%"
    ),
    size_exports = factor(size_exports, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%"))
  )

feols(
    log_mats_share ~ sw(corp*small_l,corp*exporter, jo*small_l, jo*exporter )+log(sales_tax_sales) | plant+year,
    cluster = ~ plant + year,
    data = wip_df
  ) |> etable( keep = "%:")

reg_ctrl <- feols(
    log_mats_share ~ sw(corp*small_l,corp*exporter,corp*exporter+corp*small_l, jo*small_l,  jo*exporter, jo*exporter+jo*small_l)+ poly(l,k,m, degree = 2, raw = TRUE)| plant+year,
    cluster = ~ plant + year,
    data = wip_df
  ) 
  
reg_ctrl[1:3] |> etable( keep = "%:", dict = dict)

feols(
    log_mats_share ~ sw(corp*small_l,corp*exporter,corp*exporter+corp*small_l, jo*small_l,  jo*exporter, jo*exporter+jo*small_l)+ poly(l,k, degree = 2, raw = TRUE)| plant+year,
    cluster = ~ plant + year,
    data = wip_df
  ) |> etable( keep = "%:")

## Save results 3 ---------------------

# load("Code/Products/910-reg-results.RData")

# save(
#   reg_l, reg5, dict, reg_jo_l, reg6, reg_ctrl,# wip_df,
#   file = "Code/Products/910-reg-results.RData"
# )


## %% Revision 3 --------------------------
wip_df <- colombia_data_frame %>%
  filter(
    is.finite(y),
    is.finite(k),
    # is.finite(l),
    is.finite(m),
    # log_mats_share < log(0.75),
    log_mats_share > log(threshold_cut),
    # sic_3 %in% top_20_inds
  ) %>%
  group_by(sic_3) %>%
  mutate(
    corp = factor(ifelse(juridical_organization == 3, "Corp", "Other"), levels = c("Corp", "Other")),
    small_k = factor(ifelse(k < median(k, na.rm = TRUE), "Small K", "Large K"), levels = c("Large K","Small K")),
    small_l = factor(ifelse(l < median(l, na.rm = TRUE), "Small L", "Large L"), levels = c("Large L","Small L")),
    small_rev = factor(ifelse(gross_output < median(gross_output, na.rm = TRUE), "Small GO", "Large GO"), levels = c("Large GO", "Small GO")),
    jo = factor(JO_class, levels = c("Corporation", "Ltd. Co.", "Proprietorship", "Partnership", "Other")),
    exporter = factor(ifelse( share_exports > 0.1, "Exporter", "Non-Exporter"), levels = c("Exporter", "Non-Exporter")),
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
    size_capital = case_when(
      k < quantile(k, 0.25, na.rm = TRUE) ~ "Smallest 25%",
      k >= quantile(k, 0.25, na.rm = TRUE) & k < quantile(k, 0.5, na.rm = TRUE) ~ "25-50%",
      k >= quantile(k, 0.5, na.rm = TRUE) & k < quantile(k, 0.75, na.rm = TRUE) ~ "50-75%",
      k >= quantile(k, 0.75, na.rm = TRUE) ~ "Largest 25%"
    ),
    size_capital = factor(size_capital, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%")),

  ) %>%
  ungroup() %>%
  mutate(
        size_exports = case_when(
      share_exports < 0.25 ~ "Smallest 25%",
      share_exports >= 0.25 & exports < 0.5 ~ "25-50%",
      share_exports >= 0.5 & exports < 0.75 ~ "50-75%",
      share_exports >= 0.75 ~ "Largest 25%"
    ),
    size_exports = factor(size_exports, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%"))
  )

# Any change to the base specification represents a change to the underlying model. 
# s_ijt = beta_NC D_NC,ijt + beta_NC_Small D_NC,ijt * D_Small,ijt + beta_NC,t D_NC,ijt * gamma_t + beta_NC_Small,t D_NC,ijt * D_Small,ijt * gamma_t +gamma_j+ epsilon_ijt
# According to the model, there is one production function and corporations report truthfully.
# Non-corporations evade taxes, and therefore misreport their input usage.
# Smaller non-corporations evade more, so there is an interaction between small and non-corporations.
# There are time varying shocks that affect all non-corporations in an industry,
# so there is an interaction between time and non-corporations.
# There are time varying shocks that affect small non-corporations in an industry,
# so there is an interaction between time and small non-corporations.
# There are industry fixed effects because industries have different production functions. There is no intercept because it is absorbed by the industry fixed effects.
# There are no plant fixed effects because plants within an industry share the same production function.



# Small (Labor)
feols(
    log_mats_share ~ csw(corp, i(corp_small,"Large NonCorp"), i(corp_small, year, ref="Large NonCorp", ref2="81")+i(corp,year,ref="Corp",ref2="81"))|sic_3,
    cluster = ~ plant+ year,
    data = wip_df %>% 
      mutate(
        corp_small = ifelse(
          corp == "Other" & small_l == "Small L",
          "Small NonCorp",
          "Large NonCorp"
        ),
        corp_exporter = ifelse(
          corp == "Other" & exporter == "Non-Exporter",
          "NonExporter NonCorp",
          "Exporter NonCorp"
        )
      )
) |> etable()

# Non-Exporter
feols(
    log_mats_share ~ csw(corp, corp_exporter, i(corp_exporter, year, ref="Exporter NonCorp", ref2="81")+i(corp,year,ref="Corp",ref2="81"))|sic_3,
    cluster = ~ plant+ year,
    data = wip_df %>% 
      mutate(
        corp_small = ifelse(
          corp == "Other" & small_l == "Small L",
          "Small NonCorp",
          "Large NonCorp"
        ),
        corp_exporter = ifelse(
          corp == "Other" & exporter == "Non-Exporter",
          "NonExporter NonCorp",
          "Exporter NonCorp"
        )
      )
) |> etable()

# Proprietorships (JO)

feols(
    log_mats_share ~ csw(jo, i(jo, exporter, ref="Corporation", ref2="Exporter"), i(jo, year, ref="Corporation", ref2="81"))|sic_3,
    cluster = ~ plant+ year,
    data = wip_df %>% 
      mutate(
        corp_small = ifelse(
          corp == "Other" & small_l == "Small L",
          "Small NonCorp",
          "Large NonCorp"
        ),
        corp_exporter = ifelse(
          corp == "Other" & exporter == "Non-Exporter",
          "NonExporter NonCorp",
          "Exporter NonCorp"
        ),
        jo_small = case_when(
          jo == "Proprietorship" & small_l == "Small L" ~ "Small Prop.",
          jo == "Ltd. Co." & small_l == "Small L" ~ "Small LLC",
          jo == "Partnership" & small_l == "Small L" ~ "Small Part.",
          jo == "Other" & small_l == "Small L" ~ "Small Other",
          TRUE ~ NA_character_
        )
      )
) |> etable()

## %% Nov 6, 2025 Revision --------------------------------------

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
  group_by(sic_3) %>%
  mutate(
    corp = factor(ifelse(juridical_organization == 3, "Corp", "Other"), levels = c("Corp", "Other")),
    small_k = factor(ifelse(k < median(k, na.rm = TRUE), "Small K", "Large K"), levels = c("Large K","Small K")),
    small_l = factor(ifelse(l < median(l, na.rm = TRUE), "Small L", "Large L"), levels = c("Large L","Small L")),
    small_rev = factor(ifelse(gross_output < median(gross_output, na.rm = TRUE), "Small GO", "Large GO"), levels = c("Large GO", "Small GO")),
    jo = factor(JO_class, levels = c("Corporation", "Ltd. Co.", "Proprietorship", "Partnership", "Other")),
    jo = forcats::fct_collapse(
      jo,
      "Other" = c("Partnership", "Other")),
    exporter = factor(ifelse( share_exports > 0.1, "Exporter", "Non-Exporter"), levels = c("Exporter", "Non-Exporter")),
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
    size_capital = case_when(
      k < quantile(k, 0.25, na.rm = TRUE) ~ "Smallest 25%",
      k >= quantile(k, 0.25, na.rm = TRUE) & k < quantile(k, 0.5, na.rm = TRUE) ~ "25-50%",
      k >= quantile(k, 0.5, na.rm = TRUE) & k < quantile(k, 0.75, na.rm = TRUE) ~ "50-75%",
      k >= quantile(k, 0.75, na.rm = TRUE) ~ "Largest 25%"
    ),
    size_capital = factor(size_capital, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%")),

  ) %>%
  ungroup() %>%
  mutate(
    size_exports = case_when(
      share_exports < 0.25 ~ "Smallest 25%",
      share_exports >= 0.25 & exports < 0.5 ~ "25-50%",
      share_exports >= 0.5 & exports < 0.75 ~ "50-75%",
      share_exports >= 0.75 ~ "Largest 25%"
    ),
    size_exports = factor(size_exports, levels = c("Largest 25%", "50-75%", "25-50%", "Smallest 25%")),
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

reg_small <- feols(
  log_mats_share ~ sw(corp,corp+i(non_corp_small, "Base"),corp+i(non_corp_non_exporter, "Base"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)

reg_small |> etable()

reg_jo <- feols(
  log_mats_share ~ sw(jo, jo+i(jo_small,"Base"), jo+i(jo_non_exporter,"Base"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) 

reg_jo |> etable()

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
  "Small L" = "Small",
  "jo_small::Ltd.Co." = "LLC",
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
  `i(factor_var=non_corp_non_exporter,var="Base",ref_special=TRUE)` = "Non-Corp x Non-Exporter"
)

reg_small |> etable(dict = dict)

reg_jo |> etable(dict = dict)

## %% Save results 3 ---------------------

save(
  reg_small, reg_jo, dict, wip_df,
  file = "Code/Products/910-reg-results.RData"
)

## %% Plotting ---------------------

# load("Code/Products/910-reg-results.RData")

# %% Non-Corps over time, Ref year 1983 ---------------------

coefplot(reg_small[4:6],
  keep = "%Other:year",
  dict = dict,
  group = list(`Non-Corp` = "^^corp::Other:year::"),
  ref = list("corp::Other:year::83" = 6)
)

coefplot(reg_small[5:6],
  keep = "non_corp_small",
  dict = dict,
  group = list(`Non-Corp x Small x Year` = "^^non_corp_small::Other:Small L:year::"),
  ref = list("non_corp_small::Other:Small L:year::83" = 16)
)

coefplot(reg_small[6],
  keep = "Non-Exporter:",
  dict = dict,
  group = list(`Non-Corp x Non-Exporter x Year` = "^^non_corp_non_exporter::Other:Non-Exporter:year::"),
  ref = list("non_corp_non_exporter::Other:Non-Exporter:year::83" = 26)
)

# %% JO Plots ---------------------

## %% Non-Corps over time, Ref year 1983 ---------------------


load("Code/Products/910-reg-results.RData")


regs_non_corp_time <- feols(
  log_mats_share ~ sw(corp + i(corp,year,ref="Corp", ref2="83"), corp + i(non_corp_small, "Base") + i(corp,year,ref="Corp", ref2="83") + i(non_corp_small,year,ref="Base", ref2="83"), corp + i(non_corp_non_exporter, ref="Base") + i(corp,year,ref="Corp", ref2="83") + i(non_corp_non_exporter,year,ref="Base", ref2="83"))| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)

regs_non_corp_time |> etable(dict = dict)

coefplot(regs_non_corp_time[1:3],
  keep = "%Other:year",
  dict = dict,
  group = list(`Non-Corp` = "^^corp::Other:year::"),
  ref = list("corp::Other:year::83" = 4)
)

legend(
  "topleft",
  col=1:3,
  lwd=2,
  legend = c("Non-Corp", "Large Non-Corp", "Exporter Non-Corp")
)

coefplot(regs_non_corp_time[2],
  keep = "non_corp_small",
  dict = dict,
  group = list(`Non-Corp x Small x Year` = "^^non_corp_small::Other:Small L:year::"),
  ref = list("non_corp_small::Other:Small L:year::83" = 15)
)

coefplot(regs_non_corp_time[3],
  keep = "Non-Exporter:",
  dict = dict,
  group = list(`Non-Corp x Non-Exporter x Year` = "^^non_corp_non_exporter::Other:Non-Exporter:year::"),
  ref = list("non_corp_non_exporter::Other:Non-Exporter:year::83" = 15)
)

## %% Save results 3 ---------------------

save(
  reg_small, reg_jo, dict, wip_df, regs_non_corp_time,
  file = "Code/Products/910-reg-results.RData"
)

## %% Sanity Checks ---------------------

# Do the industry fixed effects match my first stage estimates?
load("Code/Products/910-reg-results.RData")
sic_fe_df <- lapply(
  seq_along(reg_small[1:3]), 
  \(x){
    colname<-paste0("(",x,")")
    tmp_df <-fixef(reg_small[[x]])$sic_3 |> exp() |> round(2) |> data.frame(col=_)
    names(tmp_df) <- colname
    tmp_df
  }
) |> do.call(cbind,args=_)

load("Code/Products/i_elas.RData")

sanity_tbl <-main_tbl %>%
  select(sic_3:mean_V_log_mats_share) %>%
  left_join(
    sic_fe_df %>%
      rownames_to_column("sic_3") %>%
      mutate(sic_3 = as.integer(sic_3)),
    by = "sic_3"
  )

## %% Save results 3 ---------------------

save(
  reg_small, reg_jo, dict, wip_df, regs_non_corp_time,
  sanity_tbl,
  file = "Code/Products/910-reg-results.RData"
)

## %% Leveraging Fiscal Policy Changes ---------------------

# 1983: Changes to Sales tax rate, some increased some decreased depending on the industry, 
# changes to CIT to individuals and LLCs; Some individuals got an increase, others a decrease. 
# Most individuals 8 percent increase, top was reduced from 56 to 49.
# LLCs received a decrease from 20 to 18%
# 1 Diff: over time of Non-Corps
# s_{ijt}^k = \beta_0 + e^{ST}_{ijt} + e{CIT}_{ijt}+epsilon_{ijt} firm i , industry j, year t, JO k
# This assumes no time trends, static PF, one PF across JOs
# E[s_{ijt'}^k] - E[s_{ijt}^k] = \Delta e_{ST} + \Delta e_{CIT} t' before policy change, t after policy change
# We expect \Delta e_{ST} > 0 for industries with an increase in sales tax rate, and < 0 for industries with a decrease in sales tax rate.
# \Delta e_{CIT} > 0 for 
# For exempt industries, \Delta e_{ST} = 0 : E[s_{ij't'}^k] - E[s_{ij't}^k] = \Delta e_{CIT} j' in Exempt
# 2 Diff: with respect to exempt industries
# E[s_{ijt'}^k] - E[s_{ijt}^k] - (E[s_{ij't'}^k] - E[s_{ij't}^k]) = \Delta e_{ST} j in Non-Exempt, j' in Exempt
# I assume \Delta e_{CIT} is the same across industries, so it cancels out.
# Corporations report truthfully, so they \Delta e_{ST}^Corp = \Delta e_{CIT}^Corp = 0
# In regression form: 
# s_{ijt}^k = \beta_0 + \beta_1 JO_i + \beta_2 NonExempt_j + \beta_3 JO_i * NonExempt_j + \beta_4 JO_i * NonExempt_j * Year_t +\epsilon_{ijt}^k

load("Code/Products/910-reg-results.RData")

did_reg <- fixest::feols(
  log_mats_share ~ i(jo,"Corporation")+i(sic_3,"311")+i(jo, sic_3, ref="Corporation", ref2="311")+i(jo_sic_year, "Base"),
  cluster = ~ plant + year,
  data = wip_df %>%
    mutate(
      jo_sic_year = paste(jo, sic_3, year, sep = ":"),
      jo_sic_year = replace(jo_sic_year, jo == "Corporation", "Base"),
      jo_sic_year = replace(jo_sic_year, sic_3 == "311", "Base"),
      jo_sic_year = replace(jo_sic_year, year == "83", "Base")
    )
  ) 

# Use keep to plot LLCs Proprietorships over time by industry
did_reg |> fixest::etable(keep = )
coef(did_reg) |> names()
## %% Save New results  ---------------------

save(
  reg_small, reg_jo, dict, wip_df, regs_non_corp_time,
  sanity_tbl, did_reg,
  file = "Code/Products/910-reg-results.RData"
)

coefplot(
  did_reg,
  keep = "%Ltd. Co.:312",
  dict = dict,
  group = list(`312 LLC`="^^jo_sic_year::Ltd. Co.:312:"),
  ref = list("jo_sic_year::Ltd. Co.:312:83"= 131),
  col = "blue", alpha = 0.5
)


tbl_pos <- did_reg |> coef() |> names() |> grep("jo_sic_year::Proprietorship:312:82",x=_, value=FALSE)
ref_vec <- tbl_pos+1
names(ref_vec) <- paste0("jo_sic_year::Proprietorship:",312,":83")
coefplot(
  did_reg,
  keep = "%Proprietorship:312",
  dict = dict,
  group = list(`312 Proprietorships`="^^jo_sic_year::Proprietorship:312:"),
  ref = ref_vec,
  add = TRUE, col = "red", alpha = 0.5,
  x.shift = 0.2
)

coefplot(
  did_reg,
  keep = "%Proprietorship:312",
  dict = dict,
  group = list(`312 Proprietorships`="^^jo_sic_year::Proprietorship:312:"),
  ref = list("83"= tbl_pos+1),
  add = TRUE, col = "red", alpha = 0.5,
  x.shift = 0.2
)

# coefplot(
#   did_reg,
#   keep = c("%Ltd. Co.:312","%Proprietorship:312"),
#   dict = dict,
#   group = list(`312 LLC`="^^jo_sic_year::Ltd. Co.:312:", `312 Proprietorships`="^^jo_sic_year::Proprietorship:312:"),
#   ref = list("jo_sic_year::Ltd. Co.:312:83"= 131, "jo_sic_year::Proprietorship:312:83"= tbl_pos+1)#,
#   # col = "blue", alpha = 0.5
# )

jos<-c("Ltd. Co.", "Proprietorship")
top_20_inds$sic_3

lapply(
  top_20_inds$sic_3[-1],
  \(x){
    prop_pattrn <- paste0("jo_sic_year::Proprietorship:",x,":82")
    llc_pattrn <- paste0("jo_sic_year::Ltd. Co.:",x,":82")
    tbl_pos_prop <- did_reg |> coef() |> names() |> grep(prop_pattrn,x=_, value=FALSE)
    tbl_pos_llcs <- did_reg |> coef() |> names() |> grep(llc_pattrn,x=_, value=FALSE)
    ref_vec_prop <- tbl_pos_prop+1
    names(ref_vec_prop) <- paste0("jo_sic_year::Proprietorship:",x,":83")
    ref_vec_llcs <- tbl_pos_llcs+1
    names(ref_vec_llcs) <- paste0("jo_sic_year::Ltd. Co.:",x,":83")
    
    coefplot(
      did_reg,
      keep = paste0("%Ltd. Co.:",x), #"%Ltd. Co.:312",
      dict = dict,
      group = list(`JO x SIC x Year`=paste0("^^jo_sic_year::Ltd. Co.:",x,":")),# "^^jo_sic_year::Ltd. Co.:312:"),
      ref = ref_vec_llcs,
      col = "blue", alpha = 0.5,
      main = paste0("Industry ",x)
    )
    coefplot(
      did_reg,
      keep = paste0("%Proprietorship:",x),#"%Proprietorship:312",
      dict = dict,
      group = list(`JO x SIC x Year`=paste0("^^jo_sic_year::Proprietorship:",x,":")),#"^^jo_sic_year::Proprietorship:312:"),
      ref = ref_vec_prop,
      add = TRUE, col = "red", alpha = 0.5,
      x.shift = 0.2
    ) #351e1e#312d2d
    legend(
      "topleft",
      col=c("blue","red"),
      lwd=2,
      legend = c("LLCs", "Proprietorships")
    )
  }
) |> invisible()
