## %% Setup ---------------------
library(tidyverse)
library(fixest)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/910-reg-results.RData") # wip_df
load("Code/Products/i_elas.RData")

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
  did_reg,
  file = "Code/Products/920-DD.RData"
)

## %% Plot Results ---------------------

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
  top_20_inds$sic_3[-1] |> sort(),
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

## %% Does the diff in diff make sense? ---------------------

tax_rates_yr_tbl <- colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        log_mats_share > log(threshold_cut),
        sic_3 %in% top_20_inds$sic_3
    ) %>%
    mutate(
        fiscal_law = case_when(
            year < 84 ~ "Pre-1983",
            year >= 84 & year < 87 ~ "1983-1986",
            year >= 87 ~ "Post-1986"
        ),
        fiscal_law = factor(fiscal_law, levels = c("Pre-1983", "1983-1986", "Post-1986"))
    ) %>%
    group_by(sic_3, fiscal_law) %>%
    summarise(
        tau_0 = mean(sales_tax_rate_sales, na.rm = TRUE),
        tau_1 = mean(sales_tax_rate_purchases, na.rm = TRUE),
        tau_1_log_share = mean(sales_tax_pur_share_sales, na.rm = TRUE),
        tau_reported = mean(effective_sales_tax_rate, na.rm = TRUE)
    ) %>%
    left_join(
        i_elas_tbl %>%
            filter(
                type == "m"
            ) %>%
            mutate(
                across(
                    !type,
                    ~ as.numeric(.x)
                )
            ),
            by = "sic_3"
    ) %>%
    mutate(
        tau_1_beta = tau_1 * corps,
        tau_true = tau_0-tau_1_beta,
        across(
            where(is.numeric),
            ~ round(.x*100, 2)
        ),
        sic_3 = factor(sic_3, levels = top_20_inds$sic_3)
    ) %>%
    pivot_longer(
        cols = c(
            tau_0,
            tau_1,
            tau_1_log_share,
            tau_1_beta,
            tau_reported,
            tau_true
        ),
        names_to = "tax_measure",
        values_to = "value"
    ) %>%
    pivot_wider(
        id_cols = c(sic_3, tax_measure),
        names_from = fiscal_law,
        values_from = value
    ) #%>%
    # arrange(sic_3) 
    
tax_rates_yr_tbl |> View()

## %% Save New results  ---------------------

save(
  did_reg, tax_rates_yr_tbl,
  file = "Code/Products/920-DD.RData"
)


## Revision DID ---------------------

reg_did_sic_fe <- feols(
  log_mats_share ~ i(jo , "Corporation") + i(jo, sic_3, ref="Corporation", ref2="311")+i(jo,year,ref="Corporation",ref2="83")+i(jo_sic_year, "Base")| sic_3,
  cluster = ~ plant + year,
  data = wip_df %>%
    mutate(
      jo_sic_year = paste(jo, sic_3, year, sep = ":"),
      jo_sic_year = replace(jo_sic_year, jo == "Corporation", "Base"),
      jo_sic_year = replace(jo_sic_year, sic_3 == "311", "Base"),
      jo_sic_year = replace(jo_sic_year, year == "83", "Base")
    )
  )

etable(
  reg_did_sic_fe,
  dict = dict)

coefplot(
  reg_did_sic_fe,
  keep = "%Ltd. Co.:312",
  dict = dict,
  group = list(`312 LLC`="^^jo_sic_year::Ltd. Co.:312:"),
  ref = list("jo_sic_year::Ltd. Co.:312:83"= 132),
  col = "blue", alpha = 0.5
)

## %% Save New results  ---------------------

save(
  did_reg, tax_rates_yr_tbl, reg_did_sic_fe,
  file = "Code/Products/920-DD.RData"
)