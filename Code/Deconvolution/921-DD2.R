## %% Setup ---------------------
library(tidyverse)
library(fixest)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/910-reg-results.RData") # wip_df
load("Code/Products/i_elas.RData")

## %% Sales tax rate changes statistics ---------------------

sales_tax_rates_stats <-colombia_data_frame %>%
  ungroup() %>%
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
  group_by(plant, fiscal_law) %>%
  summarise(
      tau_0 = mean(sales_tax_rate_sales, na.rm = TRUE),
      tau_1 = mean(sales_tax_rate_purchases, na.rm = TRUE),
      tau_1_log_share = mean(sales_tax_pur_share_sales, na.rm = TRUE),
      tau_reported = mean(effective_sales_tax_rate, na.rm = TRUE),
      sic_3 = first(sic_3)
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
      sic_3 = factor(as.character(sic_3), levels = top_20_inds$sic_3),
      tau_1_beta = tau_1 * corps,
      tau = tau_0 - tau_1_beta,
      across(
          where(is.numeric),
          ~ round(.x*100, 2)
      )
  ) %>%
  pivot_longer(
      cols = c(
          tau_0,
          tau_1,
          tau_1_log_share,
          tau_1_beta,
          tau_reported,
          tau
      ),
      names_to = "tax_measure",
      values_to = "value"
  ) %>%
  pivot_wider(
      id_cols = c(sic_3, plant, tax_measure),
      names_from = fiscal_law,
      values_from = value
  ) %>%
  mutate(
    delta_83_86 = `1983-1986` - `Pre-1983`,
    delta_83_post_86 = `Post-1986` - `Pre-1983`,
    .before = `Pre-1983`
  ) %>%
  group_by(sic_3,tax_measure) %>%
  summarise(
    across(
      delta_83_86:`Post-1986`,
      list(
        mean = ~ mean(.x, na.rm=TRUE),
        # sd = ~ sd(.x, na.rm=TRUE),
        n = ~ n(),
        se = ~ sd(.x, na.rm=TRUE)/sqrt(n())
      )
    )
  ) %>%
  pivot_longer(
    cols = delta_83_86_mean:`Post-1986_se`,
    names_to = c("period","stat"),
    values_to = "value",
    names_pattern = "(.*_*.*_*.*)_(mean|n|se)"
  ) %>%
  pivot_wider(
    id_cols = c(sic_3, tax_measure, period),
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    lower_ci = mean - 1.96*se,
    upper_ci = mean + 1.96*se,
    pval = 2*pnorm(-abs(mean/se)),
    stars = case_when(
      pval < 0.01 ~ "***",
      pval < 0.05 ~ "**",
      pval < 0.1 ~ "*",
      TRUE ~ ""
    )
  )

sales_tax_rates_stats %>%
  filter(
    period %in% c("delta_83_86", "delta_83_post_86")
  ) |> View()

## %% Sales tax rate changes by industry ---------------------

load("Code/Products/921-DD2.RData")

sales_tax_group <- sales_tax_rates_stats %>%
  filter(
    period %in% c("delta_83_86", "delta_83_post_86"),
    tax_measure %in% c("tau_0","tau_1", "tau_1_beta","tau")
  ) %>%
  mutate(
    # delta_stx = case_when(
    #   mean > 1 & stars != "" ~ "Increase",
    #   mean < -1 & stars != "" ~ "Decrease",
    #   mean > 0 & mean <= 1 & stars != "" ~ "Slight Increase",
    #   mean < 0 & mean >= -1 & stars != "" ~ "Slight Decrease",
    #   TRUE ~ "No Change"
    # ),
    delta_stx = case_when(
      mean > 0 & stars != "" ~ "Increase",
      mean < 0 & stars != "" ~ "Decrease",
      # mean > 0 & mean <= 1 & stars != "" ~ "Slight Increase",
      # mean < 0 & mean >= -1 & stars != "" ~ "Slight Decrease",
      TRUE ~ "No Change"
    ),
    delta_stx_tax = paste0(
      "$\\",tax_measure,"$ :: ",delta_stx
    ),
    # delta_stx_tax = replace(
    #   delta_stx_tax,
    #   delta_stx == "No Change",
    #   "No Change"),
    .after = period
  )

# sales_tax_group <- sales_tax_group %>% 
#   mutate(
#     delta_83_86 = replace(
#       delta_83_86,
#       sic_3 %in% c("311","312"),
#       "Exempt"
#     ),
#     delta_83_post_86 = replace(
#       delta_83_post_86,
#       sic_3 %in% c("311","312"),
#       "Exempt"
#   )
#   )

# sales_tax_group  %>%
#   filter(
#     tax_measure %in% c("tau_1","tau_0")
#   ) %>%
#   pivot_wider(
#     id_cols = sic_3,
#     names_from = period,
#     values_from = delta_stx_tax,
#     values_fn = \(x) paste(unique(x), collapse = "; ")
#   )

sales_tax_83_group <- sales_tax_group  %>%
  filter(
    tax_measure %in% c("tau_1","tau_0","tau"),
    period == "delta_83_86"
  ) %>%
  pivot_wider(
    id_cols = c(sic_3, period),
    names_from = tax_measure,
    values_from = delta_stx_tax,
    # values_fn = \(x) paste(unique(x), collapse = "; ")
  ) %>%
  select(!period) %>%
  mutate(
    delta_tx_83 = paste(
      tau, tau_0, tau_1, 
      sep = "; "#,
      # collapse = TRUE
    )
  )
sales_tax_83_group %>% View()

# sales_tax_group  %>%
#   filter(
#     tax_measure %in% c("tau_1","tau_0","tau"),
#     period == "delta_83_86"
#   ) %>%
#   pivot_wider(
#     id_cols = c(sic_3, period),
#     names_from = tax_measure,
#     values_from = delta_stx,
#     # values_fn = \(x) paste(unique(x), collapse = "; ")
#   ) %>%
#   select(!period) %>%
#   mutate(
#     delta_tx_83 = paste(
#       tau_0, tau_1, tau,
#       sep = ", "#,
#       # collapse = TRUE
#     )
#   )

# Sales
# sales_tbl <- sales_tax_group  %>%
#   filter(
#     tax_measure == "tau_0"
#   ) %>%
#   pivot_wider(
#     id_cols = sic_3,
#     names_from = period,
#     values_from = delta_stx_tax,
#     values_fn = \(x) paste(unique(x), collapse = "; ")
#   ) %>%
#   group_by(delta_83_86) %>%
#   summarise(
#     industries = paste(sic_3, collapse = ", "),
#     # industries = factor(industries, levels = c(""))
#   )

# Purchases
# purchas_tbl <- sales_tax_group  %>%
#   filter(
#     tax_measure == "tau_1"
#   ) %>%
#   pivot_wider(
#     id_cols = sic_3,
#     names_from = period,
#     values_from = delta_stx_tax,
#     values_fn = \(x) paste(unique(x), collapse = "; ")
#   ) %>%
#   group_by(delta_83_86) %>%
#   summarise(
#     industries = paste(sic_3, collapse = ", "),
#     # industries = factor(industries, levels = c(""))
#   )

# Sales & Purchases
both_tbl <- sales_tax_group  %>%
  filter(
    tax_measure %in% c("tau_1","tau_0", "tau"),
    period == "delta_83_86"
  ) %>%
  # pivot_wider(
  #   id_cols = sic_3,
  #   names_from = period,
  #   values_from = delta_stx_tax,
  #   values_fn = \(x) paste(unique(x), collapse = "; ")
  # ) %>%
  group_by(sic_3) %>%
  summarise(
    delta_stx_tax = paste(delta_stx_tax, collapse = "; "),
    n = first(n)
  ) %>%
  mutate(
    delta_stx_tax = replace(
      delta_stx_tax,
      sic_3 %in% c("311","312"),
      paste("Exempt -", delta_stx_tax[sic_3 == "311"])
    )
  ) %>%
  group_by(delta_stx_tax) %>%
  summarise(
    industries = paste(sic_3, collapse = ", "),
    n_inds = n(),
    n = sum(n)
    # industries = factor(industries, levels = c(""))
  )

both_tbl %>% View()

# Effective Sales Tax Rate (TRUE)
eff_tbl <- sales_tax_group  %>%
  filter(
    tax_measure %in% c("tau"),
    period == "delta_83_86"
  ) %>%
  group_by(sic_3) %>%
  summarise(
    delta_stx_tax = paste(delta_stx_tax, collapse = " : "),
    n = first(n)
  ) %>%
  mutate(
    delta_stx_tax = replace(
      delta_stx_tax,
      sic_3 %in% c("311","312"),
      paste("Exempt -", delta_stx_tax[sic_3 == "311"])
    )
  ) %>%
  group_by(delta_stx_tax) %>%
  summarise(
    industries = paste(sic_3, collapse = ", "),
    n_inds = n(),
    n = sum(n)
    # industries = factor(industries, levels = c(""))
  )
  
eff_tbl |> View()


## %% Save New results  ---------------------

save(
    # sales_tbl, purchas_tbl, 
    both_tbl, sales_tax_83_group,
    eff_tbl,
    file = "Code/Products/921-DD2.RData"
)


## %% Sales tax rate changes table ---------------------

sales_tax_rate_tbl <- sales_tax_rates_stats %>%
  mutate(
    mean_str = paste0(
      round(mean,2),
      stars
    ),
    se_str = paste0(
      " (",
      round(se,2),
      ")"
    ),
    ci_str = paste0(
      "[",
      round(lower_ci,2),", ",
      round(upper_ci,2),"]"
    )
  ) %>%
  select(
    sic_3, tax_measure, period,
    mean_str, se_str, ci_str
  ) %>%
  filter(
    period %in% c("delta_83_86", "delta_83_post_86")
  ) %>%
  pivot_wider(
    id_cols = c(sic_3, tax_measure),
    names_from = period,
    values_from = c(mean_str, se_str, ci_str)
  ) %>% 
  pivot_longer(
    cols = !c(sic_3,tax_measure),
    names_to = c("stat","period"),
    values_to = "value",
    names_pattern = "(mean|se|ci)_str_(delta_83_86|delta_83_post_86)"
  ) %>%
  pivot_wider(
    id_cols = c(sic_3, tax_measure, stat),
    names_from = period,
    values_from = value
  )# |> View()

sales_tax_rate_tbl %>%
  filter(
    tax_measure %in% c("tau_0","tau_1","tau"),
    stat %in% c("mean","se")
  ) %>%
  left_join(
    sales_tax_83_group,
    by = "sic_3"
  ) %>%
  select(
    !c(tau_0, tau_1, tau)
  ) %>%
  arrange(
    delta_tx_83, sic_3, tax_measure, stat
  ) %>% View()

## %% Save New results  ---------------------

save(
    # sales_tbl, purchas_tbl, 
    both_tbl, sales_tax_83_group,
    sales_tax_rate_tbl, eff_tbl,
    file = "Code/Products/921-DD2.RData"
)

## %% Notes


## Least likely industries to react to policy changes:
# - 312 : Exempt during the whole period
# - Industries that got a reduction in their sales tax rates (?)

# Candidates from looking at the coefplots:
# - 313: Before 1983, negative, after 1983, not different from zero
# - 351: not strong evidence, mostly not different from zero, small upward
# - 369: not significant from zero before and after 1983
# - 390: not significant from zero before 1986, upward between 1989-1991
#   increase after 1986 but not significant

# Selected:
# - 312: Exempt during the whole period. In data, industry 312 is the only one,
# along with 311, to have negative effective sales tax rate (reported and corrected).
# - 313: Even though true tau increases from 4 to 6% and then to 9, tau_beta and tau_1_log_share
# do not increase drastically, tau_beta goes from 1.3 to 1.9%. It is interesting to note that,
# the diff-in-diff approach shows that after 1983, LLCs do not show evidence of increasing 

# ## Most likely industries to react to policy changes:
# - 382 and 384: Industries that lost exemption status in 1984

# Candidates from looking at the coefplots:

# 32
# - 321: downward trend before 1983, jump after 1983
# - 322: downward trend up to 1985, then jump after 1985
# - 323: downward trend before 1983 and then an steady increment after 1983,
# - 324: downward trend before 1983, jump after 1983


# - 331: downward trend before 1983 and then an steady increment after 1983,
# - 332: not significant before 1983, jump after 1983

# 34 
# - 341: flat tendency, before and after 1983
# - 342: not significant before 1983, jump after 1983
# - 352: downward trend before 1983, jump after 1983
# - 356: not significant before 1983, jump after 1983
# - 381: not significant before 1983, jump after 1983
# - 382: downward trend before 1983, jump after 1983 and second jump after 1986
# - 383: downward trend before 1983, jump after 1983
# - 384: downward trend before 1983, jump after 1983 and second jump after 1986

# Candidates by looking at tax rates:

# 0) No significant change > |0.5%|:
# - 311
# - 312
# - 369. OK


# - 331. Small increase in sales tax rate of sales and true tau.
#   Ex-ante, not likely to expect a significant response to policy.
# - 352. Small increase in reported purcase sales taxes share of sales, but not
# significant increase in true tau or tau_1*beta.


# 1) Increase in sales tax rate of sales:
# - 313: Increase in sales tax rate of sales, but not significant increase in
# sales tax rate of purchases* beta.

# 2) Increase in sales tax rate of purchases:
# - 323

# 3) Increase in both:
# - 321. Tau beta goes from 3 to 4. tau 0 from 6 o 9%, tau 1 from 7 to 10%
# - 322. 
# - 324
# - 341. Check
# - 381 small increase in both
# - 382 big increase in both
# - 383 big increase in both
# - 384 big increase in both

# 4) Decrease in sales tax rate of sales, increase in sales tax rate of purchases:
# - 332. Tau_1*beta remained almost the same
# - 342
# - 390. Tau_1*beta remained almost the same

# 5) Decrease in sales tax rate of purchases:
# - 356. Just one percent decrease. Not much change in true tau  or tau_1*beta

# 6) Decrease in both:
# - 351




## %% Revision II ---------------------
# load("Code/Products/910-reg-results.RData") # wip_df
# load("Code/Products/921-DD2.RData")

wip_df <- wip_df %>%
  left_join(
    sales_tax_83_group,
    by = "sic_3"
  ) %>%
  mutate(
    jo_sic_year = paste(jo, sic_3, year, sep = ":"),
    jo_sic_year = replace(jo_sic_year, jo == "Corporation", "Base"),
    jo_sic_year = replace(jo_sic_year, sic_3 %in% c("311","312"), "Base"),
    jo_sic_year = replace(jo_sic_year, year == "83", "Base"),
    exempt_ind = ifelse(sic_3 %in% c("311","312"), 1, 0),
    exempt_ind = factor(exempt_ind, levels = c(1,0), labels = c("Exempt","Taxed")),
    tau_0 = replace(
      tau_0,
      sic_3 %in% c("311","312"),
      "Exempt"
    ),
    tau_1 = replace(
      tau_1,
      sic_3 %in% c("311","312"),
      "Exempt"
    ),
    tau = replace(
      tau,
      sic_3 %in% c("311","312"),
      "Exempt"
    ),
    delta_tx_83 = replace(
      delta_tx_83,
      sic_3 %in% c("311","312"),
      "Exempt"
    ),
    # delta_tx_83 = replace(
    #   delta_tx_83,
    #   sic_3 %in% c("369","331"),
    #   "No Change"
    # ),
    corp_tau_0_year = factor(
      ifelse(
        corp == "Corp" | tau_0 == "Exempt" | year == "83",
        "Base",
        paste(corp, tau_0, year, sep = ":")
        )),
    corp_tau_1_year = factor(
      ifelse(
        corp == "Corp" | tau_1 == "Exempt" | year == "83",
        "Base",
        paste(corp, tau_1, year, sep = ":")
        )),
    corp_tau_year = factor(
      ifelse(
        corp == "Corp" | tau == "Exempt" | year == "83",
        "Base",
        paste(corp, tau, year, sep = ":")
        )
    ),
    corp_delta_tau_year = factor(
      ifelse(
        corp == "Corp" | delta_tx_83 == "Exempt" | year == "83",
        "Base",
        paste(corp, delta_tx_83, year, sep = ":")
        )
    ),
    jo_delta_tau_year = factor(
      ifelse(
        jo == "Corporation" | delta_tx_83 == "Exempt" | year == "83",
        "Base",
        paste(jo, delta_tx_83, year, sep = ":")
        )
    ),
    jo_delta_tau_year_ncb = factor(
      ifelse(
        jo == "Corporation" | delta_tx_83 == "No Change; No Change; No Change" | year == "83",
        "Base",
        paste(jo, delta_tx_83, year, sep = ":")
        )
    ),
    jo_tau_year = factor(
      ifelse(
        jo == "Corporation" | tau == "Exempt" | year == "83",
        "Base",
        paste(jo, tau, year, sep = ":")
        )
    ),
    corp_exempt_year = factor(
      ifelse(
        corp == "Corp" | exempt_ind == "Exempt" | year == "83",
        "Base",
        paste(corp, exempt_ind, year, sep = ":")
        )
    ),
    corp_exempt_year_het = factor(
      ifelse(
        corp == "Corp" | year == "83",
        "Base",
        paste(corp, exempt_ind, year, sep = ":")
        )
    ),
    jo_exempt_year = factor(
      ifelse(
        jo == "Corporation" | exempt_ind == "Exempt" | year == "83",
        "Base",
        paste(jo, exempt_ind, year, sep = ":")
        )
    ),
    jo_exempt_year_het = factor(
      ifelse(
        jo == "Corporation" | year == "83",
        "Base",
        paste(jo, exempt_ind, year, sep = ":")
        )
    )
  )

## %% Regression DID ---------------------


feols(
  log_sales~ i(year, ref = "83") | sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> coefplot()


# %% Changes in sales tax rate of sales

# wip_df %>%
#   left_join(
#     sales_tax_83_group,
#     by = "sic_3"
#   ) %>%
#   mutate(
#     jo_sic_year = paste(jo, sic_3, year, sep = ":"),
#     jo_sic_year = replace(jo_sic_year, jo == "Corporation", "Base"),
#     jo_sic_year = replace(jo_sic_year, sic_3 == "311", "Base"),
#     jo_sic_year = replace(jo_sic_year, year == "83", "Base"),
#     corp_tau_0_year = factor(
#       ifelse(
#         corp == "Corp" | tau_0 == "No Change" | year == "83",
#         "Base",
#         paste(corp, tau_0, year, sep = ":")
#         )
#     )
#   ) %>%

feols(
    log_mats_share ~ corp + i(corp, year, ref = "Corp", ref2 = "83") + i(corp, exempt_ind, ref = "Corp", ref2= "Exempt") + i(corp_tau_0_year, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  ) |> etable(
    dict = dict
  )


# Grouping industries by the change in sales tax rate of sales and juridical 
# organizations into all non-corporations firms,

# Non-Corps in exempt industries overreport little with respect to Corporations (Good!)
# Non-Corps in idustries that got a sales tax reduction increased their overreporting starting in
# 1984 (data, 1983 fiscal year). No pre-trend before 1983.
# Non-Corps in industries that got a sales tax increase did not increased their overrerporting
# significantly until after 1985. However, there is a pre-trend going downward before 1983.

# Slight decrease: some increase starting in 1984, then zero after 1987
# slight increase: consistent increase after 1983

## %% Exempt vs Taxed industries ---------------------

reg_grp_ex_ncrp <- feols(
    log_mats_share ~ corp + i(corp, year, ref = "Corp", ref2 = "83") + i(corp, exempt_ind, ref = "Corp", ref2= "Exempt") + i(corp_exempt_year, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  ) 

reg_grp_ex_ncrp |> etable(
    dict = dict
  )

## %% WINNER: CORP ---------------------
# Want to sum Gamma_2 and Gamma_3: I want the heterogeneous effect not the diff-in-diff
reg_nc_exe_het <- feols(
    log_mats_share ~ corp  + i(corp, exempt_ind, ref = "Corp")+i(corp_exempt_year_het, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  )

reg_nc_exe_het |> etable(
    dict = dict
  )


reg_grp_ex_jo <- feols(
    log_mats_share ~ jo + i(jo, year, ref = "Corporation", ref2 = "83") + i(jo, exempt_ind, ref = "Corporation", ref2= "Exempt") + i(jo_exempt_year, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  ) 

reg_grp_ex_jo |> etable(
    dict = dict
  )
## %% WINNER: JO ---------------------
reg_jo_exe_het <- feols(
    log_mats_share ~ jo + i(jo, exempt_ind, ref = "Corporation") + i(jo_exempt_year_het, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  )
reg_jo_exe_het |> etable(
    dict = dict
  )
## %% Changes in sales tax rate of purchases

# wip_df %>%
#   left_join(
#     sales_tax_83_group,
#     by = "sic_3"
#   ) %>%
#   mutate(
#     jo_sic_year = paste(jo, sic_3, year, sep = ":"),
#     jo_sic_year = replace(jo_sic_year, jo == "Corporation", "Base"),
#     jo_sic_year = replace(jo_sic_year, sic_3 == "311", "Base"),
#     jo_sic_year = replace(jo_sic_year, year == "83", "Base"),
#     corp_tau_1_year = factor(
#       ifelse(
#         corp == "Corp" | tau_1 == "No Change" | year == "83",
#         "Base",
#         paste(corp, tau_1, year, sep = ":")
#         )
#     )
#   ) %>%
feols(
    log_mats_share ~ corp + i(corp, year, ref = "Corp", ref2 = "83") + i(corp, tau_1, ref = "Corp", ref2= "Exempt") + i(corp_tau_1_year, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  ) |> etable(
    dict = dict
  )


## %% Changes in true effective sales tax rate ---------------- 

# wip_df %>%
#   left_join(
#     sales_tax_83_group,
#     by = "sic_3"
#   ) %>%
#   mutate(
#     jo_sic_year = paste(jo, sic_3, year, sep = ":"),
#     jo_sic_year = replace(jo_sic_year, jo == "Corporation", "Base"),
#     jo_sic_year = replace(jo_sic_year, sic_3 == "311", "Base"),
#     jo_sic_year = replace(jo_sic_year, year == "83", "Base"),
#     corp_tau_year = factor(
#       ifelse(
#         corp == "Corp" | tau == "No Change" | year == "83",
#         "Base",
#         paste(corp, tau, year, sep = ":")
#         )
#     )
#   ) %>%
feols(
    log_mats_share ~ corp + i(corp, year, ref = "Corp", ref2 = "83") + i(corp, tau, ref = "Corp", ref2= "Exempt") + i(corp_tau_year, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  ) |> etable(
    dict = dict
  )

## %% Changes in all sales tax rates ----------------


# wip_df %>%
#   left_join(
#     sales_tax_83_group,
#     by = "sic_3"
#   ) %>%
#   mutate(
#     jo_sic_year = paste(jo, sic_3, year, sep = ":"),
#     jo_sic_year = replace(jo_sic_year, jo == "Corporation", "Base"),
#     jo_sic_year = replace(jo_sic_year, sic_3 == "311", "Base"),
#     jo_sic_year = replace(jo_sic_year, year == "83", "Base"),
#     delta_tx_83 = ifelse(sic_3 %in% c("311","312"), "Exempt" , delta_tx_83),
#     corp_delta_tau_year = factor(
#       ifelse(
#         corp == "Corp" | delta_tx_83 == "Exempt" | year == "83",
#         "Base",
#         paste(corp, delta_tx_83, year, sep = ":")
#         )
#     )
#   ) %>%
feols(
    log_mats_share ~ corp + i(corp, year, ref = "Corp", ref2 = "83") + i(corp, delta_tx_83, ref = "Corp", ref2= "Exempt") + i(corp_delta_tau_year, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  ) |> etable(
    dict = dict
  )



# What makes sense:

# - tau_0 decrease, tau_1 decrease, tau decrease: No significant increase,
# even a decrease in overreporting during 1985 and 1986
# - tau_1 increase, including slight increases are associated with significant increases
# in overreporting, whether tau_0 increases or decreases.
# - no change in either tau_0 or tau_1 is associated with no change in overreporting.
# - no change in tau_1, slight decrease in tau_0 and tau some increases between 1984-1987,
# then no change.

# What does not make sense:
# - tau_0 decrease, tau_1 decrease, tau no change: significant increase in overreporting
# starting in 1984, no pre-trend. Might be explained by changes in CIT. Effect of CIT increases
# on propriertorships dominate over LLCs.



## %% Changes in all sales tax rates by juridical organization ---------------------

did_grp_reg <- feols(
    log_mats_share ~ jo + i(jo, year, ref = "Corporation", ref2 = "83") + i(jo, delta_tx_83, ref = "Corporation", ref2= "Exempt") + i(jo_delta_tau_year, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  )

did_grp_reg |> etable(
    dict = dict
  )



# What makes sense:

# - LLCs in exempt industries, no difference with Corporations, even though they got
# a decrease in their CIT rates.
# - Proprietorships in exempt industries increase overreporting starting in 1984. This
# coincides with an increase in their overall CIT rates.
# - For LLCs, increases in tau_1 , including slight increases, are associated with increases
# in overreporting, whether tau_0 increases or decreases. This does not hold for Other JOs,
# or Proprietorships.
# - For proprietorships, changes in CIT seem to dominate the effect of changes in sales tax rates.
# I do not observe a clear pattern for different changes in sales tax rates.
# - For others JOs, in industries where true tau decreases slightly due to decreases in tau_0, but 
# no changes in tau_1, overreporting decreases but starting in 1986
# - For other JOs, in industries where tau_1 increased, buth tau_0 remained without significant changes,
# overreporting increases starting in 1984, no pre-trend.



## %% Base is Firms in Industries with No Change in any tax rate ---------------------

# did_grp_reg_ncb <-feols(
#     log_mats_share ~ jo + i(jo, year, ref = "Corporation", ref2 = "83") + i(jo, delta_tx_83, ref = "Corporation", ref2= "No Change") + i(jo_delta_tau_year_ncb, "Base")| sic_3,
#     cluster = ~ plant + year,
#     data = wip_df
#   )
  
# did_grp_reg_ncb |> etable(
#     dict = dict
#   )

## %% Changes in true effective sales tax rate by juridical organization ---------------------

did_grp_reg_tet <- feols(
    log_mats_share ~ jo + i(jo, year, ref = "Corporation", ref2 = "83") + i(jo, tau, ref = "Corporation", ref2= "Exempt") + i(jo_tau_year, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  )

did_grp_reg_tet|> etable(
    dict = dict
  )

## %% Save New results  ---------------------

save(
    # sales_tbl, purchas_tbl,
    wip_df,
    both_tbl, sales_tax_83_group,
    sales_tax_rate_tbl,
    did_grp_reg, did_grp_reg_tet, eff_tbl,
    reg_grp_ex_ncrp, reg_grp_ex_jo,
    reg_nc_exe_het, reg_jo_exe_het,
    dict,
    file = "Code/Products/921-DD2.RData"
)
