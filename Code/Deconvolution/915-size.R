## %% load packages and data ---------------
library(tidyverse)
library(fixest)

load("Code/Products/test_data.RData")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/911-all_inds-2.RData")
tol_cb_palette <- c(
  "#332288", "#76b1cf", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)
wong_cb_palette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536",
  "#0072B2", "#D55E00", "#CC79A7"
)
palette(wong_cb_palette)

## %% Firm size and tax evasion ---------------

# Initially, my prior was that size was inversely related to
# tax evasion. The intuition was that the larger the firms, the more likely
# to be caught evading taxes. Higher probability of detection, lower incentives to
# evade taxes. 

# This intuition assumes that the probability of detection is increasing with size.
# However, it is standard in the literature that the probability of detection is independent of
# any firm characteristic. The probablity of detection is a simplification of two probabilities: 
# The probability of beign audited by the tax authority, and the probability of being detected cheating
# conditional on being audited. It is common in the literature to simplify these probabilities, in particular
# in empirical papers, by assuming that firms are detected everytime if they are audited, and that the 
# tax authority audits firms at random. Thus, the probability of detection is independent of firm characteristics,
# including size. The probability of detection depends only on the level of evasion —the more overreporting— and an exogenous
# component from the firm side, which is effort from the tax authority. The idea is that you can be audited at random. 
# If you are audited, you will be compared to similar firms. The more you overreport, the more likely you are to be detected.

# Another relevant component in the tax evasion decision is the cost of evasion. This cost includes accounting and managing costs
# associated with tax evasion, including the extra work of accounting employees to keep two books of records, one for the tax
# authority and one for internal use —the firm would need to know its true profits to make business decisions—, as well as economic
# costs, such as lost investment opportunities due to investors not liking low profits, or distrusting the firm because of suspected
# tax evasion. Although previous work has allowed for heterogeneity in the cost of evasion, an explicit relationship between size and
# evasion cost has not been explicitly modeled. Almunia et al suggest that costs of evasion might be heterogeneous among firms for several
# reasons, including the number of employees with which the employer needs to collude to evade taxes (this reason would suggest that larger firms
# might face higher costs of evasion, as more employees need to collude). Other reasons evasion costs might vary are manager's preferences 
# (e.g., risk aversion, ethical considerations), or the time and skills invested in the misreporting activity rather than allocating th
# to productive activities.


# Recently however, new evidence has shown that tax evasion by overreporting is increasing in revenue percentile, but it collapses
# for the largest firms, in particular, for very large corporations. The same paper finds that in Ecuador, corporations evaded 1.7% and
# sole proprietorships, 11.5%. The authors find that overreporting firms are larger than other firms, with higher revenues, costs, 
# and tax liabilities (makes sense, higher the tax liability, higher the incentives to evade taxes). The authors show that the probability
# of overreporting increases monotonically in firm revenue. Furthermore, the share of fake deductions out of total deductions due to overreporting
# increases throughout the revenue distribution, except at the very top. Authors suggest that the sharp drop at the top might be due to
# corporations having stronger incentives to avoid illegal behaviour, or that they can use more sophisticated and effective measures of 
# tax avoidance, that do not require misreporting expenses.

## %% Data wrangling ---------------

wip_df <- wip_df %>%
  ungroup() %>%
  group_by(plant) %>%
  mutate(
    lag_2_k = lag(k,2,order_by = year),
    lag_2_l = lag(l,2,order_by = year),
    lag_lexp = lag(log(exports),order_by = year),
    lag_2_lexp = lag(log(exports),2,order_by = year),
    lag_limp = lag(log(imported_inputs),order_by = year),
    lag_2_m = lag(m,2,order_by = year)
  ) %>%
  ungroup() %>%
  mutate(
    exports_pct = percent_rank(exports),
    imports_pct = percent_rank(imported_inputs),
    capital_ntile = ntile(k, 10),
    cap_lag_ntile = ntile(lag_k, 10),
    cap_lag2_ntile = ntile(lag_2_k, 10),
    cap_l_pct = percent_rank(lag_k),
    cap_l2_pct = percent_rank(lag_2_k),
    labour_ntile = ntile(l, 10),
    lab_lag_ntile = ntile(lag_l, 10),
    lab_lag2_ntile = ntile(lag_2_l, 10),
    lab_l_pct = percent_rank(lag_l),
    lab_l2_pct = percent_rank(lag_2_l),
    # reported mats is flexible: true mats is, 
    #     overreporting is as it depends only on 
    #     q,kappa, and tau
    mats_l_ntile = ntile(lag_m, 10), 
    mats_pct = percent_rank(lag_m),
    mats_l2_ntile = ntile(lag_2_m, 10),
    mats_l2_pct = percent_rank(lag_2_m),
    revenue_ntile = ntile(lag_log_sales, 10),
    revenue2_ntile = ntile(lag_2_log_sales, 10),
    exports_ntile = ntile(log(exports), 10),
    imports_ntile = ntile(log(imported_inputs), 10),
    exp_l2_ntile = ntile(lag_2_lexp, 10),
    exp_l_ntile = ntile(lag_lexp, 10),
    exp_l_pct = percent_rank(lag_lexp),
    imp_l_ntile = ntile(lag_limp, 10),
    imp_l_pct = percent_rank(lag_limp),
    # exports_perc = case_when(
    #   # exports_share <= 0 ~ "0",
    #   exports_share >= 0 & exports_share <= 0.1 ~ "10%",
    #   exports_share > 0.1 & exports_share <= 0.20 ~ "20%",
    #   exports_share > 0.2 & exports_share <= 0.30 ~ "30%",
    #   exports_share > 0.3 & exports_share <= 0.40 ~ "40%",
    #   exports_share > 0.4 & exports_share <= 0.50 ~ "50%",
    #   exports_share > 0.5 & exports_share <= 0.60 ~ "60%",
    #   exports_share > 0.6 & exports_share <= 0.70 ~ "70%",
    #   exports_share > 0.7 & exports_share <= 0.80 ~ "80%",
    #   exports_share > 0.8 & exports_share <= 0.90 ~ "90%",
    #   exports_share > 0.9 ~ "100%",
    #   .default = NA_character_
    # ),
    exports_perc = factor(round(share_exports*100,-1)),
    imports_perc = factor(round(share_imports*100,-1)),
    age_pct = percent_rank(age)
  )

## %% Add new dictionary entries
# Note: We need to check for duplicates ignoring spaces, since fixest's dict_apply
# ignores spaces when matching keys

new_dict_entries <- c(
  # i() syntax entries (no spaces to match existing format)
  "i(factor_var=corp,var=l_pct,ref=\"Corp\")" = "Non-Corp x Labour Perc.",
  "i(factor_var=corp,var=I(l_pct^2),ref=\"Corp\")" = "Non-Corp x Labour Perc. Sq.",
  "i(factor_var=corp,var=k_pct,ref=\"Corp\")" = "Non-Corp x Capital Perc.",
  "i(factor_var=corp,var=I(k_pct^2),ref=\"Corp\")" = "Non-Corp x Capital Perc. Sq.",
  "i(factor_var=corp,var=lag_rev_pct,ref=\"Corp\")" = "Non-Corp x Rev (t-1) Perc.",
  "i(factor_var=corp,var=I(lag_rev_pct^2),ref=\"Corp\")" = "Non-Corp x Rev (t-1) Perc. Sq.",
  "i(factor_var=corp,var=lag_2_rev_pct,ref=\"Corp\")" = "Non-Corp x Rev (t-2) Perc.",
  "i(factor_var=corp,var=I(lag_2_rev_pct^2),ref=\"Corp\")" = "Non-Corp x Rev (t-2) Perc. Sq.",
  "i(factor_var=corp,var=exports_pct,ref=\"Corp\")" = "Non-Corp x Exports Perc.",
  "i(factor_var=corp,var=I(exports_pct^2),ref=\"Corp\")" = "Non-Corp x Exports Perc. Sq.",
  "i(factor_var=corp,var=imports_pct,ref=\"Corp\")" = "Non-Corp x Imports Perc.",
  "i(factor_var=corp,var=I(imports_pct^2),ref=\"Corp\")" = "Non-Corp x Imports Perc. Sq.",
  "i(factor_var=corp,var=share_exports,ref=\"Corp\")" = "Non-Corp x Exports Share",
  "i(factor_var=corp,var=I(share_exports^2),ref=\"Corp\")" = "Non-Corp x Exports Share Sq.",
  "i(factor_var=corp,var=share_imports,ref=\"Corp\")" = "Non-Corp x Imports Share",
  "i(factor_var=corp,var=I(share_imports^2),ref=\"Corp\")" = "Non-Corp x Imports Share Sq.",
  "i(factor_var=corp,var=age_pct,ref=\"Corp\")" = "Non-Corp x Age Perc.",
  "i(factor_var=corp,var=I(age_pct^2),ref=\"Corp\")" = "Non-Corp x Age Perc. Sq.",
  "i(factor_var=corp,var=mats_pct,ref=\"Corp\")" = "Non-Corp x Mats Perc.",
  "i(factor_var=corp,var=I(mats_pct^2),ref=\"Corp\")" = "Non-Corp x Mats Perc. Sq.",
  # corp::Other syntax entries
  "corp::Other:l_pct" = "Non-Corp x Labour Perc.",
  "corp::Other:k_pct" = "Non-Corp x Capital Perc.",
  "corp::Other:lag_rev_pct" = "Non-Corp x Rev (t-1) Perc.",
  "corp::Other:lag_2_rev_pct" = "Non-Corp x Rev (t-2) Perc.",
  "corp::Other:exports_pct" = "Non-Corp x Exports Perc.",
  "corp::Other:imports_pct" = "Non-Corp x Imports Perc.",
  "corp::Other:I(l_pct^2)" = "Non-Corp x Labour Perc. Sq.",
  "corp::Other:I(k_pct^2)" = "Non-Corp x Capital Perc. Sq.",
  "corp::Other:I(lag_rev_pct^2)" = "Non-Corp x Rev (t-1) Perc. Sq.",
  "corp::Other:I(lag_2_rev_pct^2)" = "Non-Corp x Rev (t-2) Perc. Sq.",
  "corp::Other:I(exports_pct^2)" = "Non-Corp x Exports Perc. Sq.",
  "corp::Other:I(imports_pct^2)" = "Non-Corp x Imports Perc. Sq.",
  "corp::Other:mats_pct" = "Non-Corp x Mats Perc.",
  "corp::Other:I(mats_pct^2)" = "Non-Corp x Mats Perc. Sq.",
  "corp::Other:mats_l2_pct" = "Non-Corp x Mats (t-2) Perc.",
  "corp::Other:I(mats_l2_pct^2)" = "Non-Corp x Mats (t-2) Perc. Sq."
)


# Only add entries that don't exist (comparing without spaces to match fixest behavior)
dict_names_no_space <- gsub(" ", "", names(dict))
new_names_no_space <- gsub(" ", "", names(new_dict_entries))

# Find which new entries are truly new
# new_entries_mask <- !(new_names_no_space %in% dict_names_no_space)
# entries_to_add <- new_dict_entries[new_entries_mask]

# Find duplicated entries (if any) and replace them with the new ones
duplicated_mask <- new_names_no_space %in% dict_names_no_space
if (any(duplicated_mask)) {
  duplicated_names <- new_names_no_space[duplicated_mask]
  existing_dup_mask <- dict_names_no_space %in% duplicated_names
  dict <- dict[!existing_dup_mask]
}

# Add only the new entries
# if (length(entries_to_add) > 0) {
#   dict <- c(dict, entries_to_add)
# }
dict <- c(dict, new_dict_entries)
## %% Exploring size and tax evasion ---------------

reg_s <- feols(
  log_mats_share ~ sw(
    corp,
    corp+i(corp,l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp"),
    corp+i(corp,k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp"),
    corp+i(corp, lag_rev_pct, ref="Corp")+i(corp, lag_rev_pct^2, ref="Corp"),
    corp+i(corp, lag_2_rev_pct, ref="Corp")+i(corp, lag_2_rev_pct^2, ref="Corp"),
    corp+i(corp, exports_pct, ref="Corp")+i(corp, exports_pct^2, ref="Corp"),
    corp+i(corp,imports_pct,ref="Corp")+i(corp,imports_pct^2,ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)

reg_s |> etable()
reg_s_coefs <- reg_s |> coef()
reg_s_coefs

reg_s_ntile <- feols(
  log_mats_share ~ sw(
    corp,
    i(corp,i.labour_ntile, ref="Corp"),
    i(corp,i.capital_ntile, ref="Corp"),
    i(corp,i.revenue_ntile, ref="Corp"),
    i(corp,i.revenue2_ntile, ref="Corp"),
    i(corp,i.exports_ntile, ref="Corp"),
    i(corp,i.imports_ntile, ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)

reg_s_ntile |> etable()

reg_s_cum <- feols(
  log_mats_share ~ csw(
    corp,
    # i(corp, year, ref="Corp", ref2="83"),
    i(corp, l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp"),
    i(corp, k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp"),
    i(corp, lag_rev_pct, ref="Corp")+i(corp, lag_rev_pct^2, ref="Corp"),
    # i(corp, lag_2_rev_pct, ref="Corp")+i(corp, lag_2_rev_pct^2, ref="Corp"),
    i(corp, exports_pct, ref="Corp")+i(corp, exports_pct^2, ref="Corp"),
    i(corp, imports_pct, ref="Corp")+i(corp, imports_pct^2, ref="Corp")#,
    # i(corp, age_pct, ref="Corp") + i(corp, I(age_pct^2), ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)

reg_s_cum |> etable(dict = dict)
reg_s_cum_coefs <- reg_s_cum |> coef()

# Exports/Imports 

feols(
  log_mats_share ~ sw(
    i(corp, i.exports_ntile, ref="Corp"),
    i(corp, i.imports_ntile, ref="Corp"),
    i(corp, exports_perc, ref="Corp"),
    i(corp, imports_perc, ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)



## %% Save results ---------------

save(
  reg_s,
  reg_s_ntile,
  reg_s_cum,
  wip_df,
  dict,
  # reg_s_coefs,
  # reg_s_cum_coefs,
  file = "Code/Products/915-size.RData"
)

## %% Plot size effects ---------------

png(
  file = "Code/Products/915-size-fun.png",
  width = 620, height = 480
)

curve(
    reg_s_coefs[2,3] +
    reg_s_coefs[2,4]*x +
    reg_s_coefs[2,5]*x^2,
  0,1,
  ylab = "",
  xlab = "Percentile of size measure",
  col = 1,
  lwd = 2,
  ylim = c(-0.2, 0.3)
)

curve(
    reg_s_coefs[3,3] +
    reg_s_coefs[3,6]*x +
    reg_s_coefs[3,7]*x^2,
  0,1,
  ylab = "",
  xlab = "Percentile of size measure",
  col = 2,
  lwd = 2,
  add = TRUE
)

curve(
    reg_s_coefs[4,3] +
    reg_s_coefs[4,8]*x +
    reg_s_coefs[4,9]*x^2,
  0,1,
  ylab = "",
  xlab = "Percentile of size measure",
  col = 3,
  lwd = 2,
  add = TRUE
)

curve(
    reg_s_coefs[5,3] +
    reg_s_coefs[5,10]*x +
    reg_s_coefs[5,11]*x^2,
  0,1,
  ylab = "",
  xlab = "Percentile of size measure",
  col = 4,
  lwd = 2,
  add = TRUE
)

curve(
    reg_s_coefs[6,3] +
    reg_s_coefs[6,12]*x +
    reg_s_coefs[6,13]*x^2,
  0,1,
  ylab = "",
  xlab = "Percentile of size measure",
  col = 5,
  lwd = 2,
  add = TRUE
)

curve(
    reg_s_coefs[7,3] +
    reg_s_coefs[7,14]*x +
    reg_s_coefs[7,15]*x^2,
  0,1,
  ylab = "",
  xlab = "Percentile of size measure",
  col = 6,
  lwd = 2,
  add = TRUE
)

legend(
  "bottomright",
  legend = c("Labour","Capital","Revenue (t-1)","Revenue (t-2)","Exports","Imports"),
  col = 1:6,
  lty = 1,
  lwd = 2
)

title("Effect of firm size on tax evasion")

dev.off()

## %% Coefplot for size ntile regression ---------------



coefplot(
  reg_s_ntile,
  col = tol_cb_palette,
  main = "Firm size and tax evasion",
  drop = "NaN",
  xlab = "Size ntile",
  group = list(
    "Labour" = "^^corp::Other:labour_ntile::",
    "Capital" = "^^corp::Other:capital_ntile::",
    "Revenue" = "^^corp::Other:revenue_ntile::",
    "Exports" = "^^corp::Other:exports_ntile::",
    "Imports" = "^^corp::Other:imports_ntile::"
  ),
  dict = dict#,
  # horiz = TRUE
)


## %% all together --------------------

png(
  file = "Paper/images/915-size-ntiles.png",
  width = 620, height = 720
)

layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
par(mar = c(2.5, 2.5, 1.2, 0.8), mgp = c(1.5, 0.4, 0))


coefplot(
  reg_s_ntile[[2]],
  col = 1,
  keep = "labour",
  main = "Labour",
  # xlab = "Size ntile",
  group = list(
    "Decile" = "^^corp::Other:labour_ntile::"#,
    # "Capital" = "^^corp::Other:capital_ntile::",
    # "Revenue" = "^^corp::Other:revenue_ntile::",
    # "Exports" = "^^corp::Other:exports_ntile::",
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  dict = dict,
  # ylim = c(-0.25,0.3),
  grid = FALSE
  # horiz = TRUE
)

coefplot(
  reg_s_ntile[[3]],
  # col = tol_cb_palette,
  keep = "capital",
  main = "Capital",
  # xlab = "Size ntile",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    "Decile" = "^^corp::Other:capital_ntile::"#,
    # "Revenue" = "^^corp::Other:revenue_ntile::",
    # "Exports" = "^^corp::Other:exports_ntile::",
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  # add = TRUE,
  col = 2,
  # x.shift = 0.2,
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_s_ntile[[4]],
  # col = tol_cb_palette,
  main = "Revenue (t-1)",
  # xlab = "Size ntile",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    # " " = "^^corp::Other:capital_ntile::"#,
    "Decile" = "^^corp::Other:revenue_ntile::"#,
    # "Exports" = "^^corp::Other:exports_ntile::",
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  # add = TRUE,
  # x.shift = 0.4,
  col = 5,
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_s_ntile[[5]],
  # col = tol_cb_palette,
  main = "Revenue (t-2)",
  # drop = "NaN",
  # xlab = "Size ntile",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    # " " = "^^corp::Other:capital_ntile::"#,
    # ` ` = "^^corp::Other:revenue_ntile::"#,
    "Decile" = "^^corp::Other:revenue2_ntile::"#,
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  # add = TRUE,
  # x.shift = 0.4,
  col = 3,
  dict = dict,
  grid = FALSE
)

# coefplot(
#   reg_s_ntile[[6]],
#   # col = tol_cb_palette,
#   main = "Imports",
#   drop = "NaN",
#   # xlab = "Size ntile",
#   group = list(
#     # " " = "^^corp::Other:Labour_ntile::"#,
#     # " " = "^^corp::Other:capital_ntile::"#,
#     # ` ` = "^^corp::Other:revenue_ntile::"#,
#     # ` ` = "^^corp::Other:exports_ntile::"#,
#     "Imports" = "^^corp::Other:imports_perc::"
#   ),
#   # add = TRUE,
#   # x.shift = 0.4,
#   col = 4,
#   dict = dict,
#   grid = FALSE
# )

# title("Firm size and tax evasion", outer = TRUE)

dev.off()

## %% Conditional effects of size on tax evasion ---------------

png(
  file = "Paper/images/915-size-cond-effects.png",
  width = 620, height = 720
)

coefplot(
  reg_s_cum[[length(reg_s_cum)]],
  # pt.col = 1:9,
  as.multiple = TRUE,
  main = "Firm Size and Tax Evasion\nConditional Effects",
  # xlab = "Size ntile",
  group = list(
    "Labour" = "^^Non-Corp x Labo(u)*r ",
    "Capital" = "^^Non-Corp x Capital ",
    "Revenue (t-1)" = "^^Non-Corp x Rev \\(t-1\\)",
    "Exports" = "^^Non-Corp x Exports ",
    "Imports" = "^^Non-Corp x Imports "
  ),
  col = c(1, rep(2:6, each = 2)),
  dict = dict,
  pt.pch = 16,
  grid = FALSE,
  horiz = TRUE
)

dev.off()

## %% Plotting conditional curves ---------------

png(
  file = "Paper/images/915-size-cond-curves.png",
  width = 620, height = 720.
)

layout(matrix(c(1, 2, 3, 3, 4, 5), nrow = 3, byrow = TRUE))
par(mar = c(2.5, 2.5, 1.2, 0.8), mgp = c(1.5, 0.4, 0))

curve(
    reg_s_cum_coefs[6,3] +
    reg_s_cum_coefs[6,4]*x +
    reg_s_cum_coefs[6,5]*x^2+
    reg_s_cum_coefs[6,6]*0.5+
    reg_s_cum_coefs[6,7]*0.5^2+
    reg_s_cum_coefs[6,8]*0.5+
    reg_s_cum_coefs[6,9]*0.5^2+
    reg_s_cum_coefs[6,10]*0.5+
    reg_s_cum_coefs[6,11]*0.5^2+
    reg_s_cum_coefs[6,12]*0.5+
    reg_s_cum_coefs[6,13]*0.5^2,
  0,1,
  ylab = "",
  xlab = "",
  ylim = c(-0.45, 0.45),
  col = 1,
  lwd = 2,
  # ylim = c(-0.7, 0.35),
  main = "Labour"
)

curve(
    reg_s_cum_coefs[6,3] +
    reg_s_cum_coefs[6,4]*0.5 +
    reg_s_cum_coefs[6,5]*0.5^2+
    reg_s_cum_coefs[6,6]*x+
    reg_s_cum_coefs[6,7]*x^2+
    reg_s_cum_coefs[6,8]*0.5+
    reg_s_cum_coefs[6,9]*0.5^2+
    reg_s_cum_coefs[6,10]*0.5+
    reg_s_cum_coefs[6,11]*0.5^2+
    reg_s_cum_coefs[6,12]*0.5+
    reg_s_cum_coefs[6,13]*0.5^2,
  0,1,
  ylab = "",
  xlab = "",
  ylim = c(-0.45, 0.45),
  col = "gray",
  lwd = 2,
  main = "Capital"
  # add = TRUE
)

curve(
    reg_s_cum_coefs[6,3] +
    reg_s_cum_coefs[6,4]*0.5 +
    reg_s_cum_coefs[6,5]*0.5^2+
    reg_s_cum_coefs[6,6]*0.5+
    reg_s_cum_coefs[6,7]*0.5^2+
    reg_s_cum_coefs[6,8]*x+
    reg_s_cum_coefs[6,9]*x^2+
    reg_s_cum_coefs[6,10]*0.5+
    reg_s_cum_coefs[6,11]*0.5^2+
    reg_s_cum_coefs[6,12]*0.5+
    reg_s_cum_coefs[6,13]*0.5^2,
  0,1,
  ylab = "",
  xlab = "",
  ylim = c(-0.45, 0.45),
  main = "Revenue (t-1)",
  col = 3,
  lwd = 2,
  # add = TRUE
)

curve(
    reg_s_cum_coefs[6,3] +
    reg_s_cum_coefs[6,4]*0.5 +
    reg_s_cum_coefs[6,5]*0.5^2+
    reg_s_cum_coefs[6,6]*0.5+
    reg_s_cum_coefs[6,7]*0.5^2+
    reg_s_cum_coefs[6,8]*0.5+
    reg_s_cum_coefs[6,9]*0.5^2+
    reg_s_cum_coefs[6,10]*x+
    reg_s_cum_coefs[6,11]*x^2+
    reg_s_cum_coefs[6,12]*0.5+
    reg_s_cum_coefs[6,13]*0.5^2,
  0,1,
  ylab = "",
  xlab = "",
  ylim = c(-0.45, 0.45),
  main = "Exports",
  col = "gray",
  lwd = 2,
  # add = TRUE
)

curve(
    reg_s_cum_coefs[6,3] +
    reg_s_cum_coefs[6,4]*0.5 +
    reg_s_cum_coefs[6,5]*0.5^2+
    reg_s_cum_coefs[6,6]*0.5+
    reg_s_cum_coefs[6,7]*0.5^2+
    reg_s_cum_coefs[6,8]*0.5+
    reg_s_cum_coefs[6,9]*0.5^2+
    reg_s_cum_coefs[6,10]*0.5+
    reg_s_cum_coefs[6,11]*0.5^2+
    reg_s_cum_coefs[6,12]*x+
    reg_s_cum_coefs[6,13]*x^2,
  0,1,
  ylab = "",
  xlab = "",
  ylim = c(-0.45, 0.45),
  main = "Imports",
  col = 5,
  lwd = 2,
  # add = TRUE
)

# curve(
#     reg_s_cum_coefs[6,3] +
#     reg_s_cum_coefs[6,4]*0.5 +
#     reg_s_cum_coefs[6,5]*0.5^2+
#     reg_s_cum_coefs[6,6]*0.5+
#     reg_s_cum_coefs[6,7]*0.5^2+
#     reg_s_cum_coefs[6,8]*0.5+
#     reg_s_cum_coefs[6,9]*0.5^2+
#     reg_s_cum_coefs[6,10]*0.5+
#     reg_s_cum_coefs[6,11]*0.5^2+
#     reg_s_cum_coefs[6,12]*0.5+
#     reg_s_cum_coefs[6,13]*0.5^2+
#     reg_s_cum_coefs[6,12]*x+
#     reg_s_cum_coefs[6,13]*x^2,
#   0,1,
#   ylab = "",
#   xlab = "",
# ylim = c(-0.45, 0.45),
# main = ,
#   col = 6,
#   lwd = 2,
#   add = TRUE
# )

# legend(
#   "top",
#   legend = c("Labour","Capital","Revenue (t-2)","Exports","Imports"),
#   col = 1:6,
#   lty = 1,
#   lwd = 2
# )

# title("Effect of firm size on tax evasion\nContitional Effects", line = 0.5)

dev.off()

## %% Materials and Tax rates, do they affect overreporting measure?

feols(
  log_mats_share ~ sw(
    corp,
    corp+i(corp,l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp"),
    corp+i(corp,k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp"),
    corp+i(corp, mats_pct, ref="Corp")+i(corp,mats_pct^2, ref="Corp"),
    corp+i(corp, lag_rev_pct, ref="Corp")+i(corp, lag_rev_pct^2, ref="Corp"),
    corp+i(corp, lag_2_rev_pct, ref="Corp")+i(corp, lag_2_rev_pct^2, ref="Corp"),
    corp+i(corp, exports_pct, ref="Corp")+i(corp, exports_pct^2, ref="Corp"),
    corp+i(corp,imports_pct,ref="Corp")+i(corp,imports_pct^2,ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> etable(dict = dict)

# Similar to the other measures of size, when lag materials is used
# as proxy for size, I observe an increasing but marginally decreasing
# relationship between size and tax evasion for non-corporations.
