## %% load packages and data ---------------
library(tidyverse)
library(fixest)

load("Code/Products/test_data.RData")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/915-size.RData")
# load("Code/Products/911-all_inds-2.RData")
tol_cb_palette <- c(
  "#332288", "#76b1cf", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)
wong_cb_palette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536",
  "#0072B2", "#D55E00", "#CC79A7"
)
ibm_cb_palette <- c(
  "#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000"
)
kelly_max_contrast_palette <- c(
  "#FFB300", "#803E75", "#FF6800", "#A6BDD7", "#C10020",
  "#CEA262", "#817066", "#007D34", "#F6768E", "#00538A",
  "#FF7A5C", "#53377A", "#FF8E00", "#B32851", "#F4C800",
  "#7F180D", "#93AA00", "#593315", "#F13A13", "#232C16"
)
palette(wong_cb_palette)


## %% Second Review -----------------------------

# Feb 12- 2026: 
# I want to avoid using vars that could be mechanically
# correlated with the outcome variable, which is overreporting. For example, using lagged materials as a proxy for size could be problematic, since materials are part of the overreporting measure. If a firm overreports materials, then its lagged materials will be higher than its true materials, which could mechanically create a positive correlation between lagged materials and overreporting. To avoid this issue, I will focus on using lagged sales as a proxy for size, since sales are not directly part of the overreporting measure. I will also check if the results are robust to using other proxies for size that are not mechanically correlated with overreporting, such as number of employees or capital stock.
# Labour: Use t-2 Labour, Labour_t could be correlated with materials (misspecification, maybe it's not CD),
#        Labour_t-1 could be correlated with output_t through labour_t because Labour_t is predetermined or dynamic
# Capital: Use t-2 capital, same issue as Labour
# Reported Materials: Materials are flexible, overreporting is too it only depends on q, kapp, and tau,
#.         I can use lag reported materials
# Revenue: Use t-2 revenue, revenue_t is used in explained var, 
#          revenue_t-1 is correlated with revenue_t through productivity (AR(1))
# Exports/Imports: Exports are part of Revenue, Imports are part of materials,
#          I can use imports t-1, because of mats flex assumption
#          I can use exports t-2, because productivity (AR(1)) in revenues

reg_sz_dec <- feols(
  log_mats_share ~ sw(
    i(corp,i.lab_lag2_ntile, ref="Corp"),
    i(corp,i.cap_lag2_ntile, ref="Corp"),
    i(corp,i.mats_l_ntile, ref="Corp"),
    i(corp,i.revenue2_ntile, ref="Corp"),
    i(corp,i.exp_l_ntile, ref="Corp"),
    i(corp,i.imp_l_ntile, ref="Corp")
  )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
)

reg_sz_dec |> coefplot(
  # dict = dict,
  # main = "Size and tax evasion\nUsing more conservative size measures",
  group = list(
    "Labour (t-2)" = "^^corp::Other:lab_lag2_ntile::",
    "Capital (t-2)" = "^^corp::Other:cap_lag2_ntile::",
    "Materials (t-1)" = "^^corp::Other:mats_l_ntile::",
    "Revenue (t-2)" = "^^corp::Other:revenue2_ntile::",
    "Exports (t-1)" = "^^corp::Other:exp_l_ntile::",
    "Imports (t-1)" = "^^corp::Other:imp_l_ntile::"
  ),
  pt.pch = 16
)

# Still see the increasing then drop at the top pattern,
# more pronounced for capital and revenue, 
# less pronounced for labour and materials

## %% Marginal Effects --------------------------

reg_sz_mg <- feols(
  log_mats_share ~ 
    corp+
    i(corp, lab_l2_pct, ref="Corp")+i(corp,lab_l2_pct^2, ref="Corp")+
    i(corp, cap_l2_pct, ref="Corp")+i(corp,cap_l2_pct^2, ref="Corp")+
    sw0(
      i(corp, mats_pct, ref="Corp")+i(corp, mats_pct^2, ref="Corp"),
      i(corp, mats_l2_pct, ref="Corp")+i(corp, mats_l2_pct^2, ref="Corp")
    )+
    i(corp, lag_2_rev_pct, ref="Corp")+i(corp, lag_2_rev_pct^2, ref="Corp")+
    i(corp, exp_l_pct, ref="Corp")+i(corp, exp_l_pct^2, ref="Corp")+
    i(corp, imp_l_pct, ref="Corp")+i(corp, imp_l_pct^2, ref="Corp")#+
    # i(corp, age_pct, ref="Corp") + i(corp, I(age_pct^2), ref="Corp")+
    # i(corp, log(sales_tax_sales), ref="Corp")+i(corp, I(log(sales_tax_sales)^2), ref="Corp")+
    # i(corp, log(sales_tax_purchases), ref="Corp")+i(corp, I(log(sales_tax_purchases)^2), ref="Corp")
  | sic_3,
  cluster = ~ plant + year,
  data = wip_df
)
reg_sz_mg |> etable()
reg_sz_mg |> coef() |> names()

new_dict <- c(
    "i(factor_var = corp, var = lab_l2_pct, ref = \"Corp\")" = "Non-Corp x Labour (t-2) Perc.",                   
    "i(factor_var = corp, var = I(lab_l2_pct^2), ref = \"Corp\")" = "Non-Corp x Labour (t-2) Perc. Sq.",              
    "i(factor_var = corp, var = cap_l2_pct, ref = \"Corp\")" = "Non-Corp x Capital (t-2) Perc.",                   
    "i(factor_var = corp, var = I(cap_l2_pct^2), ref = \"Corp\")" = "Non-Corp x Capital (t-2) Perc. Sq.",
    # "i(factor_var = corp, var = mats_pct, ref = \"Corp\")" = "Non-Corp x Materials",                    
    # "i(factor_var = corp, var = I(mats_pct^2), ref = \"Corp\")" = "Non-Corp x Materials Sq.",              
    # "i(factor_var = corp, var = lag_2_rev_pct, ref = \"Corp\")" = "Revenue (t-2)",                
    # "i(factor_var = corp, var = I(lag_2_rev_pct^2), ref = \"Corp\")" = "Revenue (t-2) Sq.",           
    "i(factor_var = corp, var = exp_l_pct, ref = \"Corp\")" = "Non-Corp x Exports (t-1) Perc.",                    
    "i(factor_var = corp, var = I(exp_l_pct^2), ref = \"Corp\")" = "Non-Corp x Exports (t-1) Perc. Sq.",               
    "i(factor_var = corp, var = imp_l_pct, ref = \"Corp\")" = "Non-Corp x Imports (t-1) Perc.",                    
    "i(factor_var = corp, var = I(imp_l_pct^2), ref = \"Corp\")" = "Non-Corp x Imports (t-1) Perc. Sq.",               
    # "i(factor_var = corp, var = age_pct, ref = \"Corp\")" = "Age",                      
    # "i(factor_var = corp, var = I(age_pct^2), ref = \"Corp\")" = "Age Sq.",                 
    "i(factor_var = corp, var = log(sales_tax_sales), ref = \"Corp\")" = "Non-Corp x ST—Sales",         
    "i(factor_var = corp, var = I(log(sales_tax_sales)^2), ref = \"Corp\")" = "Non-Corp x ST—Sales Sq.",    
    "i(factor_var = corp, var = log(sales_tax_purchases), ref = \"Corp\")" = "Non-Corp x ST—Purchases",     
    "i(factor_var = corp, var = I(log(sales_tax_purchases)^2), ref = \"Corp\")" = "Non-Corp x ST—Purchases Sq.",
    "corp::Other:exp_l_pct" = "Non-Corp x Exports (t-1) Perc.",
    "corp::Other:I(exp_l_pct^2)" = "Non-Corp x Exports (t-1) Perc. Sq.",
    "corp::Other:imp_l_pct" = "Non-Corp x Imports (t-1) Perc.",
    "corp::Other:I(imp_l_pct^2)" = "Non-Corp x Imports (t-1) Perc. Sq.",
    "corp::Other:age_pct" = "Non-Corp x Age",
    "corp::Other:I(age_pct^2)" = "Non-Corp x Age Sq.",
    "corp::Other:log(sales_tax_sales)" = "Non-Corp x ST—Sales",
    "corp::Other:I(log(sales_tax_sales)^2)" = "Non-Corp x ST—Sales Sq.",
    "corp::Other:log(sales_tax_purchases)" = "Non-Corp x ST—Purchases",
    "corp::Other:I(log(sales_tax_purchases)^2)" = "Non-Corp x ST—Purchases Sq."
)
reg_sz_mg |> etable(dict = c(dict, new_dict))
reg_sz_mg |> coefplot(
  dict = c(dict, new_dict),
#   as.multiple = TRUE, 
#   col = c(1,rep(2:9, each = 2)),
  pt.pch = 16 ,
  grid = FALSE,
  group = list(
    # "Non-Corp x" = "^^Non-Corp x ",
    "Labour (t-2)" = "^^Non-Corp x Labour \\(t-2\\) ",
    "Capital (t-2)" = "^^Non-Corp x Capital \\(t-2\\) ",
    "Materials (t-1)" = "^^Non-Corp x Mats ",
    "Revenue (t-2)" = "^^Non-Corp x Rev \\(t-2\\) ",
    "Exports (t-1)" = "^^Non-Corp x Exports \\(t-1\\) ",
    "Imports (t-1)" = "^^Non-Corp x Imports \\(t-1\\) "#,
    # "Age" = "^^Non-Corp x Age",
    # "ST—Sales" = "^^Non-Corp x ST—Sales",
    # "ST—Purch" = "^^Non-Corp x ST—Purchases"
  ),
  horiz = TRUE
)

# Main vars remain the same: Labour, Revenue, Imports

## %% Robustness Check with log levels not percentiles --------------------------
feols(
  log_mats_share ~ 
    corp+
    i(corp, lag_2_l, ref="Corp")+i(corp,lag_2_l^2, ref="Corp")+
    i(corp, lag_2_k, ref="Corp")+i(corp,lag_2_k^2, ref="Corp")+
    sw0(i(corp, lag_m, ref="Corp")+i(corp, lag_m^2, ref="Corp"))+
    i(corp, lag_2_log_sales, ref="Corp")+i(corp, lag_2_log_sales^2, ref="Corp")+
    i(corp, log(exports), ref="Corp")+i(corp, log(exports)^2, ref="Corp")+
    i(corp, log(imported_inputs), ref="Corp")+i(corp, log(imported_inputs)^2, ref="Corp")+
    i(corp, log(age), ref="Corp") + i(corp, I(log(age)^2), ref="Corp")+
    i(corp, log(sales_tax_sales), ref="Corp")+i(corp, I(log(sales_tax_sales)^2), ref="Corp")+
    i(corp, log(sales_tax_purchases), ref="Corp")+i(corp, I(log(sales_tax_purchases)^2), ref="Corp")
  | sic_3,
  cluster = ~ plant + year,
  data = wip_df
) |> coefplot(
  dict = c(dict, new_dict))
# Now, there are no significant effects for any variable

old_dict <- dict
dict <- c(old_dict, new_dict)
## %% Save results ---------------

save(
  reg_sz_dec,
  reg_sz_mg,
  dict,
  file = "Code/Products/917-size.RData"
)

## Plotting Second Review Results --------------------------

reg_sz_dec |> coefplot(
  # dict = dict,
  # main = "Size and tax evasion\nUsing more conservative size measures",
  group = list(
    "Labour (t-2)" = "^^corp::Other:lab_lag2_ntile::",
    "Capital (t-2)" = "^^corp::Other:cap_lag2_ntile::",
    "Materials" = "^^corp::Other:mats_l_ntile::",
    "Revenue (t-2)" = "^^corp::Other:revenue2_ntile::",
    "Exports (t-1)" = "^^corp::Other:exp_l_ntile::",
    "Imports (t-1)" = "^^corp::Other:imp_l_ntile::"
  ),
  pt.pch = 16
)


png(
  file = "Paper/images/917-size-ntiles-robust.png",
  width = 620, height = 720
)

layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE))
par(mar = c(2.5, 2.5, 1.2, 0.8), mgp = c(1.5, 0.4, 0))


coefplot(
  reg_sz_dec[[1]],
  col = 1,
  keep = "lab_lag2",
  main = "Labour (t-2)",
  group = list(
    "Decile" = "^^corp::Other:lab_lag2_ntile::"#,
    # "Capital" = "^^corp::Other:capital_ntile::",
    # "Revenue" = "^^corp::Other:revenue_ntile::",
    # "Exports" = "^^corp::Other:exports_ntile::",
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_sz_dec[[2]],
  keep = "cap_lag2",
  main = "Capital (t-2)",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    "Decile" = "^^corp::Other:cap_lag2_ntile::"#,
    # "Revenue" = "^^corp::Other:revenue_ntile::",
    # "Exports" = "^^corp::Other:exports_ntile::",
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  col = 2,
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_sz_dec[[3]],
  keep = "mats",
  main = "Reported Materials (t-1)",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    "Decile" = "^^corp::Other:mats_l_ntile::"#,
    # "Revenue" = "^^corp::Other:revenue_ntile::",
    # "Exports" = "^^corp::Other:exports_ntile::",
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  col = 3,
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_sz_dec[[4]],
  main = "Revenue (t-2)",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    # " " = "^^corp::Other:capital_ntile::"#,
    "Decile" = "^^corp::Other:revenue2_ntile::"#,
    # "Exports" = "^^corp::Other:exports_ntile::",
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  col = 4,
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_sz_dec[[5]],
  main = "Exports (t-1)",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    # " " = "^^corp::Other:capital_ntile::"#,
    # ` ` = "^^corp::Other:revenue_ntile::"#,
    "Decile" = "^^corp::Other:exp_l_ntile::"#,
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  col = 5,
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_sz_dec[[6]],
  main = "Imports (t-1)",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    # " " = "^^corp::Other:capital_ntile::"#,
    # ` ` = "^^corp::Other:revenue_ntile::"#,
    "Decile" = "^^corp::Other:imp_l_ntile::"#,
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  col = 6,
  dict = dict,
  grid = FALSE
)

dev.off()

## %% Marginal Effects Robust Plots --------------------------

png(
  file = "Paper/images/917-sz-mrg-eff-robust.png",
  width = 620, height = 740
)

reg_sz_mg |> coefplot(
  dict = c(dict,new_dict),
#   as.multiple = TRUE, 
#   col = c(1,rep(2:9, each = 2)),
  pt.pch = 16 ,
  group = list(
    # "Non-Corp x" = "^^Non-Corp x ",
    "Labour (t-2)" = "^^Non-Corp x Labour \\(t-2\\) ",
    "Capital (t-2)" = "^^Non-Corp x Capital \\(t-2\\) ",
    "Materials (t-1)" = "^^Non-Corp x Mats Perc.",
    "Materials (t-2)" = "^^Non-Corp x Mats \\(t-2\\) ",
    "Revenue (t-2)" = "^^Non-Corp x Rev \\(t-2\\) ",
    "Exports (t-1)" = "^^Non-Corp x Exports \\(t-1\\) ",
    "Imports (t-1)" = "^^Non-Corp x Imports \\(t-1\\) "#,
    # "Age" = "^^Non-Corp x Age",
    # "ST—Sales" = "^^Non-Corp x ST—Sales",
    # "ST—Purch" = "^^Non-Corp x ST—Purchases"
  ),
  grid = FALSE,
  horiz = TRUE
)

dev.off()
