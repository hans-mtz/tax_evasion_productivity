## %% load packages and data ---------------
library(tidyverse)
library(fixest)

load("Code/Products/test_data.RData")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/911-all_inds-2.RData")
load("Code/Products/915-size.RData")


tol_cb_palette <- c(
  "#332288", "#76b1cf", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)
wong_cb_palette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536",
  "#0072B2", "#D55E00", "#CC79A7"
)
palette(wong_cb_palette)

## %% data wrangling ---------------

wip_df <-wip_df %>%
    rename(
        rev_l_ntile = revenue_ntile,
        rev_l2_ntile = revenue2_ntile
    ) %>%
    mutate(
        rev_ntile = ntile(log_sales, 10),
        rev_pct = percent_rank(log_sales),
        rev_ntile = as.factor(rev_ntile),
        aux = 1
    )

# I want all the deciles to be included in the regression,
# so, I set the contrasts to be FALSE, 
# which means that all levels of the factor will be included 
# in the model matrix.

contrasts(wip_df$rev_ntile,10) <- contrasts(wip_df$rev_ntile, contrasts=F)

## %% Regressions: Possibly wrong std errors ---------------

reg_s_deciles <- feols(
    log_mats_share ~ sw(
        i(corp, i.labour_ntile, ref = "Corp"),
        i(corp,i.capital_ntile, ref="Corp"),
        i(corp,i.rev_ntile, ref="Corp") + rev_ntile,
        i(corp,i.rev_l_ntile, ref="Corp"),
        i(corp,i.rev_l2_ntile, ref="Corp"),
        i(corp,i.exports_ntile, ref="Corp"),
        i(corp,i.imports_ntile, ref="Corp")
    )| sic_3,
  cluster = ~ plant + year,
  data = wip_df
    )

reg_s_deciles |> etable(dict=dict)

## %% Saving results ---------------

save(
    reg_s_deciles, wip_df,
    file = "Code/Products/915.1-size.RData"
)


## Plotting ----------------------- 

png(
  file = "Paper/images/915.1-size-ntiles-1.png",
  width = 620, height = 480
)


par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

coefplot(
  reg_s_deciles[[1]],
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
#   dict = dict,
  # ylim = c(-0.25,0.3),
  grid = FALSE
  # horiz = TRUE
)

coefplot(
  reg_s_deciles[[2]],
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

title(
    # "Expected Tax Evasion",
    # sub = "Conditional Effects",
    line = 1,
    outer = TRUE,
    family = "serif",
    # xlab = "Size Decile",
    ylab = "Coefficient Estimate and 95% CI",
    # cex.main = 1.5
)
mtext(
    "Average Tax Evasion by Size Decile",
    line = 1,
    outer = TRUE,
    family = "serif",
    cex = 1.5,
    font = 2
)

dev.off()

png(
  file = "Paper/images/915.1-size-ntiles-2.png",
  width = 740, height = 480
)

par(mfcol = c(1, 3), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

coefplot(
  reg_s_deciles[[3]],
  # col = tol_cb_palette,
  main = "Revenue",
  keep = "Other:rev_ntile",
  # xlab = "Size ntile",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    # " " = "^^corp::Other:capital_ntile::"#,
    "Decile" = "^^corp::Other:rev_ntile::"#,
    # "Exports" = "^^corp::Other:exports_ntile::",
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  # add = TRUE,
  # x.shift = 0.4,
  col = 3,
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_s_deciles[[4]],
  # col = tol_cb_palette,
  main = "Revenue (t-1)",
  # drop = "NaN",
  # xlab = "Size ntile",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    # " " = "^^corp::Other:capital_ntile::"#,
    # ` ` = "^^corp::Other:revenue_ntile::"#,
    "Decile" = "^^corp::Other:rev_l_ntile::"#,
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  # add = TRUE,
  # x.shift = 0.4,
  col = 4,
  dict = dict,
  grid = FALSE
)

coefplot(
  reg_s_deciles[[5]],
  # col = tol_cb_palette,
  main = "Revenue (t-2)",
  # drop = "NaN",
  # xlab = "Size ntile",
  group = list(
    # " " = "^^corp::Other:Labour_ntile::"#,
    # " " = "^^corp::Other:capital_ntile::"#,
    # ` ` = "^^corp::Other:revenue_ntile::"#,
    "Decile" = "^^corp::Other:rev_l2_ntile::"#,
    # "Imports" = "^^corp::Other:imports_ntile::"
  ),
  # add = TRUE,
  # x.shift = 0.4,
  col = 5,
  dict = dict,
  grid = FALSE
)

title(
    # "Expected Tax Evasion",
    # sub = "Conditional Effects",
    line = 1,
    outer = TRUE,
    family = "serif",
    # xlab = "Size Decile",
    ylab = "Coefficient Estimate and 95% CI",
    # cex.main = 1.5
)
mtext(
    "Average Tax Evasion by Size Decile",
    line = 1,
    outer = TRUE,
    family = "serif",
    cex = 1.5,
    font = 2
)

dev.off()

