## %% load packages and data ---------------
library(tidyverse)
library(fixest)

load("Code/Products/test_data.RData")
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/915-size.RData")
load("Code/Products/917-size.RData")

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

width <- 640
height <- 480

## Segmenting Grid Plot for Slides ---------------------

png(
  file = "Paper/images/917.5-size-ntiles-robust.png",
  width = width, height = height
)

par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

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

title(
    line = 1,
    outer = TRUE,
    family = "serif",
    ylab = "Coefficient Estimate and 95% CI"
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
  file = "Paper/images/917.5-size-ntiles-robust-2.png",
  width = width, height = height
)

par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)




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

title(
    line = 1,
    outer = TRUE,
    family = "serif",
    ylab = "Coefficient Estimate and 95% CI"
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
  file = "Paper/images/917.5-size-ntiles-robust-3.png",
  width = width, height = height
)

par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)



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

title(
    line = 1,
    outer = TRUE,
    family = "serif",
    ylab = "Coefficient Estimate and 95% CI"
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



## %% Marginal Effects Robust Plots --------------------------

png(
  file = "Paper/images/917.5-sz-mrg-eff-robust.png",
  width = width+90, height = height
)

par(family = "serif", cex.main = 1.5, cex.sub = 1.1)

reg_sz_mg |> coefplot(
  dict = dict,
  main = "Tax Evasion and Size: Marginal Effects",
#   as.multiple = TRUE, 
#   col = c(1,rep(2:9, each = 2)),
  pt.pch = 16,
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
  horiz = FALSE
)

dev.off()
