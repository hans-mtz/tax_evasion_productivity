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

## %% Segmenting Grid Plot for Slides ---------------------


png(
  file = "Paper/images/915.5-size-ntiles-1.png",
  width = 620, height = 480
)

# layout(matrix(c(1, 2), nrow = 1, byrow = FALSE))
# par(mar = c(2.5, 2, 2, 1)-0.1, mgp = c(1.5, 0.4, 0),
#     family = "serif", cex.main = 1.5, cex.sub = 1.1)
par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

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
  file = "Paper/images/915.5-size-ntiles-2.png",
  width = 620, height = 480
)

# layout(matrix(c(1, 2), nrow = 1, byrow = FALSE))
# par(mar = c(2.5, 2, 2, 1)-0.1, mgp = c(1.5, 0.4, 0),
#     family = "serif", cex.main = 1.5, cex.sub = 1.1)
par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

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


## %% Conditional effects of size on tax evasion ---------------

png(
  file = "Paper/images/915.5-size-cond-effects.png",
  width = 640, height = 480
)

par(family = "serif", cex.main = 1.5, cex.sub = 1.1)

coefplot(
  reg_s_cum[[length(reg_s_cum)]],
  # pt.col = 1:9,
  as.multiple = TRUE,
  main = "Tax Evasion and Firm Size",
  # xlab = "Size ntile",
  group = list(
    "Labour" = "^^Non-Corp x Labo(u)*r ",
    "Capital" = "^^Non-Corp x Capital ",
    "Revenue (t-1)" = "^^Non-Corp x Rev \\(t-1\\)",
    "Exports" = "^^Non-Corp x Exports ",
    "Imports" = "^^Non-Corp x Imports "
  ),
  col = c("gray", rep(c(1,"gray",3,"gray",5), each = 2)),
  dict = dict,
  pt.pch = 16,
  grid = FALSE,
  horiz = FALSE
)
mtext(
    "Conditional Effects of Size on Tax Evasion",
    line = 0.3,
    outer = FALSE,
    family = "serif",
    cex = 1.3,
)
dev.off()

## %% Plotting conditional curves ---------------

reg_s_cum_coefs <- reg_s_cum |> coef()

png(
  file = "Paper/images/915.5-size-cond-curves-1.png",
  width = 720, height = 380
)

# layout(matrix(c(1, 2, 3, 3, 4, 5), nrow = 3, byrow = TRUE))
# par(mar = c(2.5, 2.5, 1.2, 0.8), mgp = c(1.5, 0.4, 0))

# layout(matrix(c(1, 2), nrow = 1, byrow = FALSE))
# par(mar = c(2.5, 2, 2, 1)-0.1, mgp = c(1.5, 0.4, 0))

par(mfcol = c(1, 3), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

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
  sub = "",
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


title(
    # "Expected Tax Evasion",
    # sub = "Conditional Effects",
    line = 1,
    outer = TRUE,
    family = "serif",
    xlab = "Size Decile",
    ylab = "Cost Overreporting",
    # cex.main = 1.5
)
mtext(
    "Expected Tax Evasion",
    line = 2,
    outer = TRUE,
    family = "serif",
    cex = 1.5,
    font = 2
)
mtext(
    "Conditional Expectation Function. Other Variables Fixed at The Median (0.5)",
    line = 0.2,
    outer = TRUE,
    family = "serif",
    cex = 1.3,
    font = 1
)

dev.off()

# png(
#   file = "Paper/images/915.5-size-cond-curves-2.png",
#   width = 720, height = 480.
# )

# par(mfcol = c(1, 2), oma = c(2,2,4,0), mgp = c(1.5, 0.4, 0),
#     family = "serif", cex.main = 1.3, cex.sub = 1.1)
# par(oma = c(2,2,4,0), mgp = c(1.5, 0.4, 0),family = "serif", 
#     cex.main = 1.5, cex.sub = 1.1)
# layout(matrix(c(1, 2, 3, 3, 4, 5), nrow = 3, byrow = TRUE))
# par(mar = c(2.5, 2.5, 1.2, 0.8), mgp = c(1.5, 0.4, 0))

# layout(matrix(c(1, 2), byrow = TRUE))
# par(mar = c(2.5, 2, 2, 1)-0.1, mgp = c(1.5, 0.4, 0))
# par(mfcol = c(1, 2), oma = c(2,2,4,0), mgp = c(1.5, 0.4, 0),
#     family = "serif", cex.main = 1.3, cex.sub = 1.1)



# title(
#     "Expected Tax Evasion",
#     # sub = "Conditional Effects",
#     line = 3,
#     outer = FALSE,
#     family = "serif",
#     xlab = "Size Decile",
#     ylab = "Cost Overreporting",
#     cex.main = 1.5
# )
# mtext(
#     "Conditional Expectation Function. Other Variables Fixed at The Median (0.5)",
#     line = 1.6,
#     outer = FALSE,
#     family = "serif",
#     cex = 1.3,
#     font = 1
# )
# mtext(
#     "Revenue (t-2)",
#     line = 0.5,
#     outer = FALSE,
#     family = "serif",
#     cex = 1.3,
#     font = 2
# )

# dev.off()

png(
  file = "Paper/images/915.5-size-cond-curves-3.png",
  width = 720, height = 480
)

# layout(matrix(c(1, 2, 3, 3, 4, 5), nrow = 3, byrow = TRUE))
# par(mar = c(2.5, 2.5, 1.2, 0.8), mgp = c(1.5, 0.4, 0))

# layout(matrix(c(1, 2), nrow = 1, byrow = FALSE))
# par(mar = c(2.5, 2, 2, 1)-0.1, mgp = c(1.5, 0.4, 0))
par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

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



title(
    # "Expected Tax Evasion",
    # sub = "Conditional Effects",
    line = 1,
    outer = TRUE,
    family = "serif",
    xlab = "Size Decile",
    ylab = "Cost Overreporting",
    # cex.main = 1.5
)
mtext(
    "Expected Tax Evasion",
    line = 2,
    outer = TRUE,
    family = "serif",
    cex = 1.5,
    font = 2
)
mtext(
    "Conditional Expectation Function. Other Variables Fixed at The Median (0.5)",
    line = 0.6,
    outer = TRUE,
    family = "serif",
    cex = 1.3,
    font = 1
)

dev.off()
