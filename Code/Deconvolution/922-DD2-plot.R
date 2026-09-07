## %% Setup ---------------------

library(tidyverse)
library(fixest)
load("Code/Products/910-reg-results.RData") # dict
load("Code/Products/921-DD2.RData")
# palette("Tableau 10")
# Colorblind-friendly palette from Paul Tol's 9-color "muted" scheme:
# https://personal.sron.nl/~pault/#sec:qualitative
tol_cb_palette <- c(
  "#332288", "#76b1cf", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)
palette(tol_cb_palette)

## %% Plotting Results ---------------------


tbl_pos <- did_grp_reg |> coef() |> names() |> grep('jo_delta_tau_year::Proprietorship:.*Decrease.*Decrease.*Decrease:82',x=_, value=FALSE)
ref_vec <- tbl_pos+1
names(ref_vec) <- "jo_delta_tau_year::Proprietorship:$\\tau_0$ ::  Decrease; $\\tau_1$ ::  Decrease; $\\tau_true$ ::  Decrease:83"

coefplot(
  did_grp_reg,
  keep = "%jo_delta_tau_year::Proprietorship:.*Decrease.*Decrease.*Decrease",
  dict = dict,
  group = list(`Dec:Dec:Dec`="^^jo_delta_tau_year::Proprietorship:.*Decrease.*Decrease.*Decrease:"),
  ref = ref_vec,
  # add = TRUE,
  col = "red", alpha = 0.5,
  # x.shift = 0.2
  ylim = c(-1,1)
)

tbl_pos <- did_grp_reg |> coef() |> names() |> grep('jo_delta_tau_year::Proprietorship:.*Decrease.*Decrease.*No Change:82',x=_, value=FALSE)
ref_vec <- tbl_pos+1
names(ref_vec) <- "jo_delta_tau_year::Proprietorship:$\\tau_0$ ::  Decrease; $\\tau_1$ ::  Decrease; $\\tau_true$ ::  No Change:83"


coefplot(
  did_grp_reg,
  keep = "%jo_delta_tau_year::Proprietorship:.*Decrease.*Decrease.*No Change",
  dict = dict,
  group = list(`Dec:Dec:Dec`="^^jo_delta_tau_year::Proprietorship:.*Decrease.*Decrease.*No Change:"),
  ref = ref_vec,
  add = TRUE,
  col = "blue", 
  alpha = 0.5,
  x.shift = 0.2# ,
  # ylim.add = c(0.8,0)
)

# Reminder:
# works !! make a loop to iterate over what I want to show
# - Propriertorships vs LLCs in exempt industries (CIT effect)
# - LLCs when tau_1 increases (sales tax effect)

## %% Plot Exempt over time ---------------------

png(
  file = "Code/Products/922-exempt-cit.png",
  width = 620, height = 480
)

ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Proprietorship):year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo::Proprietorship:year::83"

coefplot(
  did_grp_reg_tet,
  keep = "(Proprietorship):year::\\d{2}$",
  dict = dict,
  group = list(` `="^^jo::Proprietorship:year::") ,
#   ylim = c(-0.08,0.08),
  col = 1, alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Ltd. Co.):year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo::Ltd. Co.:year::83"

coefplot(
  did_grp_reg_tet,
  keep = "(Ltd. Co.):year::\\d{2}$",
  dict = dict,
  group = list(` `="^^jo::Ltd. Co.:year::") ,
  ylim = c(-0.8,0.8),
  add = TRUE,
  col = 4,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

legend(
  "bottom",
  legend = c("Proprietorships","LLCs"),
  col = c(1,4),
  lwd = 2,
  pch = 16
)

title(
  "Effect of Change in CIT Rates on Input Overreporting\nIndustries Exempt from Sales Taxes"
)

abline(v=c(3,7), lty=2, col="gray")
dev.off()
## %% Plot: LLCs vs Proprietorships when true effective tau increases ---------------------

did_grp_reg_tet |> coef() |> names() 
png(
  file = "Code/Products/922-eff-tau-increase-jo.png",
  # width = 1260, height = 700
  width = 620, height = 480
)

# did_grp_reg_tet |> coef() |> names()
ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Proprietorship).*Increase:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_tau_year::Proprietorship:$\\tau$ :: Increase:83"

coefplot(
  did_grp_reg_tet,
  keep = "(Proprietorship):.*Increase:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_tau_year::Proprietorship:.*Increase:") ,
  # ylim = c(-0.08,0.08),
  col = 1, alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
  # ref = c("jo:Ltd. Co.:year:83"= which(coef(did_grp_reg_tet) %>% names() %>% grep("jo:Ltd. Co.:year::82",x=_))+1)
)

ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Ltd. Co.).*Increase:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_tau_year::Ltd. Co.:$\\tau$ ::  Increase:83"

coefplot(
  did_grp_reg_tet,
  keep = "(Ltd. Co.):.*Increase:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_tau::Ltd. Co.:.*Increase:") ,
  ylim = c(-0.8,0.8),
  add = TRUE,
  col = 4,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

# ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Other).*Increase:82$",x=_, value=FALSE)
# ref_vec <- ref_vec+1
# names(ref_vec) <- "jo_tau_year::Other:$\\tau$ ::  Increase:83"

# coefplot(
#   did_grp_reg_tet,
#   keep = "(Other):.*Increase:\\d{2}$",
#   dict = dict,
#   group = list(` `="^^jo_tau::Other:.*Increase:") ,
#   ylim = c(-0.8,0.8),
#   add = TRUE,
#   col = 3, alpha = 0.5,
#   x.shift = 0.2,
#   ref = ref_vec,
#   main = ""
# )

legend(
  "topleft",
  legend = c("Proprietorships","LLCs"),
  col = c(1,4),
  lwd = 2,
  pch = 16
)

title(
  "Effect of an Increase in Effective Sales Tax Rate on Input Overreporting\nNon-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

## %% Plot: LLCs vs Proprietorships when true effective tau decreases ---------------------


png(
  file = "Code/Products/922-eff-tau-decrease-jo.png",
  # width = 1260, height = 700
  width = 620, height = 480
)

# did_grp_reg_tet |> coef() |> names()
ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Proprietorship).*Decrease:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_tau_year::Proprietorship:$\\tau$ ::  Decrease:83"

coefplot(
  did_grp_reg_tet,
  keep = "(Proprietorship):.*Decrease:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_tau_year::Proprietorship:.*Decrease:") ,
  ylim = c(-0.1,0.2),
  col = 1, alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
  # ref = c("jo:Ltd. Co.:year:83"= which(coef(did_grp_reg_tet) %>% names() %>% grep("jo:Ltd. Co.:year::82",x=_))+1)
)

ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Ltd. Co.).*Decrease:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_tau_year::Ltd. Co.:$\\tau$ ::  Decrease:83"

coefplot(
  did_grp_reg_tet,
  keep = "(Ltd. Co.):.*Decrease:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_tau::Ltd. Co.:.*Decrease:") ,
  ylim = c(-0.8,0.8),
  add = TRUE,
  col = 4,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

legend(
  "topleft",
  legend = c("Proprietorships","LLCs"),
  col = c(1,4),
  lwd = 2,
  pch = 16
)

title(
  "Effect of a Decrease in Effective Sales Tax Rate on Input Overreporting\nNon-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

## %% Plot: LLCs vs Proprietorships when true effective tau remains unchanged ---------------------


png(
  file = "Code/Products/922-eff-tau-no-change-jo.png",
  # width = 1260, height = 700
  width = 620, height = 480
)

# did_grp_reg_tet |> coef() |> names()
ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Proprietorship).*No Change:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_tau_year::Proprietorship:$\\tau$ ::  No Change:83"

coefplot(
  did_grp_reg_tet,
  keep = "(Proprietorship):.*No Change:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_tau_year::Proprietorship:.*No Change:") ,
  # ylim = c(-0.1,0.2),
  col = 1, alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
  # ref = c("jo:Ltd. Co.:year:83"= which(coef(did_grp_reg_tet) %>% names() %>% grep("jo:Ltd. Co.:year::82",x=_))+1)
)

ref_vec <- did_grp_reg_tet |> coef() |> names() |>grep("(Ltd. Co.).*No Change:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_tau_year::Ltd. Co.:$\\tau$ ::  No Change:83"

coefplot(
  did_grp_reg_tet,
  keep = "(Ltd. Co.):.*No Change:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_tau::Ltd. Co.:.*No Change:") ,
  ylim = c(-0.8,0.8),
  add = TRUE,
  col = 4,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

legend(
  "topleft",
  legend = c("Proprietorships","LLCs"),
  col = c(1,4),
  lwd = 2,
  pch = 16
)

title(
  "Input Overreporting, No Change in Effective Sales Tax\nNon-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

## %% LLCs eff tau Decrease  ---------------------

palette("ggplot2")

# did_grp_reg |> coef() |> names()

# load("Code/Products/921-DD2.RData")
did_grp_reg |> coef() |> names()

png(
  file = "Code/Products/922-eff-tau-dec-bkdn.png",
  width = 620, height = 480
)

ref_vec <- did_grp_reg |> coef() |> names() |>grep(".*::Ltd. Co.:.*Decrease.*Decrease.*Decrease:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
# names(ref_vec) <- "jo_delta_tau_year::Ltd. Co.:$\\tau_true$ :: Decrease; $\\tau_0$ :: Decrease; $\\tau_1$ :: Decrease:83"
names(ref_vec) <- coef(did_grp_reg)[ref_vec-1] |> names() |> gsub("(.*)\\d{2}$","\\183",x=_)

coefplot(
  did_grp_reg,
  keep = "Ltd. Co.:.*Decrease.*Decrease.*Decrease:\\d{2}$",
  dict = dict,
  group = list(` `="^^.*::Ltd. Co.:.*Decrease.*Decrease.*Decrease:") ,
  # ylim = c(-0.1,0.2),
  col = 8, alpha = 0.5,
  pt.pch = 18,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- did_grp_reg |> coef() |> names() |>grep(".*::Ltd. Co.:.*Decrease.*Decrease.*Increase:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
# names(ref_vec) <- "jo_delta_tau_year::Ltd. Co.:$\\tau_0$ ::  Decrease; $\\tau_1$ ::  Increase; $\\tau_true$ ::  Decrease:83"
names(ref_vec) <- coef(did_grp_reg)[ref_vec-1] |> names() |> gsub("(.*)\\d{2}$","\\183",x=_)


coefplot(
  did_grp_reg,
  keep = "Ltd. Co.:.*Decrease.*Decrease.*Increase:\\d{2}$",
  dict = dict,
  group = list(` `="^^.*::Ltd. Co.:.*Decrease.*Decrease.*Increase:") ,
  # ylim = c(-0.1,0.2),
  add = TRUE,
  col = 3, alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE,
  pt.pch = 17
)

ref_vec <- did_grp_reg |> coef() |> names() |>grep(".*::Ltd. Co.:.*Decrease.*Decrease.*No Change:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
# names(ref_vec) <- "jo_delta_tau_year::Ltd. Co.:$\\tau_0$ ::  Decrease; $\\tau_1$ ::  No Change; $\\tau_true$ ::  Decrease:83"
names(ref_vec) <- coef(did_grp_reg)[ref_vec-1] |> names() |> gsub("(.*)\\d{2}$","\\183",x=_)


coefplot(
  did_grp_reg,
  keep = "Ltd. Co.:.*Decrease.*Decrease.*No Change:\\d{2}$",
  dict = dict,
  group = list(` `="^^.*::Ltd. Co.:.*Decrease.*Decrease.*No Change:") ,
  # ylim = c(-0.1,0.2),
  add = TRUE,
  col = 7, alpha = 0.5,
  x.shift = 0.4,
  ref = ref_vec,
  main = "",
  grid = FALSE,
  pt.pch = 15
)

title(
  "Input Overreporting, Decrease in Effective Sales Tax\nNon-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="lightgray")

legend(
  x="bottomleft",
  legend = c("Sales S.T.R. Decrease; Purchase S.T.R. Decrease","Decrease; Increase","Decrease; No Change"),
  col = c(8,3,7),
  lwd = 2,
  pch = c(18,17,15)
)

dev.off()


## %% LLCs eff tau no change ---------------------
# TODO: change colors to improve readability. Color code might help.
# Like increasing = green, no change = yellow/orange, decreasing = red
# Also, correct plot. Some lines are not being displayed. Not a problem with ggplot.


# load("Code/Products/921-DD2.RData")
# did_grp_reg |> coef() |> names()

#  [38] "jo::Ltd. Co.:delta_tx_83::$\\tau$ :: No Change; $\\tau_0$ :: Decrease; $\\tau_1$ :: Decrease"             
#  [39] "jo::Ltd. Co.:delta_tx_83::$\\tau$ :: No Change; $\\tau_0$ :: Increase; $\\tau_1$ :: Increase"             
#  [40] "jo::Ltd. Co.:delta_tx_83::$\\tau$ :: No Change; $\\tau_0$ :: No Change; $\\tau_1$ :: Increase"            
#  [41] "jo::Ltd. Co.:delta_tx_83::$\\tau$ :: No Change; $\\tau_0$ :: No Change; $\\tau_1$ :: No Change"   
# palette("ggplot2")

png(
  file = "Code/Products/922-eff-tau-no-change-bkdn.png",
  width = 820, height = 1220,
  pointsize = 14
  # bg = "transparent"
  # type = "cairo"
)

ref_vec <- did_grp_reg |> coef() |> names() |>grep(".*::Ltd. Co.:.*No Change.*Decrease.*Decrease:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
# names(ref_vec) <- "jo_delta_tau_year::Ltd. Co.:$\\tau_0$ ::  Decrease; $\\tau_1$ ::  Decrease; $\\tau_true$ ::  No Change:83"
names(ref_vec) <- coef(did_grp_reg)[ref_vec-1] |> names() |> gsub("(.*)\\d{2}$","\\183",x=_)


coefplot(
  did_grp_reg,
  keep = "Ltd. Co.:.*No Change.*Decrease.*Decrease:\\d{2}$",
  dict = dict,
  group = list(` `="^^.*::Ltd. Co.:.*No Change.*Decrease.*Decrease:") ,
  ylim = c(-0.15,0.25),
  # add = TRUE,
  col = 2, #alpha = 0.5,
  # lwd = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE,
  pt.pch = 25,
  pt.bg = 2,
  pt.col = 2
)

ref_vec <- did_grp_reg |> coef() |> names() |>grep(".*::Ltd. Co.:.*No Change.*No Change.*No Change:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
# names(ref_vec) <- "jo_delta_tau_year::Ltd. Co.:$\\tau_0$ ::  No Change; $\\tau_1$ ::  No Change; $\\tau_true$ ::  No Change:83"
names(ref_vec) <- coef(did_grp_reg)[ref_vec-1] |> names() |> gsub("(.*)\\d{2}$","\\183",x=_)

coefplot(
  did_grp_reg,
  grid = FALSE,
  keep = "Ltd. Co.:.*No Change.*No Change.*No Change:\\d{2}$",
  dict = dict,
  group = list(` `="^^.*::Ltd. Co.:.*No Change.*No Change.*No Change:") ,
  # ylim = c(-0.1,0.2),
  # ylim = c(-0.9,0.9),
  add = TRUE,
  # lwd = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  pt.pch = 15,
  # pt.bg = 7 ,
  col = 7#, #alpha = 0.5,
  # pt.col = 7
)

ref_vec <- did_grp_reg |> coef() |> names() |>grep(".*::Ltd. Co.:.*No Change.*Increase.*Increase:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
# names(ref_vec) <- "jo_delta_tau_year::Ltd. Co.:$\\tau_0$ ::  Increase; $\\tau_1$ ::  Increase; $\\tau_true$ ::  No Change:83"
names(ref_vec) <- coef(did_grp_reg)[ref_vec-1] |> names() |> gsub("(.*)\\d{2}$","\\183",x=_)


coefplot(
  did_grp_reg,
  keep = "Ltd. Co.:.*No Change.*Increase.*Increase:\\d{2}$",
  dict = dict,
  group = list(` `="^^.*::Ltd. Co.:.*No Change.*Increase.*Increase:") ,
  # ylim = c(-0.15,0.35),
  # ylim = c(-0.10,0.25),
  add = TRUE,
  col = 3, #alpha = 0.5,
  x.shift = 0.4,
  # lwd = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE,
  pt.pch = 17#,
  # pt.bg = 3,
  # pt.col = 3
)

ref_vec <- did_grp_reg |> coef() |> names() |>grep(".*::Ltd. Co.:.*No Change.*No Change.*Increase:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
# names(ref_vec) <- "jo_delta_tau_year::Ltd. Co.:$\\tau_0$ ::  No Change; $\\tau_1$ ::  Increase; $\\tau_true$ ::  No Change:83"
names(ref_vec) <- coef(did_grp_reg)[ref_vec-1] |> names() |> gsub("(.*)\\d{2}$","\\183",x=_)


coefplot(
  did_grp_reg,
  keep = "Ltd. Co.:.*No Change.*No Change.*Increase:\\d{2}$",
  dict = dict,
  group = list(` `="^^.*::Ltd. Co.:.*No Change.*No Change.*Increase:") ,
  # ylim = c(-0.1,0.2),
  # ylim = c(-0.10,0.35),
  add = TRUE,
  col = 5,# alpha = 0.5,
  # lwd = 0.5,
  x.shift = 0.6,
  ref = ref_vec,
  main = "",
  grid = FALSE,
  pt.pch = 17#,
  # pt.bg = 5,
  # pt.col = 5
)

title(
  "Input Overreporting, No Change in Effective Sales Tax\nIncrease in Purchase S.T.R.; Non-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="lightgray")

legend(
  x="bottom",
  legend = c("Sales S.T.R. Decrease; Purchase S.T.R. Decrease","No Change; No Change","Increase; Increase","No Change; Increase"),
  col = c(2,7,3,5),
  pt.bg = c(2,7,3,5),
  lwd = 2,
  pch = c(25,22,24,24)
)

dev.off()

## %% Grouping by Exempt and Non-Exempt Industries ---------------------

## %% Plot Exempt and Non-Corp over time ---------------------

# reg_grp_ex_ncrp |> coef() |> names()
palette(tol_cb_palette)

png(
  file = "Code/Products/922-exempt-ncrp.png",
  width = 620, height = 480
)

ref_vec <- reg_grp_ex_ncrp |> coef() |> names() |>grep("Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp_exempt_year::Other:Taxed:83"

coefplot(
  reg_grp_ex_ncrp,
  keep = "Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^corp_exempt_year::Other:Taxed:") ,
  ylim = c(-0.05,0.1),
  # add = TRUE,
  col = 4,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_grp_ex_ncrp |> coef() |> names() |>grep("corp::Other:year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp::Other:year::83"

coefplot(
  reg_grp_ex_ncrp,
  keep = "^corp::Other",
  dict = dict,
  group = list(` `="^^corp::Other:year::") ,
#   ylim = c(-0.08,0.08),
  col = 1, alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE,
  add = TRUE
)

legend(
  "topleft",
  legend = c("Exempted","Taxed"),
  col = c(1,4),
  lwd = 2,
  pch = 16
)

title(
  "Effect of Change in CIT and ST Rates on Input Overreporting\nSales Tax Exempt vs. Non-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="gray")
dev.off()

## %% Plot: JO and Exempt Industries over time ---------------------

reg_grp_ex_jo |> coef() |> names()

png(
  file = "Code/Products/922-exempt-jo.png",
  width = 620, height = 480
)

ref_vec <- reg_grp_ex_jo |> coef() |> names() |>grep("jo::Proprietorship:year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo::Proprietorship:year::83"

coefplot(
  reg_grp_ex_jo,
  keep = "(Proprietorship):year::\\d{2}$",
  dict = dict,
  group = list(` `="^^jo::Proprietorship:year::") ,
  # ylim = c(-0.08,0.08),
  col = 1, alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_grp_ex_jo |> coef() |> names() |>grep("jo::(Ltd. Co.):year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo::Ltd. Co.:year::83"

coefplot(
  reg_grp_ex_jo,
  keep = "(Ltd. Co.):year::\\d{2}$",
  dict = dict,
  group = list(` `="^^jo::Ltd. Co.:year::") ,
  ylim = c(-0.8,0.8),
  add = TRUE,
  col = 4,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)


legend(
  "topleft",
  legend = c("Proprietorships","LLCs"),
  col = c(1,4),
  lwd = 2,
  pch = 16
)

title(
  "Effect of a Change in CIT Rate on Input Overreporting\nExempt Industries"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

## %% Grouping by Exempt and Non-Exempt Industries ---------------------

## %% Plot Exempt and Non-Corp over time ---------------------

reg_nc_exe_het|> coef() |> names()
palette(tol_cb_palette)

png(
  file = "Code/Products/922-exempt-ncrp-het.png",
  width = 620, height = 480
)

ref_vec <- reg_nc_exe_het |> coef() |> names() |>grep("Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp_exempt_year_het::Other:Taxed:83"

coefplot(
  reg_nc_exe_het,
  keep = "Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^corp_exempt_year_het::Other:Taxed:") ,
  ylim = c(-0.06,0.1),
  # add = TRUE,
  col = 2,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_nc_exe_het |> coef() |> names() |>grep("Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp_exempt_year_het::Other:Exempt:83"

coefplot(
  reg_nc_exe_het,
  keep = "Exempt:\\d{2}$",
  dict = dict,
  group = list(` `="^^corp_exempt_year_het::Other:Exempt:") ,
#   ylim = c(-0.08,0.08),
  col = 1,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE,
  add = TRUE
)

legend(
  "topleft",
  legend = c("Exempted","Taxed"),
  col = c(1,2),
  lwd = 2,
  pch = 16
)

title(
  "Effect of 1983 Fiscal Reform on Input Overreporting\nExempt vs. Non-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="gray")
dev.off()

## %% Plot: JO and Exempt Industries over time By JO ---------------------

reg_jo_exe_het |> coef() |> names()

png(
  file = "Code/Products/922-exempt-PRT-het.png",
  width = 620, height = 480
)

ref_vec <- reg_jo_exe_het |> coef() |> names() |>grep("::Proprietorship:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Proprietorship:Exempt:83"

coefplot(
  reg_jo_exe_het,
  keep = "(Proprietorship):Exempt:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Proprietorship:Exempt:") ,
  ylim = c(-0.08,0.12),
  col = 1,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_jo_exe_het |> coef() |> names() |>grep("::Proprietorship:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Proprietorship:Taxed:83"

coefplot(
  reg_jo_exe_het,
  keep = "(Proprietorship):Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Proprietorship:Taxed:") ,
  # ylim = c(-0.8,0.8),
  add = TRUE,
  col = 2,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)


legend(
  "topleft",
  legend = c("Exempted","Taxed"),
  col = c(1,2),
  lwd = 2,
  pch = 16
)

title(
  "Effect of 1983 Fiscal Reform on Proprietorships' Input Overreporting\nExempt vs. Non-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

## %% Plot: JO and Exempt Industries over time By JO ---------------------

reg_jo_exe_het |> coef() |> names()

png(
  file = "Code/Products/922-exempt-LLC-het.png",
  width = 620, height = 480
)

ref_vec <- reg_jo_exe_het |> coef() |> names() |>grep("::Ltd. Co.:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Ltd. Co.:Exempt:83"

coefplot(
  reg_jo_exe_het,
  keep = "(Ltd. Co.):Exempt:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Ltd. Co.:Exempt:") ,
  ylim = c(-0.08,0.12),
  col = 1,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_jo_exe_het |> coef() |> names() |>grep("::Ltd. Co.:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Ltd. Co.:Taxed:83"

coefplot(
  reg_jo_exe_het,
  keep = "(Ltd. Co.):Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Ltd. Co.:Taxed:") ,
  # ylim = c(-0.8,0.8),
  add = TRUE,
  col = 2,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)


legend(
  "topleft",
  legend = c("Exempted","Taxed"),
  col = c(1,2),
  lwd = 2,
  pch = 16
)

title(
  "Effect of 1983 Fiscal Reform on LLCs' Input Overreporting\nExempt vs. Non-Exempt Industries"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()


## %% Plot: JO and Exempt Industries over time Only Exempt ---------------------

reg_jo_exe_het |> coef() |> names()

png(
  file = "Code/Products/922-exempt-jo-het.png",
  width = 620, height = 480
)

ref_vec <- reg_jo_exe_het |> coef() |> names() |>grep("::Proprietorship:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Proprietorship:Exempt:83"

coefplot(
  reg_jo_exe_het,
  keep = "(Proprietorship):Exempt:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Proprietorship:Exempt:") ,
  ylim = c(-0.08,0.12),
  col = 1,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_jo_exe_het |> coef() |> names() |>grep("::Ltd. Co.:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Ltd. Co.:Exempt:83"

coefplot(
  reg_jo_exe_het,
  keep = "(Ltd. Co.):Exempt:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Ltd. Co.:Exempt:") ,
  # ylim = c(-0.8,0.8),
  add = TRUE,
  col = 2,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)


legend(
  "topleft",
  legend = c("Proprietorships","LLCs"),
  col = c(1,2),
  lwd = 2,
  pch = 16
)

title(
  "Effect of 1983 Fiscal Reform on Input Overreporting; Exempt Industries\nProprietorships vs. LLCs"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

## %% Plot: JO and Exempt Industries over time Taxed only---------------------

reg_jo_exe_het |> coef() |> names()

png(
  file = "Code/Products/922-taxed-jo-het.png",
  width = 620, height = 480
)

ref_vec <- reg_jo_exe_het |> coef() |> names() |>grep("::Ltd. Co.:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Ltd. Co.:Taxed:83"

coefplot(
  reg_jo_exe_het,
  keep = "(Ltd. Co.):Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Ltd. Co.:Taxed:") ,
  ylim = c(-0.08,0.12),
  col = 2,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_jo_exe_het |> coef() |> names() |>grep("::Proprietorship:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Proprietorship:Taxed:83"

coefplot(
  reg_jo_exe_het,
  keep = "(Proprietorship):Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Proprietorship:Taxed:") ,
  # ylim = c(-0.8,0.8),
  add = TRUE,
  col = 1,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)


legend(
  "topleft",
  legend = c("Proprietorship","LLCs"),
  col = c(1,2),
  lwd = 2,
  pch = 16
)

title(
  "Effect of 1983 Fiscal Reform on Input Overreporting; Non-Exempt Industries\nProprietorships vs. LLCs"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

