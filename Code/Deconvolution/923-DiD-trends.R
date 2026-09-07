## %% Setup ---------------------
library(tidyverse)
library(fixest)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/910-reg-results.RData") # wip_df
load("Code/Products/i_elas.RData")
load("Code/Products/921-DD2.RData")
tol_cb_palette <- c(
  "#332288", "#76b1cf", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)
palette(tol_cb_palette)
## %% De-trending ---------------------

# library(fixest)
# library(dplyr)

wip_df <- wip_df %>%
  mutate(
    t_rel = as.integer(as.character(year)) - 83L,          # numeric time
    grp   = interaction(corp, exempt_ind, drop = TRUE)     # Corp/Non-Corp × Exempt/Taxed
  )

# 1) Fit group-specific linear trends using only pre-83 data
trend_fit <- feols(
  log_mats_share ~ 0 + i(grp) + i(grp, t_rel),             # intercepts + slopes by grp
  data   = wip_df,
  subset = ~ as.integer(as.character(year)) < 83
)

# 2) Detrend outcome using those pre-period slopes
wip_df <- wip_df %>%
  mutate(
    pred_trend          = predict(trend_fit, newdata = .),
    log_mats_share_det  = log_mats_share - pred_trend      # residualized outcome
  )

# 3) Re-run your DiD on the detrended outcome
did_det <- feols(
  log_mats_share_det ~ corp +
    i(corp, year, ref = "Corp", ref2 = "83") +
    i(corp, exempt_ind, ref = "Corp", ref2 = "Exempt") +
    i(corp_exempt_year, "Base") | sic_3,
  cluster = ~ plant + year,
  data    = wip_df
)

reg_nc_exe_het_det <- feols(
    log_mats_share_det ~ corp + 
    i(corp, exempt_ind, ref = "Corp") +
    i(corp_exempt_year_het, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  )

reg_nc_exe_het_det |> etable()

## %% Coefficient plots ---------------------


## %% Plot Exempt and Non-Corp over time ---------------------

reg_nc_exe_het_det|> coef() |> names()


png(
  file = "Code/Products/923-exempt-ncrp-het-det.png",
  width = 620, height = 480
)

ref_vec <- reg_nc_exe_het_det |> coef() |> names() |>grep("Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp_exempt_year_het::Other:Taxed:83"

coefplot(
  reg_nc_exe_het_det,
  keep = "Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^corp_exempt_year_het::Other:Taxed:") ,
  ylim = c(-0.14,0.18),
  # add = TRUE,
  col = 2,# alpha = 0.5,
  x.shift = 0.2,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_nc_exe_het_det |> coef() |> names() |>grep("Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp_exempt_year_het::Other:Exempt:83"

coefplot(
  reg_nc_exe_het_det,
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
  "Effect of 1983 Fiscal Reform on Input Overreporting\nExempt vs. Non-Exempt Industries\nDetrended Outcomes"
)

abline(v=c(3,7), lty=2, col="gray")
dev.off()

## %% Detrend by JO ------------------------------


wip_df <- wip_df %>%
  mutate(
    # t_rel = as.integer(as.character(year)) - 83L,          # numeric time
    grp_jo   = interaction(jo, exempt_ind, drop = TRUE)     # Corp/Non-Corp × Exempt/Taxed
  )

# 1) Fit group-specific linear trends using only pre-83 data
trend_fit_jo <- feols(
  log_mats_share ~ 0 + i(grp_jo) + i(grp_jo, t_rel),             # intercepts + slopes by grp
  data   = wip_df,
  subset = ~ as.integer(as.character(year)) < 83
)

# 2) Detrend outcome using those pre-period slopes
wip_df <- wip_df %>%
  mutate(
    pred_trend_jo          = predict(trend_fit_jo, newdata = .),
    log_mats_share_det_jo  = log_mats_share - pred_trend_jo      # residualized outcome
  )

# 3) Re-run your DiD on the detrended outcome
reg_jo_exe_het_det <- feols(
    log_mats_share_det_jo ~ jo + i(jo, exempt_ind, ref = "Corporation") + i(jo_exempt_year_het, "Base")| sic_3,
    cluster = ~ plant + year,
    data = wip_df
  )
reg_jo_exe_het_det |> etable(
    dict = dict
  )

## %% Plot Exempt and JO over time ---------------------


## %% Plot: JO and Exempt Industries over time By JO ---------------------

reg_jo_exe_het_det |> coef() |> names()

png(
  file = "Code/Products/923-exempt-PRT-het-det.png",
  width = 620, height = 480
)

ref_vec <- reg_jo_exe_het_det |> coef() |> names() |>grep("::Proprietorship:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Proprietorship:Exempt:83"

coefplot(
  reg_jo_exe_het_det,
  keep = "(Proprietorship):Exempt:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Proprietorship:Exempt:") ,
  ylim = c(-0.12,0.10),
  col = 1,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_jo_exe_het_det |> coef() |> names() |>grep("::Proprietorship:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Proprietorship:Taxed:83"

coefplot(
  reg_jo_exe_het_det,
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
  "Effect of 1983 Fiscal Reform on Proprietorships' Input Overreporting\nExempt vs. Non-Exempt Industries\nDetrended Outcomes"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

## %% Plot: JO and Exempt Industries over time By JO ---------------------

reg_jo_exe_het_det |> coef() |> names()

png(
  file = "Code/Products/923-exempt-LLC-het-det.png",
  width = 620, height = 480
)

ref_vec <- reg_jo_exe_het_det |> coef() |> names() |>grep("::Ltd. Co.:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Ltd. Co.:Exempt:83"

coefplot(
  reg_jo_exe_het_det,
  keep = "(Ltd. Co.):Exempt:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Ltd. Co.:Exempt:") ,
  ylim = c(-0.08,0.28),
  col = 1,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_jo_exe_het_det |> coef() |> names() |>grep("::Ltd. Co.:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Ltd. Co.:Taxed:83"

coefplot(
  reg_jo_exe_het_det,
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
  "Effect of 1983 Fiscal Reform on LLCs' Input Overreporting\nExempt vs. Non-Exempt Industries\nDetrended Outcomes"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

###### HERE !!!!!!##########

## %% Plot: JO and Exempt Industries over time Only Exempt ---------------------

reg_jo_exe_het_det |> coef() |> names()

png(
  file = "Code/Products/923-exempt-jo-het-det.png",
  width = 620, height = 480
)

ref_vec <- reg_jo_exe_het_det |> coef() |> names() |>grep("::Proprietorship:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Proprietorship:Exempt:83"

coefplot(
  reg_jo_exe_het_det,
  keep = "(Proprietorship):Exempt:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Proprietorship:Exempt:") ,
#   ylim = c(-0.08,0.12),
  col = 1,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_jo_exe_het_det |> coef() |> names() |>grep("::Ltd. Co.:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Ltd. Co.:Exempt:83"

coefplot(
  reg_jo_exe_het_det,
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
  "Effect of 1983 Fiscal Reform on Input Overreporting; Exempt Industries\nProprietorships vs. LLCs\nDetrended Outcomes"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

## %% Plot: JO and Exempt Industries over time Taxed only---------------------

reg_jo_exe_het_det |> coef() |> names()

png(
  file = "Code/Products/923-taxed-jo-het-det.png",
  width = 620, height = 480
)

ref_vec <- reg_jo_exe_het_det |> coef() |> names() |>grep("::Ltd. Co.:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Ltd. Co.:Taxed:83"

coefplot(
  reg_jo_exe_het_det,
  keep = "(Ltd. Co.):Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Ltd. Co.:Taxed:") ,
  ylim = c(-0.12,0.28),
  col = 2,# alpha = 0.5,
  ref = ref_vec,
  main = "",
  grid = FALSE
)

ref_vec <- reg_jo_exe_het_det |> coef() |> names() |>grep("::Proprietorship:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_year_het::Proprietorship:Taxed:83"

coefplot(
  reg_jo_exe_het_det,
  keep = "(Proprietorship):Taxed:\\d{2}$",
  dict = dict,
  group = list(` `="^^jo_exempt_year_het::Proprietorship:Taxed:") ,
  # ylim = c(-0.6,0.8),
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
  "Effect of 1983 Fiscal Reform on Input Overreporting; Non-Exempt Industries\nProprietorships vs. LLCs\nDetrended Outcomes"
)

abline(v=c(3,7), lty=2, col="gray")

dev.off()

