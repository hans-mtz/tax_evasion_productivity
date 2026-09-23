## Cross-test table (2026-09-19): each estimator's best point evaluated under each estimator's test, REAL data (lag_m, trim 0.5%).
## Sources: ELVIS TS = 2*n*Lhat from Code/Products/1440-elvis-at-{ELVIS,MSL,MSM}.csv (delta0,gamma free, seeded from ELVIS's fit; n=32,232);
## naive MSM J from Code/Products/1442-MSM-eval-lag_m-S250-overID.RData (test inversion, chi2_5); MSL LR and plant-clustered score statistics from
## Code/Deconvolution/1441-msl-lr-at-points.R and 1443-msl-score-test.R (sigma_psi profiled; chi2_4) -- values copied from those runs' output.
library(tidyverse); library(tinytable); source("Code/Deconvolution/050-render-tbls.R")
ts <- sapply(c("ELVIS", "MSL", "MSM"), function(nm) { d <- read.csv(sprintf("Code/Products/1440-elvis-at-%s.csv", nm)); 2 * d$n[1] * d$Lhat[1] })
load("Code/Products/1442-MSM-eval-lag_m-S250-overID.RData"); Jm <- warmstart_lag_m$J_pts[c("ELVIS", "MSL", "MSM_best"), "J"]
lr <- c(ELVIS = 46481.4, MSL = 0, MSM = 23433.3); sc <- c(ELVIS = 3392.5, MSL = 0.0, MSM = 2701.4)
fmt <- function(x, crit) { s <- vapply(x, function(v) format(round(v, if (v < 100 && v > 0) 1 else 0), big.mark = ",", trim = TRUE, nsmall = 0), ""); ifelse(x < crit, paste0("\\textbf{", s, "}"), s) }   # bold = not rejected at 95%
tbl <- tibble(
    Point = c("ELVIS", "MSL", "Naive MSM"),
    lam = c("$5.4\\times10^{-7}$", "$2.7\\times10^{-5}$", "$6.8\\times10^{-4}$"),
    d0 = c("3.46", "7.42", "$-5.42$"), d1 = c("4.30", "3.24", "14.96"), d2 = c("0.54", "0.23", "3.42"),
    e = fmt(unname(ts), qchisq(.95, 9)), m = fmt(unname(Jm), qchisq(.95, 5)), l = fmt(unname(lr), qchisq(.95, 4)), s = fmt(unname(sc), qchisq(.95, 4)))
print(tbl)
tt_obj <- tt(tbl, align = "lcccccccc", notes = "Real data (lag\\_m, trim 0.5\\%). Each row: one estimator's best point; each column: one estimator's test evaluated at that point (fixed $\\theta$, test inversion). Bold = not rejected at 95\\%. ELVIS: $\\delta_0,\\gamma$ profiled; MSL: $\\sigma_\\psi$ profiled, plant-clustered score.")
colnames(tt_obj) <- c(" ", "$\\hat\\lambda$", "$\\hat\\delta_0$", "$\\hat\\delta_1$", "$\\hat\\delta_2$", "$TS$ ($\\chi^2_9$: 16.9)", "$J$ ($\\chi^2_5$: 11.1)", "$LR$ ($\\chi^2_4$: 9.5)", "Score ($\\chi^2_4$: 9.5)")
tt_obj <- tt_obj |> group_tt(j = list("Point evaluated" = 2:5, "ELVIS" = 6, "Naive MSM" = 7, "MSL" = 8:9)) |> style_tt(i = "notes", fontsize = 0.7)
render_png_tt_tbl(tt_obj, "1444-crosstest-table"); cat("Saved: Paper/tbls/1444-crosstest-table.png\n")
