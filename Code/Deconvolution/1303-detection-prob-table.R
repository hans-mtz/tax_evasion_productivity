## Detection-probability table (Mean/Median/P95/P99/Max of e, and the implied
## q(e)=lambda_hat*e), grouped by instrument -- for the "What Detection
## Probability Does This Imply?" slide (650-stage2-prelim-results.qmd).
## Values come from a fresh revenue_baseline forward-simulation (2026-09-16)
## at each instrument's CURRENT best-fit theta -- the 3D-cube point for
## lag_m (lambda=5.427e-7, delta0=3.464, delta1=4.3, delta2=0.54), and the
## unchanged lambdagrid point for lag_2_cal_W (lambda=5.427e-7,
## delta0=10.206, delta1=7.460, delta2=0.972). Supersedes an earlier
## pre-cube ballpark for lag_m (E[e]=2,545/Med[e]=625 -- computed at the
## lambdagrid's own lambda=3.501e-7, before the cube refined lag_m's point).
## Purpose (user's own framing): show sups that the top ~1% of evaders are
## what's pulling E[e] (hence the fitted lambda) down -- Mean/Median/P95/P99/
## Max side by side makes the tail's leverage visible directly.

library(tidyverse)
library(tinytable)
source("Code/Deconvolution/050-render-tbls.R")

## tinytable's LaTeX backend mishandles unicode script-W and parses a bare
## underscore as math mode (hit directly in 1237-stage2-estimates-so-far-
## plot-table.R) -- plain ASCII group labels only, same convention reused.
INS_LABELS_ASCII <- c(lag_m = "m*(t-1)", lag_2_cal_W = "W(t-2)")
LAMBDA <- 5.427e-7

stats_for <- function(ins) {
    d <- read.csv(sprintf("Code/Products/1302-omega-e-percentiles-%s.csv", ins))
    e <- d$e_mean
    tibble(
        Stat = c("Mean", "Median", "P95", "P99", "Max"),
        e = c(mean(e), median(e), quantile(e, 0.95, names = FALSE),
              quantile(e, 0.99, names = FALSE), max(e))
    ) %>% mutate(q = LAMBDA * e)
}

lm <- stats_for("lag_m")
w  <- stats_for("lag_2_cal_W")
stopifnot(identical(lm$Stat, w$Stat))

tbl <- tibble(
    Stat = lm$Stat,
    e1 = format(round(lm$e), big.mark = ","),
    q1 = sprintf("%.2f%%", 100 * lm$q),
    e2 = format(round(w$e), big.mark = ","),
    q2 = sprintf("%.2f%%", 100 * w$q)
)
write.csv(tbl, "Code/Products/1303-detection-prob-table.csv", row.names = FALSE)
print(tbl)

## Same "%" escape as 1301-headline-pct-table.R: LaTeX reads a raw "%" as a
## comment marker and silently merges rows -- escape only the LaTeX-bound copy.
tbl_tex <- tbl %>% mutate(across(everything(), ~ gsub("%", "\\\\%", .x)))

tt_obj <- tt(tbl_tex, notes = "Implied detection probability $q(e)=\\hat\\lambda\\cdot e$ across the evasion distribution, using forward-simulation estimates.")
colnames(tt_obj) <- c("Stat", "$e$", "$q(e)$", "$e$", "$q(e)$")
tt_obj <- tt_obj |> group_tt(j = list(
    "$m^*_{t-1}$"  = 2:3,
    "$\\tilde{\\mathcal{W}}_{t-2}$"   = 4:5
    )) |>
    style_tt(i = "notes", fontsize = 0.8)

render_png_tt_tbl(tt_obj, "1303-detection-prob-table")
cat("Saved: Paper/tbls/1303-detection-prob-table.png\n")
