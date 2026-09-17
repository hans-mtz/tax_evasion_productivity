## Headline stage-2 estimates table, transposed to 3 columns (Stat, m*(t-1),
## W(t-2)) -- for the "Headline Estimates" slide (650-stage2-prelim-
## results.qmd). Pulled directly from each instrument's own source CSV
## (never hand-typed) so the table can't silently go stale the way the
## detection-probability table did (see 1303-detection-prob-table.R):
##   - lag_m:       Code/Products/1288-cube-lag_m-combined.csv,
##                  the global-minimum cell (lambda=5.427e-7,delta1=4.3,
##                  delta2=0.54) of the 27-point 3D cube -- already carries
##                  TS_hard as its own column.
##   - lag_2_cal_W: Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv,
##                  the lambda=5.427e-7 row of the 10-point lambdagrid --
##                  TS_hard computed here as 2*n*Lhat (same convention as
##                  1301-headline-pct-table.R), since this file predates the
##                  TS_hard column being added to grid_estimator's own output.
## d_g=9 (moment set A, 9 rows) throughout -- NOT 10 (that's the
## revenue-grid's d_g, moment set A's 9 rows plus the auxiliary R-moment).

library(tidyverse)
library(tinytable)
source("Code/Deconvolution/050-render-tbls.R")

DG <- 9
CHI2_95 <- qchisq(0.95, DG)

## --- lag_m: global minimum of the 3D cube ----------------------------------
cube <- read.csv("Code/Products/1288-cube-lag_m-combined.csv")
lm <- cube %>% filter(lambda == 5.427e-07, delta1 == 4.3, delta2 == 0.54)
stopifnot(nrow(lm) == 1)
lm_gamma <- lm %>% select(starts_with("gamma")) %>% unlist()

## --- lag_2_cal_W: lambda=5.427e-7 row of its own lambdagrid ----------------
lg <- read.csv("Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv")
w <- lg %>% filter(lambda == 5.427e-07)
stopifnot(nrow(w) == 1)
w_gamma <- w %>% select(starts_with("gamma")) %>% unlist()
w_TS_hard <- 2 * w$n * w$Lhat

fmt_sci <- function(x) sprintf("%.2e", x)
fmt3 <- function(x) sprintf("%.3f", x)

pass_lm <- lm$TS_hard < CHI2_95
pass_w  <- w_TS_hard < CHI2_95

STAT_ROWS <- c("$\\hat\\lambda$", "$\\hat\\delta_0$", "$\\hat\\delta_1$", "$\\hat\\delta_2$",
               "$\\hat\\omega^*=\\hat\\delta_1/2\\hat\\delta_2$", "$\\max\\vert\\gamma\\vert$",
               "$TS_{\\text{hard}}$", "$\\chi^2_{9,.95}$", "Result")
DELTA2_ROW <- which(STAT_ROWS == "$\\hat\\delta_2$")
RESULT_ROW <- which(STAT_ROWS == "Result")

tbl <- tibble(
    ` ` = STAT_ROWS,
    `$m^*_{t-1}$` = c(
        fmt_sci(lm$lambda), fmt3(lm$delta0_hat), fmt3(lm$delta1), fmt3(lm$delta2),
        fmt3(lm$delta1 / (2 * lm$delta2)), fmt3(max(abs(lm_gamma))),
        fmt3(lm$TS_hard), fmt3(CHI2_95), ifelse(pass_lm, "Passes", "Fails")
    ),
    `$\\tilde{\\mathcal{W}}_{t-2}$` = c(
        fmt_sci(w$lambda), fmt3(w$delta0_hat), fmt3(w$delta1_hat), fmt3(w$delta2_hat),
        fmt3(w$delta1_hat / (2 * w$delta2_hat)), fmt3(max(abs(w_gamma))),
        fmt3(w_TS_hard), fmt3(CHI2_95), ifelse(pass_w, "Passes", "Fails")
    )
)
write.csv(tbl, "Code/Products/1304-headline-estimates-table.csv", row.names = FALSE)
print(tbl)

## Bold via style_tt (a literal "**x**" in a cell is NOT parsed as markdown
## bold by tinytable's LaTeX backend -- it renders the literal asterisks,
## caught directly on first render). delta2 is bolded throughout (the
## coefficient repeatedly cross-validated across every grid/instrument in
## CLAUDE.md); "Passes"/"Fails" is bolded only in whichever column actually
## passes the hard test.
tt_obj <- tbl |> tt(notes = list(
    a = list(i = 0, j=2, text = "$m^*_{it-1}$'s point estimate refined from a joint 3D $(\\lambda,\\delta_1,\\delta_2)$ grid (27 cells)."),
    b = list(i = 0,j = 3, text = "$\\tilde{\\mathcal W}_{it-2}$'s point estimate from $\\lambda$-only grid search.")
    )) |>
    # style_tt(i = DELTA2_ROW, j = 2:3, bold = TRUE) |>
    style_tt(i = RESULT_ROW, j = if (pass_lm) 2 else 3, bold = TRUE) |>
    style_tt(i = 4, line = "b", line_width = 0.05) |>
    style_tt("notes", fontsize = 0.8)

render_png_tt_tbl(tt_obj, "1304-headline-estimates-table")
cat("Saved: Paper/tbls/1304-headline-estimates-table.png\n")
