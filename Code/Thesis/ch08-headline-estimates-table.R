## PRODUCT: Thesis/tables/ch08-headline-estimates.png := headline stage-2 estimates,
## 3 columns (Stat, m*(t-1), W(t-2)).
## Ported from Code/Deconvolution/1304-headline-estimates-table.R (that script still feeds
## the old Paper/tbls/ pipeline for slides -- left untouched). Same fix as the other two
## ch08 tables: the old Thesis/tables/ copy predates the DPI-tagging fix.
source("Code/Thesis/001-setup.R")

DG <- 9
CHI2_95 <- qchisq(0.95, DG)

## --- lag_m: global minimum of the 3D cube ----------------------------------
cube <- read.csv(file.path(PRODUCTS_DIR, "1288-cube-lag_m-combined.csv"))
lm <- cube %>% filter(lambda == 5.427e-07, delta1 == 4.3, delta2 == 0.54)
stopifnot(nrow(lm) == 1)
lm_gamma <- lm %>% select(starts_with("gamma")) %>% unlist()

## --- lag_2_cal_W: lambda=5.427e-7 row of its own lambdagrid ----------------
lg <- read.csv(file.path(PRODUCTS_DIR, "1273-lambdagrid-noeta-lag_2_cal_W.csv"))
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
               "$TS_{\\text{cons}}$", "$\\chi^2_{9,.95}$", "Result")
RESULT_ROW <- which(STAT_ROWS == "Result")

tbl <- tibble(
    ` ` = STAT_ROWS,
    `$m^*_{it-1}$` = c(
        fmt_sci(lm$lambda), fmt3(lm$delta0_hat), fmt3(lm$delta1), fmt3(lm$delta2),
        fmt3(lm$delta1 / (2 * lm$delta2)), fmt3(max(abs(lm_gamma))),
        fmt3(lm$TS_hard), fmt3(CHI2_95), ifelse(pass_lm, "Passes", "Fails")
    ),
    `$\\tilde{\\mathcal{W}}_{it-2}$` = c(
        fmt_sci(w$lambda), fmt3(w$delta0_hat), fmt3(w$delta1_hat), fmt3(w$delta2_hat),
        fmt3(w$delta1_hat / (2 * w$delta2_hat)), fmt3(max(abs(w_gamma))),
        fmt3(w_TS_hard), fmt3(CHI2_95), ifelse(pass_w, "Passes", "Fails")
    )
)
print(tbl)

## width=1, no caption= (project defaults). Notes list preserved as-is.
tt_obj <- tbl |> tt(width = 1, notes = list(
    a = list(i = 0, j=2, text = "$m^*_{it-1}$'s point estimate refined from a joint 3D $(\\lambda,\\delta_1,\\delta_2)$ grid (27 cells)."),
    b = list(i = 0,j = 3, text = "$\\tilde{\\mathcal W}_{it-2}$'s point estimate from $\\lambda$-only grid search.")
    )) |>
    style_tt(i = RESULT_ROW, j = if (pass_lm) 2 else 3, bold = TRUE) |>
    style_tt(i = 4, line = "b", line_width = 0.05) |>
    style_tt("notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch08-headline-estimates")
cat("Saved: Thesis/tables/ch08-headline-estimates.{png,pdf}\n")
