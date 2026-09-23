## PRODUCT: Thesis/tables/ch08-detection-prob.png := Mean/Median/P95/P99/Max of e and the
## implied q(e), by instrument.
## Ported from Code/Deconvolution/1303-detection-prob-table.R (that script still feeds the
## old Paper/tbls/ pipeline for slides -- left untouched). Same fix as
## ch08-headline-pct-table.R: the old Thesis/tables/ copy predates the DPI-tagging fix.
source("Code/Thesis/001-setup.R")

INS_LABELS_ASCII <- c(lag_m = "m*(t-1)", lag_2_cal_W = "W(t-2)")
LAMBDA <- 5.427e-7

stats_for <- function(ins) {
    d <- read.csv(file.path(PRODUCTS_DIR, sprintf("1302-omega-e-percentiles-%s.csv", ins)))
    e <- d$e_mean
    tibble(
        Stat = c("Mean", "Median", "P95", "P99", "Max"),
        e = c(mean(e), median(e), quantile(e, 0.95, names = FALSE),
              quantile(e, 0.99, names = FALSE), max(e))
    ) %>% mutate(q = LAMBDA * e, rel = e / e[Stat == "Median"])
}

lm <- stats_for("lag_m")
w  <- stats_for("lag_2_cal_W")
stopifnot(identical(lm$Stat, w$Stat))

tbl <- tibble(
    Stat = lm$Stat,
    e1 = format(round(lm$e), big.mark = ","),
    q1 = sprintf("%.2f%%", 100 * lm$q),
    rel1 = sprintf("%.0f$\\times$", lm$rel),
    e2 = format(round(w$e), big.mark = ","),
    q2 = sprintf("%.2f%%", 100 * w$q),
    rel2 = sprintf("%.0f$\\times$", w$rel)
)
print(tbl)

tbl_tex <- tbl %>% mutate(across(everything(), ~ gsub("%", "\\\\%", .x)))

tt_obj <- tt(tbl_tex, align = "lcccccc", width = 1,
             notes = "Implied detection probability $q(e)=\\hat\\lambda\\cdot e$ across the evasion distribution, using forward-simulation estimates. Because $q$ is linear in $e$, the $q(e)/q(\\text{median})=e/\\text{median}$.")
colnames(tt_obj) <- c(" ", "$e$", "$q(e)$", "$q(e)/q(\\text{Med})$", "$e$", "$q(e)$", "$q(e)/q(\\text{Med})$")
tt_obj <- tt_obj |> group_tt(j = list(
    "$m^*_{t-1}$"  = 2:4,
    "$\\tilde{\\mathcal{W}}_{t-2}$"   = 5:7
    )) |>
    style_tt(i = "notes", fontsize = 0.8) |>
    format_tt(replace = list(" "= "1$\\times$"))

render_thesis_table(tt_obj, "ch08-detection-prob")
cat("Saved: Thesis/tables/ch08-detection-prob.{png,pdf}\n")
