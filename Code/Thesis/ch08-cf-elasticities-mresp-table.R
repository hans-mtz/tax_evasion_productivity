## PRODUCT: Thesis/tables/ch08-cf-elasticities-mresp.png := arc elasticities with respect to the purchases rate, true materials
## responding (headline), at Delta = +-1, 1.5, 2 percent: claimed deductions decomposed into mechanical (1), materials response and overreporting
## response; net sales-tax revenue relative to |R(0)| (the base scales the magnitude, the difference carries the sign) and relative
## to the mean sales tax on sales. ELVIS interior firms.
## Sources (mode=cfprofile, cf_mresp=1, R = 1000, theta fixed at 1616, cf_multi 3 starts, 2026-10-06):
##   1655-cf-diff_input-mr1-D<Delta>(-macbook).csv, 1655-cf-diff_evasion-mr1-D<Delta>.csv, 1655-cf-diff_revenue-mr1-D<Delta>(-oldmac).csv;
##   R(0) from 1653-cf-revenue-mr1-D0.csv; C(0) = mean t1/p_gdp/scale - R(0) (the sales-tax term has no latent part).
## Each elasticity = paired difference / (Delta x baseline), baseline treated as known (as in ch08-elasticities-table.R).
## Conservative 95% sets of the differences, scaled by the same factor. Formulas: appendix A @sec-app-cf-implementation.
source("Code/Thesis/001-setup.R")
rd <- function(f) { for (g in c(f, sub("\\.csv$", "-macbook.csv", f), sub("\\.csv$", "-oldmac.csv", f))) {
    p <- file.path(PRODUCTS_DIR, g); if (file.exists(p) && length(readLines(p)) > 1) return(read.csv(p)) }
    stop("missing: ", f) }
r0 <- rd("1653-cf-revenue-mr1-D0.csv"); R0 <- r0$T_hat; t1s <- r0$mean_t1p / r0$scale; C0 <- t1s - R0
stopifnot(r0$cf_target == "revenue", R0 < 0, abs(t1s - 0.789292) < 1e-4)
D <- c(-0.02, -0.015, -0.01, 0.01, 0.015, 0.02)
el <- function(x, base, d) { v <- c(x$T_hat, x$hard_lo, x$hard_hi) / (d * base); c(v[1], sort(v[2:3])) }
rows <- lapply(D, function(d) {
    i <- rd(sprintf("1655-cf-diff_input-mr1-D%s.csv", d)); v <- rd(sprintf("1655-cf-diff_evasion-mr1-D%s.csv", d))
    r <- rd(sprintf("1655-cf-diff_revenue-mr1-D%s.csv", d))
    stopifnot(i$cf_target == "diff_input", v$cf_target == "diff_evasion", r$cf_target == "diff_revenue",
              all(c(i$cf_mresp, v$cf_mresp, r$cf_mresp) == 1), all(is.finite(c(i$hard_lo, i$hard_hi, v$hard_lo, v$hard_hi, r$hard_lo, r$hard_hi))))
    ei <- el(i, C0, d); ev <- el(v, C0, d); ea <- el(r, abs(R0), d); et <- el(r, t1s, d)
    tibble(Delta = d, input = ei[1], input_lo = ei[2], input_hi = ei[3], evasion = ev[1], evasion_lo = ev[2], evasion_hi = ev[3],
           total = 1 + ei[1] + ev[1], revA = ea[1], revA_lo = ea[2], revA_hi = ea[3], revT = et[1], revT_lo = et[2], revT_hi = et[3])
})
d <- bind_rows(rows); print(as.data.frame(d))
write.csv(d, file.path(PRODUCTS_DIR, "ch08-cf-elasticities-mresp.csv"), row.names = FALSE)

pct <- function(x) paste0(ifelse(x < 0, "$-$", "+"), sprintf("%g\\%%", abs(100 * x)))
f2 <- function(v) ifelse(v < 0, paste0("$-$", sprintf("%.2f", -v)), sprintf("%.2f", v))
set <- function(lo, hi) paste0("[", f2(lo), ", ", f2(hi), "]")
tbl <- d %>% transmute(`$\\Delta$` = pct(Delta), `Total` = f2(total),
                       `Materials` = paste(f2(input), set(input_lo, input_hi)), `Overreporting` = paste(f2(evasion), set(evasion_lo, evasion_hi)),
                       `Base $|R_0|$` = paste(f2(revA), set(revA_lo, revA_hi)), `Base sales tax` = paste(f2(revT), set(revT_lo, revT_hi)))
print(tbl)
tt_obj <- tt(tbl, width = c(0.5, 0.5, 1.2, 1.2, 1.5, 1.5), notes = paste0(
    "Arc elasticities with respect to the purchases rate, true materials responding, ELVIS interior firms; conservative 95\\% sets ",
    "in brackets ($\\chi^2_{18,.95}=28.87$). Claimed deductions: $[C(\\Delta)-C(0)]/(\\Delta C(0))$ = 1 (mechanical) + materials response + ",
    "overreporting response; the total is the sum of the point estimates. Net sales-tax revenue: $[R(\\Delta)-R(0)]/(\\Delta|R(0)|)$, with ",
    "$R(0)=", sprintf("%.0f", R0 * r0$scale), "$ per firm-year, and $[R(\\Delta)-R(0)]/(\\Delta\\,\\overline{t1})$, relative to the mean sales tax on sales ",
    "(", formatC(r0$mean_t1p, format = "f", digits = 0, big.mark = ","), " per firm-year). Baselines treated as known.")) |>
    group_tt(j = list("Claimed deductions" = 2:4, "Net sales-tax revenue" = 5:6)) |>
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "ch08-cf-elasticities-mresp")
