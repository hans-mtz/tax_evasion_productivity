## PRODUCT: Thesis/tables/ch08-elasticities.png := elasticity of claimed purchase deductions with respect to tau_P,
## total (central difference at +-1%) and behavioural (by side, at Delta = +-1, +-1.5, +-2%), ELVIS interior firms.
## Sources (mode=cfprofile, theta fixed at the operating point 1616, gamma free, R = 1000, seed 30):
##   claims elasticity: Code/Products/1642-cf-elast-claims-cold.csv (cf_target=elast_claims, cf_cold=1; the warm-path
##     run 1638 had identical hard and soft lower bounds, a profile jump);
##   behavioural change: 1639-cf-diffbeh-small.csv (cf_target=diff_beh) except Delta = -2%, from the cold rerun
##     1640-cf-diffbeh-cold-m002.csv (the warm path failed there, TS_min 269);
##   baseline claims C(0): 1622-cf-i-k0.75-kappa0.5.csv, T_hat at Delta = 0.
## Behavioural elasticity = [C(Delta) - (1+Delta) C(0)] / (Delta C(0)), the behavioural part of the arc elasticity;
## its set divides the diff_beh set by Delta C_hat(0), treating C(0) as known. Scope (Hans, 2026-10-03): +-1, +-1.5,
## +-2% only.
source("Code/Thesis/001-setup.R")

rd <- function(f) read.csv(file.path(PRODUCTS_DIR, f))
lev <- rd("1622-cf-i-k0.75-kappa0.5.csv"); C0 <- lev$T_hat[lev$Delta == 0]
el <- rd("1642-cf-elast-claims-cold.csv")
db <- bind_rows(rd("1639-cf-diffbeh-small.csv") %>% filter(Delta != -0.02), rd("1640-cf-diffbeh-cold-m002.csv")) %>%   # bind_rows: files may differ in trailing columns (cf6 adds cf_target, gamma)
    arrange(Delta)
stopifnot(nrow(el) == 1, all(is.finite(c(el$hard_lo, el$hard_hi))), nrow(db) == 6, all(is.finite(c(db$hard_lo, db$hard_hi))),
          all(db$TS_min < db$crit))
## dividing by Delta < 0 swaps the bounds
beh <- db %>% mutate(est = T_hat / (Delta * C0), a = hard_lo / (Delta * C0), b = hard_hi / (Delta * C0),
                     lo = pmin(a, b), hi = pmax(a, b))
write.csv(beh %>% select(Delta, est, lo, hi, TS_min), file.path(PRODUCTS_DIR, "ch08-behavioural-elasticity.csv"), row.names = FALSE)

f3 <- function(v) formatC(v, format = "f", digits = 3)
f2 <- function(v) formatC(v, format = "f", digits = 2)
pct <- function(d) paste0(ifelse(d < 0, "$-$", "+"), sprintf("%g\\%%", abs(100 * d)))
tbl <- bind_rows(
    tibble(Elasticity = "Claims (total)", `$\\Delta$` = "$\\pm$1\\%", Estimate = f3(el$T_hat),
           `95\\% set` = paste0("[", f3(el$hard_lo), ", ", f3(el$hard_hi), "]"), `$TS_{\\min}$` = formatC(el$TS_min, format = "f", digits = 1)),
    tibble(Elasticity = "Behavioural", `$\\Delta$` = pct(beh$Delta), Estimate = f2(beh$est),
           `95\\% set` = paste0("[", f2(beh$lo), ", ", f2(beh$hi), "]"), `$TS_{\\min}$` = formatC(beh$TS_min, format = "f", digits = 1)))
tbl$Elasticity[3:nrow(tbl)] <- ""
print(tbl)

tt_obj <- tt(tbl, width = c(1.2, 0.6, 0.7, 1.1, 0.6), notes = paste0(
    "ELVIS interior firms, structural parameters fixed at the operating point. Claims (total): elasticity of mean claimed ",
    "deductions with respect to $\\tau_P$, central difference at $\\pm$1\\%. Behavioural: $[C(\\Delta)-(1+\\Delta)C(0)]/\\Delta C(0)$, ",
    "the part of the response that comes from firms changing their overreporting; its set treats $C(0)$ as known. ",
    "95\\% sets: conservative test, $TS\\le\\chi^2_{", el$d_g, ",.95}=", sprintf("%.2f", el$crit), "$.")) |>
    style_tt(i = 1, line = "b", line_width = 0.05) |>
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch08-elasticities")
cat("Saved: Thesis/tables/ch08-elasticities.{png,pdf}\n")
