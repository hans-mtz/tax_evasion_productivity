## PRODUCT: Thesis/tables/ch08-cf-revenue-totals.png := counterfactual net sales-tax revenue, sample totals (millions of real
## pesos, all firm-years 1981-1991), by change Delta in the purchases rate, true materials responding (headline):
## (A) evader industries = ELVIS interior firms + the trimmed top 0.5% mechanically; (B) whole economy = (A) + corner firms
## (non-evader industries and tau_P = 0), whose claims are (1+Delta) tau_P r M* and sales tax t1 r^beta.
## Source: Code/Products/1621-cf-economy-revenue-mr1.csv (1621-cf-economy.R mresp=1 on 1653 (Delta = 0) and 1651 revenue runs;
## Delta = 0 row added 2026-10-07, other rows reproduced exactly). The sets carry only the interior part's uncertainty;
## the mechanical groups are deterministic. Formulas: appendix A, @sec-app-cf-implementation.
source("Code/Thesis/001-setup.R")

d <- read.csv(file.path(PRODUCTS_DIR, "1621-cf-economy-revenue-mr1.csv")) %>% arrange(Delta)
stopifnot(all(d$cf_target == "revenue"), all(d$cf_mresp == 1), 0 %in% d$Delta,
          all(is.finite(c(d$revenue_A_hard_lo, d$revenue_A_hard_hi, d$revenue_B_hard_lo, d$revenue_B_hard_hi))))
print(d %>% select(Delta, revenue_A, revenue_A_hard_lo, revenue_A_hard_hi, revenue_B, revenue_B_hard_lo, revenue_B_hard_hi))
write.csv(d %>% select(Delta, revenue_A, revenue_A_hard_lo, revenue_A_hard_hi, revenue_B, revenue_B_hard_lo, revenue_B_hard_hi),
          file.path(PRODUCTS_DIR, "ch08-cf-revenue-totals.csv"), row.names = FALSE)

pct <- function(x) ifelse(x == 0, "0", paste0(ifelse(x < 0, "$-$", "+"), sprintf("%g\\%%", abs(100 * x))))
f1 <- function(v) ifelse(v < 0, paste0("$-$", sprintf("%.1f", -v / 1e6)), sprintf("%.1f", v / 1e6))
set <- function(lo, hi) paste0("[", f1(lo), ", ", f1(hi), "]")
tbl <- d %>% transmute(`$\\Delta$` = pct(Delta),
                       `Estimate` = f1(revenue_A), `95\\% set` = set(revenue_A_hard_lo, revenue_A_hard_hi),
                       `Estimate ` = f1(revenue_B), `95\\% set ` = set(revenue_B_hard_lo, revenue_B_hard_hi))
print(tbl)

tt_obj <- tt(tbl, width = c(0.5, 0.7, 1.3, 0.7, 1.3), notes = paste0(
    "Millions of real pesos, sum over all firm-years in the sample (1981--1991), true materials respond. ",
    "(A) evader industries: the ELVIS interior firms plus the 0.5\\% trimmed from estimation; ",
    "(B) all firms: (A) plus every other firm in the sample. Firms outside the ELVIS sample are treated as reporting their true materials, ",
    "so their claims and sales tax change only through the rate and the response of their materials. The 95\\% sets (conservative test, ",
    "$TS\\le\\chi^2_{18,.95}=", sprintf("%.2f", d$crit[1]), "$) come from the interior firms; the other firms add a known amount.")) |>
    group_tt(j = list("(A) Evader industries" = 2:3, "(B) All firms" = 4:5)) |>
    style_tt(i = which(d$Delta == 0), background = "#f2f2f2") |>
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch08-cf-revenue-totals")
cat("Saved: Thesis/tables/ch08-cf-revenue-totals.{png,pdf}\n")
