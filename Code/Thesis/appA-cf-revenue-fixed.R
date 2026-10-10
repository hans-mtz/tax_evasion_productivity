## PRODUCT: Thesis/tables/appA-cf-revenue-fixed.png := robustness of the counterfactual net sales-tax revenue per firm-year
## (real pesos, ELVIS interior firms) to the response of true materials: true materials respond (headline, cf_mresp=1) and
## true materials fixed (cf_mresp=0, r = 1), estimate and conservative 95% set, at the Delta evaluated for both versions
## (the break-even bracket rows exist only for the headline and stay in tbl-cf-revenue). Hans 2026-10-10: the fixed-M
## version moved from tbl-cf-revenue to appendix A.
## Source: Code/Products/ch08-cf-revenue.csv (written by ch08-cf-revenue-bounds-plot.R from 1651/1652/1653).
## Break-even of each estimate by linear interpolation between adjacent Delta (as ch08-cf-revenue-bounds-plot.R).
source("Code/Thesis/001-setup.R")

d <- read.csv(file.path(PRODUCTS_DIR, "ch08-cf-revenue.csv")) %>% filter(!bracket)
pct <- function(x) ifelse(x == 0, "0", paste0(ifelse(x < 0, "$-$", "+"), sprintf("%g\\%%", abs(100 * x))))
f0 <- function(v) ifelse(v < 0, paste0("$-$", formatC(-v, format = "f", digits = 0, big.mark = ",")),
                         formatC(v, format = "f", digits = 0, big.mark = ","))
set <- function(lo, hi) paste0("[", f0(lo), ", ", f0(hi), "]")
be <- function(m) { m <- arrange(m, Delta); k <- which(diff(sign(m$rev)) != 0); stopifnot(length(k) == 1)
    m$Delta[k] - m$rev[k] * (m$Delta[k + 1] - m$Delta[k]) / (m$rev[k + 1] - m$rev[k]) }
w <- d %>% mutate(v = ifelse(version == "True materials respond", "mr", "fx")) %>%
    select(v, Delta, rev, lo, hi) %>% pivot_wider(names_from = v, values_from = c(rev, lo, hi)) %>% arrange(Delta)
stopifnot(!anyNA(w))
be_mr <- be(filter(d, version == "True materials respond")); be_fx <- be(filter(d, version == "True materials fixed"))
slope <- function(v) { a <- filter(d, version == v, Delta %in% c(-0.05, 0.05)) %>% arrange(Delta); a$rev[2] - a$rev[1] }
cat(sprintf("Break-even: respond %.4f, fixed %.4f. Change -5%% to +5%%: respond %.1f, fixed %.1f\n",
            be_mr, be_fx, slope("True materials respond"), slope("True materials fixed")))
cat(sprintf("Sets of the two versions overlap at every Delta: %s\n", all(w$lo_mr <= w$hi_fx & w$lo_fx <= w$hi_mr)))

tbl <- w %>% transmute(`$\\Delta$` = pct(Delta), `Estimate` = f0(rev_mr), `95\\% set` = set(lo_mr, hi_mr),
                       `Estimate ` = f0(rev_fx), `95\\% set ` = set(lo_fx, hi_fx))
print(tbl)
tt_obj <- tt(tbl, width = c(0.5, 0.7, 1.3, 0.7, 1.3), notes = paste0(
    "Real pesos per firm-year, ELVIS interior firms (12,050 firm-years). Net sales-tax revenue = gross sales-tax revenue minus ",
    "expected sales-tax refunds. 95\\% set: conservative test, $TS\\le\\chi^2_{18,.95}=", sprintf("%.2f", d$crit[1]), "$. ",
    "True materials respond: the materials first-order condition with the two tax rates, capital, labour and productivity fixed. ",
    "True materials fixed: true materials stay at their current level. Break-even of the estimate, interpolating linearly: ",
    sprintf("%.0f", abs(100 * be_mr)), "\\% cut with true materials responding, ", sprintf("%.0f", abs(100 * be_fx)),
    "\\% cut with true materials fixed.")) |>
    group_tt(j = list("True materials respond" = 2:3, "True materials fixed" = 4:5)) |>
    style_tt(i = which(w$Delta == 0), background = "#f2f2f2") |>
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "appA-cf-revenue-fixed")
