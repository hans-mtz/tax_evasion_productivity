## PRODUCT: Thesis/tables/ch08-claims-level.png := counterfactual claimed purchase deductions by change Delta in the
## purchases rate (tau_P -> (1+Delta) tau_P): per firm-year for the ELVIS interior firms (estimate and 95% conservative
## set), totals for (A) evader industries (interior + trimmed top 0.5%, mechanical) and (B) the whole economy
## (A + corner firms, mechanical), and the behavioural share of the change in (A).
## Source: Code/Products/1621-cf-economy-i-k0.75-kappa0.5.csv (built on 1622-cf-i-k0.75-kappa0.5.csv, mode=cfprofile,
## cf_target=level, theta fixed at the operating point 1616, gamma free, R = 1000). Interim (Hans, 2026-10-03): all six
## Delta kept until the 1% grid (-10% to +10%) replaces this table.
## Units: real pesos (nominal / p_gdp); totals summed over firm-years.
source("Code/Thesis/001-setup.R")

e <- read.csv(file.path(PRODUCTS_DIR, "1621-cf-economy-i-k0.75-kappa0.5.csv"))
n_int <- 12050
stopifnot(abs(e$claimed_int[e$Delta == 0] / (e$T_hat[e$Delta == 0] * e$scale[1]) - n_int) < 1)

pct <- function(d) ifelse(d == 0, "0", paste0(ifelse(d < 0, "$-$", "+"), sprintf("%g\\%%", abs(100 * d))))
f0 <- function(v) formatC(v, format = "f", digits = 0, big.mark = ",")
fm <- function(v) formatC(v / 1e6, format = "f", digits = 1)
tbl <- e %>% arrange(Delta) %>% transmute(
    `$\\Delta$` = pct(Delta),
    `Estimate` = f0(T_hat * scale),
    `95\\% set` = paste0("[", f0(hard_lo * scale), ", ", f0(hard_hi * scale), "]"),
    `$TS_{\\min}$` = formatC(TS_min, format = "f", digits = 1),
    `(A)` = fm(claimed_A),
    `(B)` = fm(claimed_B),
    `Behavioural share` = ifelse(is.na(behavioural_share_of_d_claimed_A), "--",
                                 formatC(behavioural_share_of_d_claimed_A, format = "f", digits = 2)))
print(tbl)

crit <- e$crit[1]; dg <- e$d_g[1]
tt_obj <- tt(tbl, width = c(0.6, 0.8, 1.5, 0.6, 0.6, 0.6, 1.0), notes = paste0(
    "Real pesos. Estimate and 95\\% set: mean claims per firm-year, ELVIS interior firms (", f0(n_int),
    " firm-years); conservative test, $TS\\le\\chi^2_{", dg, ",.95}=", sprintf("%.2f", crit), "$. ",
    "(A) evader industries: interior firms plus the trimmed top 0.5\\% (claims scale with the rate); ",
    "(B) whole economy: (A) plus corner firms (claims scale with the rate); totals in millions over all firm-years. ",
    "Behavioural share: part of the change in (A) not explained by the rate change at the baseline claims.")) |>
    group_tt(j = list("Claims per firm-year" = 2:4, "Total claims" = 5:6)) |>
    style_tt(i = which(e$Delta[order(e$Delta)] == 0), background = "#f2f2f2") |>
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch08-claims-level")
cat("Saved: Thesis/tables/ch08-claims-level.{png,pdf}\n")
