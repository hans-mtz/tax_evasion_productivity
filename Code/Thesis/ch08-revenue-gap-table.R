## PRODUCT: Thesis/tables/ch08-revenue-gap.png := revenue lost to undetected overreporting among the ELVIS interior firms,
## at current rates: true credits E[tau_P M], potential revenue E[P] = E[t1/pgdp] - E[tau_P M], and the gap
## R/P - 1 = L/|P| (P < 0), with L = tau_P (1 - q(e)) e and R = P - L.
## Source: Code/Products/1644-cf-gap.csv (cf_target=gap, T = E[L]/E[P]) and 1644-cf-true-credit.csv (cf_target=true_credit,
## T = E[tau_P M]/scale); mode=cfprofile, theta fixed at the operating point 1616, gamma free, R = 1000, cf_cold=1.
## The E[P] set is the E[tau_P M] set shifted by the observed mean t1/pgdp. Since E[P] < 0 on its whole set, the
## reported gap is -T (= L/|P|), with the bounds swapped.
## Loss as a share of sales tax owed on sales (2026-10-04): 1645-cf-loss-t1-{int,A,B}.csv (cf_target=loss_t1, T = E[L]/E[t1/pgdp + x];
## (A) adds the trimmed firms' t1, (B) all other firms' t1, their L = 0, so lower bounds). Interior set = union of the cold
## (1645) and warm (1646b) runs: a failed gamma solve only raises TS, so every accepted value is in the set.
source("Code/Thesis/001-setup.R")

g <- read.csv(file.path(PRODUCTS_DIR, "1644-cf-gap.csv"))
tc <- read.csv(file.path(PRODUCTS_DIR, "1644-cf-true-credit.csv"))
sc <- tc$scale; t1p <- tc$mean_t1p
P <- c(est = t1p - tc$T_hat * sc, lo = t1p - tc$hard_hi * sc, hi = t1p - tc$hard_lo * sc)
stopifnot(P["hi"] < 0, all(is.finite(c(g$hard_lo, g$hard_hi))))
lt <- lapply(c(int = "int", A = "A", B = "B"), function(t) read.csv(file.path(PRODUCTS_DIR, sprintf("1645-cf-loss-t1-%s.csv", t))))
lw <- read.csv(file.path(PRODUCTS_DIR, "1646-cf-loss-t1-int-profile-warm.csv"))
lt$int$hard_lo <- min(lt$int$hard_lo, lw$hard_lo); lt$int$hard_hi <- max(lt$int$hard_hi, lw$hard_hi)
pc <- function(v) sprintf("%.1f\\%%", 100 * v)

f0 <- function(v) formatC(v, format = "f", digits = 0, big.mark = ",")
f3 <- function(v) formatC(v, format = "f", digits = 3)
neg <- function(s) sub("^-", "$-$", s)
set0 <- function(a, b) paste0("[", neg(f0(a)), ", ", neg(f0(b)), "]")
tbl <- tibble(
    ` ` = c("True credits, $E[\\tau_PM]$", "Potential revenue, $E[P]$", "Increase in net refunds, $L/\\vert P\\vert$",
            "Loss / sales tax on sales: interior firms", "\\quad (A) evader industries", "\\quad (B) all firms"),
    Estimate = c(f0(tc$T_hat * sc), neg(f0(P["est"])), f3(-g$T_hat), pc(lt$int$T_hat), pc(lt$A$T_hat), pc(lt$B$T_hat)),
    `95\\% set` = c(set0(tc$hard_lo * sc, tc$hard_hi * sc), set0(P["lo"], P["hi"]),
                    paste0("[", f3(-g$hard_hi), ", ", f3(-g$hard_lo), "]"),
                    sapply(lt, function(r) paste0("[", pc(r$hard_lo), ", ", pc(r$hard_hi), "]"))),
    `$TS_{\\min}$` = formatC(c(tc$TS_min, tc$TS_min, g$TS_min, lt$int$TS_min, lt$A$TS_min, lt$B$TS_min), format = "f", digits = 1))
print(tbl)

tt_obj <- tt(tbl, width = c(1.6, 0.6, 0.9, 0.5), notes = paste0(
    "ELVIS interior firms, real pesos per firm-year, current rates. $L=\\tau_P(1-q(e))e$; $P=t1/p_{gdp}-\\tau_PM$; $R=P-L$. ",
    "The $E[P]$ set follows from the $E[\\tau_PM]$ set because $t1$ is observed (mean ", f0(t1p), "). ",
    "Loss $L$ as a share of the sales tax owed on sales: (A) adds the trimmed firms' tax, (B) every firm's tax in the sample; their loss is set to zero, so (A) and (B) are lower bounds. The interior set is the union over two starting points of the $\\gamma$ solve. ",
    "Conservative test, $TS\\le\\chi^2_{", g$d_g, ",.95}=", sprintf("%.2f", g$crit), "$.")) |>
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch08-revenue-gap")
cat("Saved: Thesis/tables/ch08-revenue-gap.{png,pdf}\n")
