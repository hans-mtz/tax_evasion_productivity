## PRODUCT: Thesis/tables/ch08-revenue-gap.png := revenue lost to undetected overreporting at current rates, as a share of
## gross sales-tax revenue: interior firms and all firms in the sample (2026-10-10). Still computed below but not shown: true credits E[tau_P M], potential revenue E[P] = E[t1/pgdp] - E[tau_P M], and the gap
## R/P - 1 = L/|P| (P < 0), with L = tau_P (1 - q(e)) e and R = P - L.
## Source: Code/Products/1644-cf-gap.csv (cf_target=gap, T = E[L]/E[P]) and 1644-cf-true-credit.csv (cf_target=true_credit,
## T = E[tau_P M]/scale); mode=cfprofile, theta fixed at the operating point 1616, gamma free, R = 1000, cf_cold=1.
## The E[P] set is the E[tau_P M] set shifted by the observed mean t1/pgdp. Since E[P] < 0 on its whole set, the
## reported gap is -T (= L/|P|), with the bounds swapped.
## Loss as a share of sales tax owed on sales (2026-10-04): 1645-cf-loss-t1-{int,A,B}.csv (cf_target=loss_t1, T = E[L]/E[t1/pgdp + x];
## (A) adds the trimmed firms' t1, (B) all other firms' t1, their L = 0, so lower bounds). Interior set = union of the cold
## (1645) and warm (1646b) runs: a failed gamma solve only raises TS, so every accepted value is in the set.
## 2026-10-06: point estimates and sets from the audited-solver reruns 1653-cf-{gap,true-credit,loss-t1-*}.csv (cf_multi, 3 starts);
## each set is the union with the earlier runs' accepted values (1644/1645/1646b), which stay valid evidence.
source("Code/Thesis/001-setup.R")

un <- function(new, ...) { for (o in list(...)) { new$hard_lo <- min(new$hard_lo, o$hard_lo); new$hard_hi <- max(new$hard_hi, o$hard_hi) }; new }
rp <- function(f) read.csv(file.path(PRODUCTS_DIR, f))
g <- un(rp("1653-cf-gap.csv"), rp("1644-cf-gap.csv"))
tc <- un(rp("1653-cf-true-credit.csv"), rp("1644-cf-true-credit.csv"))
sc <- tc$scale; t1p <- tc$mean_t1p
P <- c(est = t1p - tc$T_hat * sc, lo = t1p - tc$hard_hi * sc, hi = t1p - tc$hard_lo * sc)
stopifnot(P["hi"] < 0, all(is.finite(c(g$hard_lo, g$hard_hi))))
lt <- lapply(c(int = "int", A = "A", B = "B"), function(t) un(rp(sprintf("1653-cf-loss-t1-%s.csv", t)), rp(sprintf("1645-cf-loss-t1-%s.csv", t))))
lt$int <- un(lt$int, rp("1646-cf-loss-t1-int-profile-warm.csv"))
pc <- function(v) sprintf("%.1f\\%%", 100 * v)

f0 <- function(v) formatC(v, format = "f", digits = 0, big.mark = ",")
f3 <- function(v) formatC(v, format = "f", digits = 3)
neg <- function(s) sub("^-", "$-$", s)
set0 <- function(a, b) paste0("[", neg(f0(a)), ", ", neg(f0(b)), "]")
# 2026-10-07 (Hans, D6): the table reports the cost of evasion as the loss L, in pesos and relative to the sales tax owed on
# sales (observed, positive base; the same base as the revenue elasticities). True credits E[tau_P M], potential revenue E[P]
# and L/|P| (negative latent base, poorly bounded) are still computed above but no longer shown.
stopifnot(abs(lt$int$mean_t1p - t1p) < 1e-6)   # interior run: no extra t1, so E[L] = T x mean t1/pgdp
# 2026-10-10 (Hans, outline 15): only the two shares the text reports; (A) and the loss level (72 real pesos, whose point does
# not add up with the separately estimated E[P] and E[R(0)]) are dropped.
tbl <- tibble(
    ` ` = c("Interior firms", "All firms in the sample"),
    Estimate = c(pc(lt$int$T_hat), pc(lt$B$T_hat)),
    `95\\% set` = sapply(lt[c("int", "B")], function(r) paste0("[", pc(r$hard_lo), ", ", pc(r$hard_hi), "]")))
print(tbl)

tt_obj <- tt(tbl, width = c(1.3, 0.6, 0.9), notes = paste0(
    "Current rates. Loss $L=\\tau_P(1-q(e))e$: the credit paid on undetected overreporting. Each share is the mean loss over ",
    "mean gross sales-tax revenue, estimated as an auxiliary parameter with its own moment. ",
    "Interior firms: mean gross sales-tax revenue ", f0(t1p), " real pesos per firm-year. ",
    "All firms in the sample: every other firm's gross sales-tax revenue is added with a loss of zero, which makes the share a lower bound. ",
    "Each set is the union of the values accepted over several solver runs. ",
    "Conservative test, $TS\\le\\chi^2_{", g$d_g, ",.95}=", sprintf("%.2f", g$crit), "$.")) |>
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch08-revenue-gap")
cat("Saved: Thesis/tables/ch08-revenue-gap.{png,pdf}\n")
