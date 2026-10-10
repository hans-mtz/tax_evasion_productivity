## PRODUCT: Code/Products/ch08-cf-revenue-split.csv := back-of-envelope split of the change in mean net sales-tax revenue per
## firm-year (real pesos, ELVIS interior firms, true materials respond) at Delta = +-1, +-1.5, +-2%, by channel (Hans 2026-10-09:
## reported in the text as back-of-envelope estimates). Combines separately estimated point estimates, so it carries no set:
##   claims change dC = Delta C(0) x (1 + materials response + overreporting response)   [claims arc elasticities, 1655 diff_*]
##   gross change  dG = mean t1/p_gdp (r^beta - 1), r = [(1-(1+Delta) tau_P)/(1-tau_P)]^(-1/(1-beta))   [data alone, no draws]
##   revenue change dR = dG - dC; check: matches the directly estimated diff_revenue (Delta G(0) x share-of-gross elasticity).
## Shares of |dR|: mechanical (Delta C(0)), materials net of the gross-revenue change, overreporting; they add to 1.
## C(0) = G(0) - R(0), with R(0) and G(0) from 1653-cf-revenue-mr1-D0.csv.
## Sources: Code/Products/ch08-cf-elasticities-mresp.csv (ch08-cf-elasticities-mresp-table.R), 1653-cf-revenue-mr1-D0.csv,
## 1598-stage2-input-designA-interior-plant-k-trim0.005.csv.
source("Code/Thesis/001-setup.R")

r0 <- read.csv(file.path(PRODUCTS_DIR, "1653-cf-revenue-mr1-D0.csv"))
stopifnot(r0$cf_target == "revenue", r0$cf_mresp == 1)
G0 <- r0$mean_t1p; R0 <- r0$T_hat * r0$scale; C0 <- G0 - R0
e <- read.csv(file.path(PRODUCTS_DIR, "ch08-cf-elasticities-mresp.csv"))
inp <- read.csv(file.path(PRODUCTS_DIR, "1598-stage2-input-designA-interior-plant-k-trim0.005.csv"))
t1 <- inp$t1 / inp$pgdp; tau <- inp$sales_tax_rate_purchases; b <- inp$beta
cat(sprintf("G(0) %.1f  R(0) %.1f  C(0) %.1f real pesos per firm-year\n", G0, R0, C0))

d <- e %>% rowwise() %>% mutate(
    dG = { r <- ((1 - (1 + Delta) * tau) / (1 - tau))^(-1 / (1 - b)); mean(t1 * (r^b - 1)) }) %>% ungroup() %>%
    mutate(mech = Delta * C0, mat = input * Delta * C0, ev = evasion * Delta * C0, dC = mech + mat + ev,
           dR = dG - dC, dR_direct = revT * Delta * G0,
           share_mech = mech / -dR, share_mat_net = (mat - dG) / -dR, share_ev = ev / -dR) %>%
    select(Delta, dG, mech, mat, ev, dC, dR, dR_direct, share_mech, share_mat_net, share_ev)
stopifnot(abs(d$share_mech + d$share_mat_net + d$share_ev - 1) < 1e-12,
          abs(d$dR - d$dR_direct) < 0.5)   # identity check against the directly estimated revenue change
print(as.data.frame(d), digits = 3)
write.csv(d, file.path(PRODUCTS_DIR, "ch08-cf-revenue-split.csv"), row.names = FALSE)
cat("Saved: Code/Products/ch08-cf-revenue-split.csv\n")
