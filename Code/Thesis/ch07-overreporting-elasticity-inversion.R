## 2026-10-01: test-inversion confidence set for the MIDPOINT (arc) elasticity of overreporting w.r.t. the purchases-side rate tau_P.
## PRODUCT: Code/Products/ch07-overreporting-elasticity-inversion.csv (+ console output); point estimates in ch07-overreporting-elasticity.R.
## Under the null, ln D_j (corporations' mean log net share, industry j, held fixed as in 1510), mu_83, mu_87 and the mean rates
## T_83, T_87 are the truth; the question is whether the arc elasticity equals E0. Only E0 is gridded.
## Means (each pinned by its own moment, just identified, so the weight matrix does not move the estimates):
##   mu_t = mean_{uninc, liable, t}(s - lnD_j),  T_t = mean_{uninc, liable, t}(tau_P),  r_t = exp(mu_t) - 1.
## Midpoint elasticity: E = [(r1 - r0)/rbar] / [(T1 - T0)/Tbar],  rbar = (r0 + r1)/2,  Tbar = (T0 + T1)/2  (0 = 1983, 1 = post year).
## Null written without division:  theta(E0; m) = (r1 - r0) - E0 * D * rbar = 0,  D = (T1 - T0)/Tbar,  m = (mu0, mu1, T0, T1).
## Statistic: TS(E0) = theta^2 / (grad' V grad) ~ chi2_1; V = Omega_h / n, Omega_h = n^-1 sum_p S_p S_p', S_p = sum_{i in p}(h_i - hbar),
## h_i the influence scores of the four means (plant-clustered, centred, divisor n: the convention of ELVIS cluster=plant, 1510, 1512).
## The gradient depends on E0, so the variance is re-evaluated at every candidate. Set = {E0 : TS <= chi2_1(.95)}.
## Bound: u >= 0 means r >= 0, which caps E at 2/D (E0 * D <= 2 for r0, r1 >= 0); a set reaching the cap is reported open there.
## Sample: codes 6-9 dropped; liable industries (every industry except 311, 312); net share > 5%; 0 < tau_P < 50%; as ch. 7 / ch. 3.
library(tidyverse)
PRODUCTS_DIR <- "Code/Products"
load(file.path(PRODUCTS_DIR, "921-DD2.RData"))   # wip_df
threshold_cut <- 0.05; BASE <- "83"; POST <- c("84", "85", "86", "87", "88")
grid <- seq(-10, 60, by = 0.01); crit <- qchisq(0.95, 1)

d <- wip_df %>% filter(!juridical_organization %in% 6:9) %>%
    mutate(s = suppressWarnings(log((nom_mats * (1 - sales_tax_rate_purchases)) / (nom_gross_output - sales_tax_rate_sales * nom_sales))),
           in_reg = is.finite(s) & s > log(threshold_cut), yr = as.character(year),
           tau = sales_tax_rate_purchases,
           in_tau = is.finite(tau) & tau > 0 & tau < 0.5)
lnD <- d %>% filter(corp == "Corp", in_reg, exempt_ind == "Taxed") %>% group_by(sic_3) %>% summarise(lnD = mean(s), .groups = "drop")
u <- d %>% filter(corp == "Other", exempt_ind == "Taxed", yr %in% c(BASE, POST)) %>% inner_join(lnD, by = "sic_3") %>%
    mutate(v = s - lnD)

invert <- function(post) {
    x <- u %>% filter(yr %in% c(BASE, post), in_reg | in_tau); n <- nrow(x)
    c0 <- x$yr == BASE & x$in_reg; c1 <- x$yr == post & x$in_reg; t0 <- x$yr == BASE & x$in_tau; t1 <- x$yr == post & x$in_tau
    mu0 <- mean(x$v[c0]); mu1 <- mean(x$v[c1]); T0 <- mean(x$tau[t0]); T1 <- mean(x$tau[t1])
    ## influence scores of the four means (each mean = sum over its cell / cell size; h_i scaled by n / cell size)
    H <- cbind(ifelse(c0, (x$v - mu0), 0) * n / sum(c0), ifelse(c1, (x$v - mu1), 0) * n / sum(c1),
               ifelse(t0, (x$tau - T0), 0) * n / sum(t0), ifelse(t1, (x$tau - T1), 0) * n / sum(t1))
    S <- rowsum(H, x$plant); S <- S - outer(as.vector(table(x$plant)[rownames(S)]), colMeans(H))
    Vm <- crossprod(S) / n / n                                   # Omega_h / n
    r0 <- exp(mu0) - 1; r1 <- exp(mu1) - 1; rb <- (r0 + r1) / 2; Tb <- (T0 + T1) / 2; D <- (T1 - T0) / Tb
    Ehat <- ((r1 - r0) / rb) / D
    th <- function(E0) (r1 - r0) - E0 * D * rb
    gr <- function(E0) c(-(1 + E0 * D / 2) * exp(mu0), (1 - E0 * D / 2) * exp(mu1),
                         -E0 * rb * (-T1 / Tb^2), -E0 * rb * (T0 / Tb^2))
    TS <- sapply(grid, function(E0) { g <- gr(E0); th(E0)^2 / drop(t(g) %*% Vm %*% g) })
    ## delta-method SE of the point estimate (gradient of E itself): E = th-free ratio, use the E0 = Ehat gradient / (D * rb)
    gE <- gr(Ehat) / (D * rb); seE <- sqrt(drop(t(gE) %*% Vm %*% gE))
    ok <- grid[TS <= crit]
    tibble(post = post, n = n, plants = length(unique(x$plant)), mu0 = mu0, mu1 = mu1, r0 = r0, r1 = r1, T0 = T0, T1 = T1,
           D = D, cap = 2 / D, E_mid = Ehat, se_delta = seE,
           ci_lo = if (length(ok)) min(ok) else NA, ci_hi = if (length(ok)) max(ok) else NA,
           contiguous = length(ok) == 0 || all(diff(ok) < 1.5 * (grid[2] - grid[1])),
           hits_cap = length(ok) > 0 && max(ok) >= min(2 / D, max(grid)) - 0.02, TS_at_hat = TS[which.min(abs(grid - Ehat))],
           p_E_le_1 = 1 - pchisq(TS[which.min(abs(grid - 1))], 1))
}
res <- map_dfr(POST, invert)
options(width = 220, digits = 4); print(res %>% select(-n, -plants), n = Inf, width = Inf)
write.csv(res, file.path(PRODUCTS_DIR, "ch07-overreporting-elasticity-inversion.csv"), row.names = FALSE)
cat("Saved: Code/Products/ch07-overreporting-elasticity-inversion.csv\n")
