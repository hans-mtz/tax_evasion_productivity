## PRODUCT: Code/Products/<out>.csv := counterfactual claimed deductions and revenue, two exercises (Hans, 2026-10-02):
##   (A) evader industries: the ELVIS estimate for the interior firms (mode=cfprofile CSV: T_hat and its hard/soft
##       test-inversion bounds, T in units of `scale` = mean tau_P M* over interior firms), plus, mechanically, the
##       top-0.5% interior firms trimmed from stage 2 (their credits scale with the rate, no behavioural response);
##   (B) whole economy: (A) + all corner firms (non-evader industries with tau_P > 0, and tau_P = 0 firms, which add 0),
##       whose claimed deductions are deterministic, (1 + Delta) tau_P M* (e = 0, M = M*; no PF needed).
## 2026-10-06 (Hans): option mresp = 1 lets true materials respond to the purchases rate for the mechanical groups too
## (corner and trimmed firms: M = M*, e = 0, so claims (1+Delta) tau_P r M* and sales tax t1 r^beta, with
## r = [(1-(1+Delta) tau_P)/(1-tau_P)]^(-1/(1-beta)); appendix A @sec-app-cf-implementation); the cf CSVs must then be cf_mresp = 1 runs.
## cf_csv may be a comma-separated list (one file per Delta). cf_target = revenue CSVs give the interior revenue directly;
## the economy revenue adds the other groups' t1 r^beta - claims.
## Units: real pesos (M* = nominal materials / p_gdp; credits tau_P M*; sales taxes t1 / p_gdp). Totals over firm-years
## and per-year averages; changes relative to Delta = 0, with the interior response split into mechanical
## ((1+Delta) x baseline) and behavioural (the rest).
source("Code/Deconvolution/utils-cli.R")
defaults <- list(cf_csv = "Code/Products/1622-cf-smoke.csv", out = "Code/Products/1621-cf-economy-smoke.csv", trim = 0.005, mresp = 0)
opt <- parse_cli_args(defaults); log_run_header("1621-cf-economy.R", opt)
suppressPackageStartupMessages(library(dplyr))
load("Code/Products/1532-stage2-data-final.RData")   # stage2_final
i9 <- c("313", "321", "322", "324", "331", "342", "351", "352", "369")
b <- stage2_final %>% filter(!corp) %>%
    mutate(use_c = evader, cal_V = if_else(use_c, cal_V_c, cal_V_u), tilde_cal_W = if_else(use_c, tilde_cal_W_c, tilde_cal_W_u),
           beta = if_else(use_c, beta_c, beta_u), corner = as.integer(sales_tax_rate_purchases == 0 | !use_c)) %>%
    filter(is.finite(cal_V), is.finite(tilde_cal_W), is.finite(beta), is.finite(sales_tax_rate_purchases),
           sales_tax_rate_purchases >= 0, is.finite(M_star), M_star > 0, is.finite(t1), is.finite(pgdp))   # as 1532's export
cut <- if (opt$trim > 0) quantile(b$M_star[b$corner == 0], 1 - opt$trim) else Inf   # trim = 0: no trimmed group
b <- b %>% mutate(group = case_when(corner == 0 & M_star <= cut ~ "interior (ELVIS)",
                                    corner == 0 ~ "interior trimmed (mechanical)",
                                    as.character(sic_3) %in% i9 ~ "corner, evader industries",
                                    TRUE ~ "corner, other industries"),
                  credit0 = sales_tax_rate_purchases * M_star, t1r = t1 / pgdp)
n_years <- n_distinct(b$year)
base <- b %>% group_by(group) %>% summarise(n = n(), credit0 = sum(credit0), t1r = sum(t1r), .groups = "drop")
print(as.data.frame(base))
n_int <- base$n[base$group == "interior (ELVIS)"]
cf <- bind_rows(lapply(unlist(strsplit(opt$cf_csv, ",")), read.csv)) %>% arrange(Delta)   # parse_cli_args already splits commas
mresp <- as.integer(opt$mresp)
stopifnot("cf_mresp in the CSVs must match option mresp" = all((if ("cf_mresp" %in% names(cf)) cf$cf_mresp else 0) == mresp))
tg <- if ("cf_target" %in% names(cf)) unique(cf$cf_target) else "level"
stopifnot("one target per call: level (claims) or revenue" = length(tg) == 1 && tg %in% c("level", "revenue"))
stopifnot("hard bounds must be finite (an open or empty set needs explicit handling)" = all(is.finite(c(cf$hard_lo, cf$hard_hi))))
stopifnot(abs(cf$scale[1] - base$credit0[base$group == "interior (ELVIS)"] / n_int) < 1e-6 * cf$scale[1])   # same interior sample
rr <- function(D, tau, beta) if (mresp == 1) ((1 - (1 + D) * tau) / (1 - tau))^(-1 / (1 - beta)) else rep(1, length(tau))
mech <- function(g, D) { x <- b[b$group == g, ]; if (nrow(x) == 0) 0 else sum((1 + D) * x$credit0 * rr(D, x$sales_tax_rate_purchases, x$beta)) }
t1g <- function(gs, D) { x <- b[b$group %in% gs, ]; sum(x$t1r * rr(D, x$sales_tax_rate_purchases, x$beta)^x$beta) }
if (tg == "revenue") {   # interior revenue from the revenue target; the other groups are deterministic
    out <- cf %>% rowwise() %>% mutate(
        revenue_int = T_hat * scale * n_int, revenue_int_hard_lo = hard_lo * scale * n_int, revenue_int_hard_hi = hard_hi * scale * n_int,
        rev_trim = t1g("interior trimmed (mechanical)", Delta) - mech("interior trimmed (mechanical)", Delta),
        rev_corner = t1g(c("corner, evader industries", "corner, other industries"), Delta)
                     - mech("corner, evader industries", Delta) - mech("corner, other industries", Delta),
        revenue_A = revenue_int + rev_trim, revenue_A_hard_lo = revenue_int_hard_lo + rev_trim, revenue_A_hard_hi = revenue_int_hard_hi + rev_trim,
        revenue_B = revenue_A + rev_corner, revenue_B_hard_lo = revenue_A_hard_lo + rev_corner, revenue_B_hard_hi = revenue_A_hard_hi + rev_corner,
        per_year_revenue_B = revenue_B / n_years,
        # interim claims (Hans, 2026-10-06): interior claims = interior sales tax (t1 r^beta, no latent) - interior revenue; the set is
        # the revenue moment's (it differs slightly from the claims-level moment's); replaced by the cf_target=level runs after the JMP
        claims_int = t1g("interior (ELVIS)", Delta) - revenue_int, claims_int_hard_lo = t1g("interior (ELVIS)", Delta) - revenue_int_hard_hi,
        claims_int_hard_hi = t1g("interior (ELVIS)", Delta) - revenue_int_hard_lo,
        claims_A = claims_int + mech("interior trimmed (mechanical)", Delta),
        claims_B = claims_A + mech("corner, evader industries", Delta) + mech("corner, other industries", Delta)) %>% ungroup()
    print(as.data.frame(out %>% select(Delta, TS_min, revenue_int, revenue_int_hard_lo, revenue_int_hard_hi, revenue_A, revenue_B,
                                      revenue_B_hard_lo, revenue_B_hard_hi, claims_int, claims_A, claims_B) %>% mutate(across(where(is.numeric), ~ signif(.x, 5)))))
    write.csv(out, opt$out, row.names = FALSE); cat("Saved:", opt$out, "\n"); quit(save = "no")
}
c00 <- cf$T_hat[cf$Delta == 0] * cf$scale[1] * n_int
out <- cf %>% rowwise() %>% mutate(
    claimed_int = T_hat * scale * n_int, claimed_int_hard_lo = hard_lo * scale * n_int, claimed_int_hard_hi = hard_hi * scale * n_int,
    claimed_int_soft_lo = soft_lo * scale * n_int, claimed_int_soft_hi = soft_hi * scale * n_int,
    int_mechanical = (1 + Delta) * c00, int_behavioural = claimed_int - (1 + Delta) * c00,
    claimed_trim = mech("interior trimmed (mechanical)", Delta),
    claimed_A = claimed_int + claimed_trim,
    claimed_corner_i9 = mech("corner, evader industries", Delta), claimed_corner_other = mech("corner, other industries", Delta),
    claimed_B = claimed_A + claimed_corner_i9 + claimed_corner_other,
    t1_A = t1g(c("interior (ELVIS)", "interior trimmed (mechanical)"), Delta), t1_B = t1g(unique(b$group), Delta),
    revenue_A = t1_A - claimed_A, revenue_B = t1_B - claimed_B,
    share_A_in_B = claimed_A / claimed_B, per_year_claimed_B = claimed_B / n_years) %>% ungroup()
b0 <- out %>% filter(Delta == 0)
out <- out %>% mutate(d_claimed_A = claimed_A - b0$claimed_A, d_claimed_B = claimed_B - b0$claimed_B,
                      d_revenue_A = revenue_A - b0$revenue_A, d_revenue_B = revenue_B - b0$revenue_B,
                      behavioural_share_of_d_claimed_A = ifelse(Delta == 0, NA, (int_behavioural - b0$int_behavioural) / d_claimed_A))
print(as.data.frame(out %>% select(Delta, TS_min, claimed_int, claimed_int_hard_lo, claimed_int_hard_hi, claimed_A, claimed_B, share_A_in_B,
                                  d_claimed_A, d_claimed_B, behavioural_share_of_d_claimed_A, revenue_A, revenue_B) %>%
                    mutate(across(where(is.numeric), ~ signif(.x, 5)))))
write.csv(out, opt$out, row.names = FALSE); cat("Saved:", opt$out, "\n")
