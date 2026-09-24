## Persistence of productivity: AR(1) coefficient gamma_1, corrected (single instruments) vs GNR --------
## Corrected: at the PF point estimates of 1472-pf-instrument-comparison.csv, rebuild
##   w_eps = cal_W - aK*k - aL*l  (= W~ = omega + (1-beta) eps)
## and run the same IV regression estimate_prod_fn_bounds() runs internally (021-deconv-funs.R):
##   w_eps ~ lag_w_eps | ins,   ins in {lag_m, lag_2_w_eps}; lags within plant ordered by year.
## The measurement error in lag_w_eps is handled by the instrument (independent of eps_{t-1}).
## GNR: firm-level omega (ε already removed) from Code/Products/stata-gnr-me-omg-<sic>.csv;
##   AR(1) OLS coefficient of omega_t on omega_{t-1} and the correlation corr(omega_t, omega_{t-1})
##   (the statistic GNR 2020 report in the text, p. 3006-3007).
## Plant-clustered SEs. Output: Code/Products/294-omega-persistence.csv
library(tidyverse)
load("Code/Products/931.1-fs-se-het.RData")   # fs_all_ls
five <- c("331", "322", "369", "313", "321")
pf <- read.csv("Code/Products/1472-pf-instrument-comparison.csv") %>%
    mutate(sic_3 = as.character(sic_3)) %>%
    filter(sic_3 %in% five, ins %in% c("lag_m", "lag_2_w_eps"))

corrected <- pmap_dfr(pf, function(sic_3, ins, alpha_K, alpha_L, ...) {
    d <- fs_all_ls[[sic_3]]$data %>% ungroup() %>%
        filter(is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m)) %>%
        group_by(plant) %>%
        mutate(w_eps = cal_W - alpha_K * k - alpha_L * l,
               lag_w_eps = lag(w_eps, order_by = year),
               lag_2_w_eps = lag(w_eps, 2, order_by = year),
               lag_m = lag(m, order_by = year)) %>%
        ungroup() %>% filter(is.finite(w_eps), is.finite(lag_w_eps), is.finite(.data[[ins]]))
    fit <- ivreg::ivreg(as.formula(paste("w_eps ~ lag_w_eps |", ins)), data = d)
    se <- sqrt(diag(sandwich::vcovCL(fit, cluster = ~plant)))[["lag_w_eps"]]
    tibble(sic_3, method = ins, gamma1 = coef(fit)[["lag_w_eps"]], se = se, corr = NA_real_, n = nrow(d))
})
gnr <- map_dfr(five, function(s) {
    g <- read.csv(sprintf("Code/Products/stata-gnr-me-omg-%s.csv", s)) %>%
        arrange(id, time) %>% group_by(id) %>%
        mutate(lag_om = lag(logomega)) %>%   # row-based within plant, as the corrected side and obj_fun_ivar1_bounds
        ungroup() %>% filter(is.finite(logomega), is.finite(lag_om))
    fit <- lm(logomega ~ lag_om, data = g)
    se <- sqrt(diag(sandwich::vcovCL(fit, cluster = ~id)))[["lag_om"]]
    tibble(sic_3 = s, method = "gnr", gamma1 = coef(fit)[["lag_om"]], se = se,
           corr = cor(g$logomega, g$lag_om), n = nrow(g))
})
out <- bind_rows(corrected, gnr) %>% arrange(factor(sic_3, five), method)
print(out, n = Inf)
write.csv(out, "Code/Products/294-omega-persistence.csv", row.names = FALSE)
