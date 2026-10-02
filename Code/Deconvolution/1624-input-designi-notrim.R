## PRODUCT: Code/Products/1624-stage2-input-designA-interior-plant-k-notrim.csv := the design i interior input (as 1598,
##   the columns design i uses) WITHOUT the top-0.5% M* trim (Hans, 2026-10-02: the trim was set when q was linear in
##   levels; the 61 trimmed firm-years carry 12.9% of interior claimed credits). Same filters as 1532's export otherwise;
##   ltau_bar = ln(own tau_P) (headline definition, 1546), sig2eps by industry (as in 1598), k from stage2_final,
##   plant_id = dense index of plant. Check: the 12,050 rows shared with 1598 match it column by column.
suppressPackageStartupMessages(library(dplyr))
load("Code/Products/1532-stage2-data-final.RData")
b <- stage2_final %>% filter(!corp) %>%
    mutate(use_c = evader, cal_V = if_else(use_c, cal_V_c, cal_V_u), tilde_cal_W = if_else(use_c, tilde_cal_W_c, tilde_cal_W_u),
           beta = if_else(use_c, beta_c, beta_u), corner = as.integer(sales_tax_rate_purchases == 0 | !use_c)) %>%
    filter(is.finite(cal_V), is.finite(tilde_cal_W), is.finite(beta), is.finite(sales_tax_rate_purchases),
           sales_tax_rate_purchases >= 0, is.finite(M_star), M_star > 0, is.finite(t1), is.finite(pgdp)) %>%
    filter(corner == 0) %>% mutate(row_id = row_number())
old <- read.csv("Code/Products/1598-stage2-input-designA-interior-plant-k-trim0.005.csv", colClasses = c(sic_3 = "character"))
s2 <- old %>% distinct(sic_3, sig2eps)
out <- b %>% transmute(M_star, cal_V, tilde_cal_W, sales_tax_rate_purchases, beta, corner, row_id, t1, pgdp, Mbar, year,
                       sic_3 = as.character(sic_3), Mbar_loo = as.integer(Mbar_loo), ltau_bar = log(sales_tax_rate_purchases),
                       plant = as.character(plant), k) %>%
    left_join(s2, by = "sic_3") %>% mutate(plant_id = as.integer(factor(plant))) %>%
    select(M_star, cal_V, tilde_cal_W, sales_tax_rate_purchases, beta, corner, row_id, t1, pgdp, Mbar, year, sic_3, Mbar_loo, ltau_bar, sig2eps, plant_id, k)
stopifnot(!anyNA(out), all(is.finite(as.matrix(out %>% select(-sic_3)))))
## check against 1598 on the shared rows (match on M_star, cal_V, year, sic_3)
key <- c("M_star", "cal_V", "year", "sic_3")
m <- inner_join(old %>% mutate(across(c(M_star, cal_V), ~ signif(.x, 12))), out %>% mutate(across(c(M_star, cal_V), ~ signif(.x, 12))), by = key, suffix = c(".o", ".n"))
cat(sprintf("untrimmed interior n = %d (1598: %d) | matched rows %d\n", nrow(out), nrow(old), nrow(m)))
for (v in c("tilde_cal_W", "sales_tax_rate_purchases", "beta", "t1", "pgdp", "Mbar", "ltau_bar", "sig2eps", "k"))
    cat(sprintf("  %-26s max abs diff %.3g\n", v, max(abs(m[[paste0(v, ".o")]] - m[[paste0(v, ".n")]]))))
f <- "Code/Products/1624-stage2-input-designA-interior-plant-k-notrim.csv"
write.csv(out, f, row.names = FALSE, quote = FALSE); cat("Saved:", f, "\n")
