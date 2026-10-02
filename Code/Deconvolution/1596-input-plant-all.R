## PRODUCT: Code/Products/1596-stage2-input-designA-plant-trim0.005.csv := the full design-A stage-2 input (corner and
##          interior firms; 1572 = 1546 + sig2eps) plus `plant_id`, for the plant-clustered Omega with corner firms in
##          the sample (moment re-test ladder, 2026-09-30). Same row_id -> plant map and checks as 1592-input-plant.R.
suppressPackageStartupMessages(library(dplyr))
load("Code/Products/1532-stage2-data-final.RData")          # stage2_final, evaders
trim_top_pct <- 0.005
b <- stage2_final %>% filter(!corp) %>%
    mutate(use_c = evader,
           cal_V = if_else(use_c, cal_V_c, cal_V_u), tilde_cal_W = if_else(use_c, tilde_cal_W_c, tilde_cal_W_u),
           beta = if_else(use_c, beta_c, beta_u),
           corner = as.integer(sales_tax_rate_purchases == 0 | !use_c)) %>%
    filter(is.finite(cal_V), is.finite(tilde_cal_W), is.finite(beta), is.finite(sales_tax_rate_purchases),
           sales_tax_rate_purchases >= 0, is.finite(M_star), M_star > 0, is.finite(t1), is.finite(pgdp))
cut <- quantile(b$M_star[b$corner == 0], 1 - trim_top_pct)
b <- b %>% filter(corner == 1 | M_star <= cut) %>% mutate(row_id = row_number())
map <- b %>% transmute(row_id, plant = as.character(plant), M_star_chk = M_star, cal_V_chk = cal_V, sic_chk = sic_3, year_chk = year)
inp <- read.csv("Code/Products/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv", colClasses = c(sic_3 = "character"))
out <- inp %>% left_join(map, by = "row_id")
stopifnot(!anyNA(out$plant),
          isTRUE(all.equal(out$M_star, out$M_star_chk, tolerance = 1e-12)),
          isTRUE(all.equal(out$cal_V, out$cal_V_chk, tolerance = 1e-12)),
          all(out$sic_3 == out$sic_chk), all(out$year == out$year_chk))
out <- out %>% select(-ends_with("_chk")) %>% mutate(plant_id = as.integer(factor(plant))) %>% select(-plant)
cat(sprintf("rows %d (corner %d) | plants %d | firm-periods per plant: mean %.2f, max %d\n", nrow(out), sum(out$corner == 1),
            n_distinct(out$plant_id), nrow(out) / n_distinct(out$plant_id), max(table(out$plant_id))))
f <- "Code/Products/1596-stage2-input-designA-plant-trim0.005.csv"
write.csv(out, f, row.names = FALSE, quote = FALSE); cat("Saved:", f, "\n")
