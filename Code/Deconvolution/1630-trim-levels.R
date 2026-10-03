## PRODUCT: Code/Products/1630-stage2-input-designA-interior-plant-k-trim<p>.csv for p in {0.001, 0.002, 0.003, 0.004}
##   := the untrimmed design i input (1624) with the top p of interior firm-years by M* removed (Hans, 2026-10-03: how
##   much trimming gets the best point back under the critical value). Also prints, per level: firm-years removed by
##   industry, their mean V, and their share of interior claimed credits (tau_P M*).
suppressPackageStartupMessages(library(dplyr))
a <- read.csv("Code/Products/1624-stage2-input-designA-interior-plant-k-notrim.csv", colClasses = c(sic_3 = "character"))
tot <- sum(a$sales_tax_rate_purchases * a$M_star)
for (p in c(0.001, 0.002, 0.003, 0.004, 0.005)) {
    cut <- quantile(a$M_star, 1 - p); rm <- a %>% filter(M_star > cut); kp <- a %>% filter(M_star <= cut)
    by <- rm %>% group_by(sic_3) %>% summarise(n = n(), meanV = round(mean(cal_V), 3), .groups = "drop")
    cat(sprintf("\ntrim %.1f%%: removes %d firm-years (credit share %.1f%%) | 321: %d | by industry: %s\n", 100 * p, nrow(rm),
                100 * sum(rm$sales_tax_rate_purchases * rm$M_star) / tot, sum(rm$sic_3 == "321"),
                paste0(by$sic_3, ":", by$n, " (V ", by$meanV, ")", collapse = ", ")))
    cat(sprintf("  remaining 321 mean V %.3f (untrimmed %.3f)\n", mean(kp$cal_V[kp$sic_3 == "321"]), mean(a$cal_V[a$sic_3 == "321"])))
    if (p < 0.005) { f <- sprintf("Code/Products/1630-stage2-input-designA-interior-plant-k-trim%g.csv", p)
        write.csv(kp %>% mutate(plant_id = as.integer(factor(plant_id))), f, row.names = FALSE, quote = FALSE); cat("  Saved:", f, "\n") }
}
