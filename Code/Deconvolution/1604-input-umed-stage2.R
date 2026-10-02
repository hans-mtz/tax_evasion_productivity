## PRODUCT: Code/Products/1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv := the 1598 input with `umed`
## replaced by the deconvolved medians on the stage-2 sample (1603, all 9 interior industries; Hans 2026-10-01: keep
## all industries, report 369 and 351 -- corporate eps sd above unincorporated V sd -- in the discussion).
suppressPackageStartupMessages(library(dplyr))
inp <- read.csv("Code/Products/1598-stage2-input-designA-interior-plant-k-trim0.005.csv", colClasses = c(sic_3 = "character"))
med <- read.csv("Code/Products/1603-np-deconv-stage2-summary-macbook.csv", colClasses = c(sic_3 = "character")) %>% select(sic_3, med_u)
out <- inp %>% select(-umed) %>% left_join(med, by = "sic_3") %>% rename(umed = med_u)
stopifnot(!anyNA(out$umed), nrow(out) == nrow(inp))
print(out %>% group_by(sic_3) %>% summarise(n = n(), umed = first(umed)) %>% as.data.frame())
f <- "Code/Products/1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv"
write.csv(out, f, row.names = FALSE, quote = FALSE); cat("Saved:", f, "\n")
