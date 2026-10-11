## Compare old test_data vs colombia_data_frame vs 931.1 df, industry 313
suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity")
load("Code/Products/test_data.RData")
load("Code/Products/colombia_data.RData")
e931 <- new.env(); load("Code/Products/931.1-fs-se-het.RData", envir = e931)
cdf <- colombia_data_frame %>% ungroup()
cat("rows: test_data", nrow(test_data), " colombia_data_frame", nrow(cdf), " 931.1 df", nrow(e931$df), "\n")
t3 <- test_data %>% ungroup() %>% filter(sic_3 == 313)
c3 <- cdf %>% filter(sic_3 == 313)
cat("313 rows: test_data", nrow(t3), " cdf", nrow(c3), "\n")
print(table(c3$juridical_organization, useNA = "a"))
## duplicates?
cat("dup plant-year in cdf 313:", sum(duplicated(c3[, c("plant","year")])), " test_data:", sum(duplicated(t3[,c("plant","year")])), "\n")
cat("dup plant-year in cdf all:", sum(duplicated(cdf[, c("plant","year")])), "\n")
## share definitions
j <- inner_join(t3 %>% select(plant, year, ls_t = log_mats_share, jo_t = juridical_organization),
                c3 %>% select(plant, year, ls_c = log_mats_share, jo_c = juridical_organization, nom_mats, nom_gross_output, materials, gross_output),
                by = c("plant","year"))
cat("joined", nrow(j), " max|ls_t-ls_c|", max(abs(j$ls_t - j$ls_c), na.rm=TRUE),
    " max|ls_c - log(nom_mats/nom_go)|", max(abs(j$ls_c - log(j$nom_mats/j$nom_gross_output)), na.rm=TRUE),
    " jo mismatch", sum(as.character(j$jo_t)!=as.character(j$jo_c), na.rm=TRUE), "\n")
