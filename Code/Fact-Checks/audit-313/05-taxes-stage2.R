suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity")
load("Code/Products/colombia_data.RData")
thr <- log(0.05)
d <- colombia_data_frame %>% ungroup() %>%
  filter(sic_3 == 313, is.finite(y), is.finite(k), is.finite(l), is.finite(m)) %>%
  mutate(jo = as.integer(as.character(juridical_organization)),
         grp = case_when(jo == 3 ~ "corp", jo %in% c(0,1,2,4,5) ~ "uninc", TRUE ~ paste0("code", jo)),
         s_gross = log(nom_mats / nom_gross_output)) %>% filter(is.finite(s_gross), s_gross > thr)
options(width = 220, pillar.sigfig = 3)
cat("== tau_S, tau_P quantiles by group x sic4 ==\n")
print(d %>% group_by(grp, sic_4) %>% summarise(n = n(), tauS_p10 = quantile(sales_tax_rate_sales, .1, na.rm=T), tauS_p50 = median(sales_tax_rate_sales, na.rm=T),
   tauS_p90 = quantile(sales_tax_rate_sales, .9, na.rm=T), tauS_max = max(sales_tax_rate_sales, na.rm=T),
   tauP_p50 = median(sales_tax_rate_purchases, na.rm=T), tauP_max = max(sales_tax_rate_purchases, na.rm=T), tauP0 = mean(sales_tax_rate_purchases == 0, na.rm=T), .groups="drop"), n = Inf)
cat("== tau_S median by year x group (3131/3132 vs corp) ==\n")
print(d %>% mutate(g2 = paste(grp, ifelse(sic_4 %in% c(3131,3132), "spirits/wine", ifelse(sic_4 == 3133, "beer", "soft")))) %>%
   group_by(year, g2) %>% summarise(tauS = median(sales_tax_rate_sales, na.rm=T), .groups="drop") %>% pivot_wider(names_from = g2, values_from = tauS), n = Inf)
## Net-share effect contributed by code 8 (why 1a moves mu by +0.05 with code 8 in)
d2 <- d %>% mutate(t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
                   s_net = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1))), dnet = s_net - s_gross)
print(d2 %>% group_by(grp) %>% summarise(n = n(), mean_dnet = mean(dnet, na.rm = TRUE)))

## Stage-2 sample (1532) vs test sample for 313
load("Code/Products/1532-stage2-data-final.RData")
s2 <- stage2_final %>% filter(sic_3 == 313)
cat("stage2_final 313 rows:", nrow(s2), " corp:", sum(s2$corp), " uninc:", sum(!s2$corp), "\n")
u2 <- s2 %>% filter(!corp)
cat("uninc: tau_P==0:", sum(u2$sales_tax_rate_purchases == 0, na.rm=TRUE), " cal_V finite:", sum(is.finite(u2$cal_V_c)),
    " tilde_cal_W_c finite:", sum(is.finite(u2$tilde_cal_W_c)), "\n")
inp <- read.csv("Code/Products/1598-stage2-input-designA-interior-plant-k-trim0.005.csv", colClasses = c(sic_3 = "character"))
i3 <- inp %>% filter(sic_3 == "313"); cat("ELVIS input 313:", nrow(i3), " mean V", mean(i3$cal_V), "\n")
fin <- u2 %>% filter(is.finite(cal_V_c))
cat("uninc with finite cal_V_c:", nrow(fin), " mean V", mean(fin$cal_V_c), "; with finite tilde W:", sum(is.finite(fin$tilde_cal_W_c)),
    " mean V among those without tilde W:", mean(fin$cal_V_c[!is.finite(fin$tilde_cal_W_c)]), "\n")
