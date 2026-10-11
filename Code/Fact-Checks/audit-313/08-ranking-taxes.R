## Audit 313 (2026-10-11): (1) industry ranking by real sales under the current rule (codes 0-5) vs all codes;
## (2) taxes paid by juridical code (corporations, unincorporated 0-2/4-5, and the dropped codes 6-9), 313 and all
## manufacturing. Sample as ch03-sample.R but keeping codes 6-9: finite y, k, l, m; 353 dropped.
## Real amounts (1981-91 sums, real pesos as stored in colombia_data_frame): sales, t1 = sales tax on sales (sales_tax_sales),
## t2 = sales tax paid on purchases (sales_tax_purchases), t3 = consumption taxes (consumption_taxes_on_sales).
## Codes (Paper/Tax-Prod.qmd l. 102-104; Paper/sections/01-notes.qmd): 6 stock partnership, 7 cooperative,
## 8 official entity (state industrial and commercial enterprise), 9 religious community / other.
suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity"); OUT <- "Code/Fact-Checks/audit-313"
load("Code/Products/colombia_data.RData")
options(width = 220, pillar.sigfig = 3, dplyr.summarise.inform = FALSE)

d <- colombia_data_frame %>% ungroup() %>%
  filter(is.finite(y), is.finite(k), is.finite(l), is.finite(m), sic_3 != 353) %>%
  mutate(jo = as.integer(as.character(juridical_organization)),
         grp = case_when(jo == 3 ~ "3 corp", jo %in% c(0, 1, 2, 4, 5) ~ "0-5 uninc", TRUE ~ paste0(jo, " dropped")),
         sic_3 = as.character(sic_3))

## (1) Rankings. The 20 tested industries (top_20_inds, Code/Colombia/15_global_vars.R l. 314-356) are the top 20 by
## number of firm-years (n_sic), all codes 0-9, finite y, k, l, m (353 included there). Here: firm-years and real sales
## summed 1981-91, under the current rule (codes 0-5) and with all codes.
rank_by <- function(x, lab) x %>% group_by(sic_3) %>% summarise(fy = n(), sales = sum(sales, na.rm = TRUE)) %>%
  mutate(share = 100 * sales / sum(sales), rank_fy = rank(-fy, ties.method = "min"), rank_sales = rank(-sales)) %>%
  rename_with(~ paste0(.x, "_", lab), -sic_3)
tested <- c("311","312","313","321","322","323","324","331","332","341","342","351","352","356","369","381","382","383","384","390")
rk <- full_join(rank_by(filter(d, jo %in% 0:5), "rule"), rank_by(d, "all"), by = "sic_3") %>%
  mutate(tested = sic_3 %in% tested) %>% arrange(rank_fy_rule)
cat("== Industry rankings (firm-years, real sales), current rule (codes 0-5) vs all codes ==\n"); print(rk, n = Inf)
cat("Top 20 by firm-years under the current rule == tested set?", setequal(rk$sic_3[rk$rank_fy_rule <= 20], tested), "\n")
write.csv(rk, file.path(OUT, "08-ranking.csv"), row.names = FALSE)

## (2) Taxes by juridical group
tax_tab <- function(x) x %>% group_by(grp) %>%
  summarise(firm_years = n(), plants = n_distinct(plant),
            sales = sum(sales, na.rm = TRUE), t1 = sum(sales_tax_sales, na.rm = TRUE),
            t2 = sum(sales_tax_purchases, na.rm = TRUE), t3 = sum(consumption_taxes_on_sales, na.rm = TRUE),
            pct_t1_pos = 100 * mean(sales_tax_sales > 0, na.rm = TRUE),
            pct_t3_pos = 100 * mean(consumption_taxes_on_sales > 0, na.rm = TRUE),
            tauS_med = median(sales_tax_rate_sales, na.rm = TRUE), tauP_med = median(sales_tax_rate_purchases, na.rm = TRUE)) %>%
  mutate(net_st = t1 - t2, t1_over_sales = 100 * t1 / sales, t3_over_sales = 100 * t3 / sales,
         pct_sales = 100 * sales / sum(sales), pct_t1 = 100 * t1 / sum(t1), pct_net_st = 100 * net_st / sum(net_st),
         pct_t3 = 100 * t3 / sum(t3))
cat("\n== Taxes by juridical group, industry 313 ==\n"); t313 <- tax_tab(filter(d, sic_3 == "313")); print(t313)
cat("\n== Taxes by juridical group, all manufacturing ==\n"); tall <- tax_tab(d); print(tall)
cat("\n== Dropped codes by industry (firm-years; share of industry sales) ==\n")
byind <- d %>% group_by(sic_3) %>% mutate(ind_sales = sum(sales, na.rm = TRUE)) %>% filter(jo >= 6) %>%
  group_by(sic_3, jo) %>% summarise(firm_years = n(), pct_ind_sales = 100 * sum(sales, na.rm = TRUE) / first(ind_sales)) %>%
  pivot_wider(names_from = jo, values_from = c(firm_years, pct_ind_sales), values_fill = 0)
print(byind, n = Inf)
cat("\n== 313 code 8 by 4-digit and year: firm-years, t1/sales, t3/sales ==\n")
print(d %>% filter(sic_3 == "313", jo == 8) %>% group_by(sic_4) %>%
        summarise(n = n(), plants = n_distinct(plant), t1_sales = 100 * sum(sales_tax_sales) / sum(sales),
                  t3_sales = 100 * sum(consumption_taxes_on_sales) / sum(sales), t2_mats = 100 * sum(sales_tax_purchases) / sum(materials)))
write.csv(bind_rows(mutate(t313, scope = "313"), mutate(tall, scope = "all")), file.path(OUT, "08-taxes-by-jo.csv"), row.names = FALSE)
