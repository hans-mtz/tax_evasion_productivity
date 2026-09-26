## Check for the detection-function note (Paper/sections/9999-detection-q.qmd): build the lagged industry
## reference scale Mbar_{j,t-1} (mean and median reported M* over all firms, corporations included) and
## measure how often the support restriction e < Mbar_{j,t-1} can bind for unincorporated evaders.
## Read-only: prints results, saves nothing.
suppressMessages(library(dplyr))
load("Code/Products/1200-stage2-data.RData")
d <- stage2_data
cat("ins values:"); print(table(d$ins))
d1 <- d %>% filter(ins == unique(d$ins)[1]) %>% distinct(plant, year, .keep_all = TRUE)
cat("rows (one instrument):", nrow(d1), " corps:", sum(d1$corp), "\n")
ref <- d1 %>% group_by(sic_3, year) %>%
  summarise(Mbar_mean = mean(M_star, na.rm = TRUE), Mbar_med = median(M_star, na.rm = TRUE), n = n(), .groups = "drop") %>%
  mutate(year = year + 1)                      # lagged: value from t-1 used in t
u <- d1 %>% filter(!corp, sales_tax_rate_purchases > 0) %>%
  inner_join(ref, by = c("sic_3", "year")) %>%
  mutate(r_mean = M_star / Mbar_mean, r_med = M_star / Mbar_med)
cat("unincorporated, tau_P>0, with a lagged reference:", nrow(u), "\n")
cut <- quantile(u$M_star, 0.995); u5 <- u %>% filter(M_star <= cut)
cat("after 0.5% trim:", nrow(u5), "\n\n")
for (v in c("r_mean", "r_med")) {
  x <- u5[[v]]
  cat(sprintf("%s = M*/Mbar_{j,t-1}: quantiles p10 %.2f p25 %.2f p50 %.2f p75 %.2f p90 %.2f p95 %.2f p99 %.2f max %.1f\n", v,
      quantile(x,.1),quantile(x,.25),quantile(x,.5),quantile(x,.75),quantile(x,.9),quantile(x,.95),quantile(x,.99),max(x)))
  cat(sprintf("   share with M* > Mbar (ceiling can bind): %.1f%%;  M* > 5*Mbar (max e/M* < 20%%): %.1f%%;  M* > 10*Mbar (< 10%%): %.2f%%\n",
      100*mean(x>1), 100*mean(x>5), 100*mean(x>10)))
}
cat("\nReference scale by industry (lagged mean / median, averaged over years), top 8 industries by n:\n")
print(ref %>% group_by(sic_3) %>% summarise(Mbar_mean = round(mean(Mbar_mean)), Mbar_med = round(mean(Mbar_med)), firms_per_year = round(mean(n))) %>% arrange(desc(firms_per_year)) %>% head(8), n = 8)
cat(sprintf("\nRatio mean/median of the reference, across industry-years: median %.1f, range %.1f-%.1f\n",
    median(ref$Mbar_mean/ref$Mbar_med), min(ref$Mbar_mean/ref$Mbar_med), max(ref$Mbar_mean/ref$Mbar_med)))
