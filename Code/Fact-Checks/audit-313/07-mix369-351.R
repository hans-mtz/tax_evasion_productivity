suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity")
load("Code/Products/colombia_data.RData")
thr <- log(0.05)
d <- colombia_data_frame %>% ungroup() %>%
  filter(sic_3 %in% c(369, 351, 331), is.finite(y), is.finite(k), is.finite(l), is.finite(m), !as.integer(as.character(juridical_organization)) %in% 6:9) %>%
  mutate(corp = juridical_organization == 3, t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
         s = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1)))) %>% filter(is.finite(s), s > thr)
options(width = 200, pillar.sigfig = 3)
print(d %>% group_by(sic_3, sic_4, corp) %>% summarise(n = n(), plants = n_distinct(plant), mean = mean(s), sd = sd(s), .groups = "drop") %>%
  pivot_wider(names_from = corp, values_from = c(n, plants, mean, sd)), n = Inf)
## switchers in 313: effect of dropping plants that appear in both groups
load("Code/Products/colombia_data.RData")
x <- colombia_data_frame %>% ungroup() %>% filter(sic_3 == 313, is.finite(y), is.finite(k), is.finite(l), is.finite(m),
       !as.integer(as.character(juridical_organization)) %in% 6:9) %>%
  mutate(corp = juridical_organization == 3, t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
         s = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1)))) %>% filter(is.finite(s), s > thr)
sw <- x %>% group_by(plant) %>% filter(n_distinct(corp) > 1) %>% pull(plant) %>% unique()
y <- x %>% filter(!plant %in% sw)
cat(sprintf("313 switchers: %d plants, %d rows; mean V without them %.3f (with %.3f)\n", length(sw), sum(x$plant %in% sw),
    mean(y$s[!y$corp]) - mean(y$s[y$corp]), mean(x$s[!x$corp]) - mean(x$s[x$corp])))
