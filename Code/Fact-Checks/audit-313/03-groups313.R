## Characterize 313 groups: corporations, unincorporated (0,1,2,4,5), code 8, code 6
suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity")
load("Code/Products/colombia_data.RData")
thr <- log(0.05)
d <- colombia_data_frame %>% ungroup() %>%
  filter(sic_3 == 313, is.finite(y), is.finite(k), is.finite(l), is.finite(m)) %>%
  mutate(jo = as.integer(as.character(juridical_organization)),
         grp = case_when(jo == 3 ~ "corp", jo %in% c(0,1,2,4,5) ~ "uninc", jo == 8 ~ "code8", TRUE ~ paste0("code", jo)),
         t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
         s_gross = log(nom_mats / nom_gross_output),
         s_net = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1))),
         t3_sales = consumption_taxes_on_sales / sales,     # consumption (excise) taxes over sales, both real
         t6_sales = indirect_taxes / sales,
         go_sales = log(nom_gross_output / nom_sales),
         s_net_x = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1 - consumption_taxes_on_sales * nom_sales / sales)))) %>%
  filter(is.finite(s_gross), s_gross > thr)
options(width = 250, pillar.sigfig = 4)
cat("== by group ==\n")
print(d %>% group_by(grp) %>% summarise(n = n(), plants = n_distinct(plant), s_gross = mean(s_gross), sd_gross = sd(s_gross),
          s_net = mean(s_net, na.rm = TRUE), tauS = mean(sales_tax_rate_sales, na.rm=TRUE), tauP = mean(sales_tax_rate_purchases, na.rm=TRUE),
          t3_sales = mean(t3_sales, na.rm = TRUE), t6_sales = mean(t6_sales, na.rm = TRUE), go_sales = mean(go_sales, na.rm = TRUE),
          s_net_x = mean(s_net_x, na.rm = TRUE), GO_share = sum(nom_gross_output)))
cat("== by group x sic_4 ==\n")
print(d %>% group_by(grp, sic_4) %>% summarise(n = n(), plants = n_distinct(plant), s_gross = mean(s_gross), s_net = mean(s_net, na.rm=TRUE),
          tauS = mean(sales_tax_rate_sales, na.rm=TRUE), tauP = mean(sales_tax_rate_purchases, na.rm=TRUE),
          t3_sales = mean(t3_sales, na.rm = TRUE), go_sales = mean(go_sales, na.rm=TRUE), .groups = "drop"), n = Inf)
cat("== code 8 plants: years, rows ==\n")
print(d %>% filter(grp == "code8") %>% group_by(plant) %>% summarise(n = n(), yrs = paste(range(year), collapse="-"), sic4 = paste(unique(sic_4), collapse=","),
          s_gross = mean(s_gross), t3_sales = mean(t3_sales, na.rm=TRUE), jo_other = NA) %>% arrange(desc(n)), n = Inf)
cat("== plants switching legal form in 313 (any rows) ==\n")
sw <- d %>% group_by(plant) %>% summarise(ng = n_distinct(grp), grps = paste(sort(unique(grp)), collapse="/")) %>% filter(ng > 1)
print(sw, n = Inf)
cat("rows from switchers:", sum(d$plant %in% sw$plant), "\n")
## mean V by year, current definition (net, drop 6-9)
cur <- d %>% filter(grp %in% c("corp","uninc"), is.finite(s_net), s_net > thr)
lnD <- mean(cur$s_net[cur$grp == "corp"])
cat("== current sample: lnD", round(lnD,3), " mean V uninc", round(mean(cur$s_net[cur$grp=="uninc"]) - lnD, 3), "\n")
print(cur %>% group_by(year) %>% summarise(n_c = sum(grp=="corp"), n_u = sum(grp=="uninc"), lnD_t = mean(s_net[grp=="corp"]),
       su = mean(s_net[grp=="uninc"]), V_fixedD = su - lnD, V_yearD = su - lnD_t), n = Inf)
## influence: mean V leaving out top plants (by contribution)
u <- cur %>% filter(grp == "uninc") %>% mutate(V = s_net - lnD)
pl <- u %>% group_by(plant) %>% summarise(n = n(), sumV = sum(V), meanV = mean(V)) %>% arrange(desc(sumV))
cat("plants uninc:", nrow(pl), " top 5 plants carry rows", sum(pl$n[1:5]), "\n"); print(head(pl, 8))
for (k in c(1, 3, 5, 10)) cat(sprintf("drop top %d plants by sum V: mean V %.3f\n", k, mean(u$V[!u$plant %in% pl$plant[1:k]])))
cat(sprintf("median V uninc %.3f ; trimmed mean 10%% %.3f\n", median(u$V), mean(u$V, trim = .1)))
## corporate benchmark: robustness
cc <- cur %>% filter(grp == "corp")
cat(sprintf("corp: mean %.3f median %.3f sd %.3f; by sic4:\n", mean(cc$s_net), median(cc$s_net), sd(cc$s_net)))
## excise diagnostic: V with consumption taxes also removed from GO
cx <- d %>% filter(grp %in% c("corp","uninc"), is.finite(s_net_x), s_net_x > thr)
cat(sprintf("Diagnostic, share net of t1 AND consumption taxes in GO: lnD %.3f mean V %.3f (n %d/%d)\n",
            mean(cx$s_net_x[cx$grp=="corp"]), mean(cx$s_net_x[cx$grp=="uninc"]) - mean(cx$s_net_x[cx$grp=="corp"]),
            sum(cx$grp=="corp"), sum(cx$grp=="uninc")))
