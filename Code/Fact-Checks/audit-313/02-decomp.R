## Decomposition of mean V (test's mu-hat) from the approved test to the current 1510 test, industry by industry.
## Read-only: loads repo products, writes only to Code/Fact-Checks/audit-313.
suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity")
out_dir <- "Code/Fact-Checks/audit-313"
load("Code/Products/colombia_data.RData"); load("Code/Products/global_vars.RData")
load("Code/Products/test_data.RData")
thr <- log(0.05)
inds <- c(313, 321, 322, 324, 331, 342, 351, 352, 369)

base <- colombia_data_frame %>% ungroup() %>%
  filter(is.finite(y), is.finite(k), is.finite(l), is.finite(m), sic_3 %in% top_20_inds$sic_3) %>%
  mutate(corp = juridical_organization == 3,
         jo = as.integer(as.character(juridical_organization)),
         t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
         s_gross = log(nom_mats / nom_gross_output),
         s_net   = suppressWarnings(log((nom_mats - t2) / (nom_gross_output - t1))),          # 1501/1510
         s_net_m = suppressWarnings(log((nom_mats - t2) / ((1 - sales_tax_rate_sales) * nom_gross_output))), # model
         s_net_purch_only = suppressWarnings(log((nom_mats - t2) / nom_gross_output)),
         s_net_sales_only = suppressWarnings(log(nom_mats / (nom_gross_output - t1))))

## sanity: the approved test's sample = base with share filter (test_data rows)
chk <- base %>% filter(sic_3 == 313, is.finite(s_gross), s_gross > thr)
cat("313 approved-test sample check: base rows", nrow(chk), " test_data rows",
    nrow(test_data %>% filter(sic_3 == 313, log_mats_share > thr)), "\n")

grid_mu <- seq(0, 1, by = 0.001)
ts_at <- function(a, mu, s, corp, cl) {
  n <- length(s); g <- cbind(corp * (s - a), (!corp) * (s - a - mu))
  gb <- colMeans(g); G <- rowsum(g, cl); G <- G - outer(as.vector(table(cl)[rownames(G)]), gb)
  drop(n * t(gb) %*% solve(crossprod(G) / n, gb))
}
stat <- function(d, share, drop_codes = integer(0), region = TRUE) {
  d <- d %>% filter(!jo %in% drop_codes, is.finite(.data[[share]]), .data[[share]] > thr)
  s <- d[[share]]; corp <- d$corp
  lnD <- mean(s[corp]); mu <- mean(s[!corp]) - lnD
  out <- tibble(n_corp = sum(corp), n_uninc = sum(!corp), lnD = lnD, beta = exp(lnD),
                s_uninc = mean(s[!corp]), muhat = mu,
                var_ratio = var(s[!corp]) / var(s[corp]))
  if (region) {
    TS <- sapply(grid_mu, \(m) ts_at(lnD, m, s, corp, d$plant)); p <- grid_mu[TS <= qchisq(.95, 1)]
    out$sharp <- if (length(p)) sprintf("[%.3f, %.3f]", min(p), max(p)) else "empty"
    out$TS0 <- TS[1]
  }
  out
}

steps <- list(
  list(step = "S0 approved: gross, uninc = all jo!=3",          share = "s_gross", drop = integer(0)),
  list(step = "1a: + net share (GO-t1) only",                    share = "s_net",   drop = integer(0)),
  list(step = "1b: + net share ((1-tauS)GO) only",               share = "s_net_m", drop = integer(0)),
  list(step = "1c: + net of t2 in numerator only",               share = "s_net_purch_only", drop = integer(0)),
  list(step = "1d: + net of t1 in denominator only",             share = "s_net_sales_only", drop = integer(0)),
  list(step = "2a: + drop codes 6-9 only (gross)",               share = "s_gross", drop = 6:9),
  list(step = "2b: + drop code 8 only (gross)",                  share = "s_gross", drop = 8),
  list(step = "2c: + drop codes 6,7,9 only (gross)",             share = "s_gross", drop = c(6, 7, 9)),
  list(step = "C1 = current 1510: net (GO-t1), drop 6-9",       share = "s_net",   drop = 6:9),
  list(step = "C2: net ((1-tauS)GO), drop 6-9",                  share = "s_net_m", drop = 6:9)
)
res <- map_dfr(inds, function(sic) {
  d <- base %>% filter(sic_3 == sic)
  map_dfr(steps, \(st) bind_cols(tibble(sic_3 = sic, step = st$step), stat(d, st$share, st$drop, region = TRUE)))
})
options(width = 250, pillar.sigfig = 4)
print(res %>% mutate(across(c(lnD, beta, s_uninc, muhat, var_ratio, TS0), \(x) round(x, 3))), n = Inf)
write.csv(res, file.path(out_dir, "02-decomp.csv"), row.names = FALSE)

## juridical composition of the uninc group by industry
comp <- base %>% filter(sic_3 %in% inds, is.finite(s_gross), s_gross > thr, !corp) %>%
  count(sic_3, jo) %>% group_by(sic_3) %>% mutate(share = n / sum(n)) %>% ungroup()
print(comp %>% filter(jo >= 6), n = Inf)
