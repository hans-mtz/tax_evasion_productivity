## PRODUCT: Code/Products/1530-fiscal-did.RData := the ch. 7 (1983 reform) event-study models on the final sample definition
## (2026-09-26): juridical organization codes 6-9 dropped (PLAN.md §9a). Two outcome variants, for Hans to choose between:
##   "gross": log materials share (as in 910/921.1, the current ch. 7);
##   "net":   log materials share net of sales taxes, log((nom_mats - tau_P nom_mats) / (nom_gross_output - tau_S nom_sales)),
##            as in 1501 -- under the two-tax model the gross share carries the wedge ln((1-tau_S)/(1-tau_P)), and the 1983
##            reform changed those rates, so a gross-share DiD can mix a mechanical wedge change into the evasion response.
## Data: wip_df from 921-DD2.RData (the object 921.1 actually uses; carries exempt_ind). Sample rule: share > 5% on the
## outcome's own share. Models copied unchanged from 921.1-DD.R (rg_lvl_crp, rg_lvl_b83_crp, rg_lvl_jo, rg_lvl_b83_jo) and
## ch07-fiscal-all-table.R (reg_all, two models); two-way clustered by plant and year. Also the tax-wedge models (`wedge`).
## DECIDED 2026-09-26 (Hans): ch. 7 uses the NET share; the wedge table goes to an appendix.
## Note: the old grouping put code 6 in "Corporation" for the jo models (JO_class) but in "Other" for the corp models.
library(tidyverse); library(fixest)
load("Code/Products/921-DD2.RData")   # wip_df (with exempt_ind)
threshold_cut <- 0.05

base <- wip_df %>% filter(!juridical_organization %in% 6:9) %>%
    mutate(log_mats_share_net = suppressWarnings(log((nom_mats * (1 - sales_tax_rate_purchases)) /
                                                     (nom_gross_output - sales_tax_rate_sales * nom_sales))),
           jo = droplevels(jo), corp = droplevels(corp),
           corp_exempt_year = factor(ifelse(corp == "Corp", "Base", paste(corp, exempt_ind, year, sep = ":"))),
           jo_exempt_year   = factor(ifelse(jo == "Corporation", "Base", paste(jo, exempt_ind, year, sep = ":"))),
           corp_exempt_y83  = factor(ifelse(corp == "Corp" | year == "83", "Base", paste(corp, exempt_ind, year, sep = ":"))),
           jo_exempt_y83    = factor(ifelse(jo == "Corporation" | year == "83", "Base", paste(jo, exempt_ind, year, sep = ":"))))

fit_all <- function(outcome) {
    d <- base %>% filter(is.finite(.data[[outcome]]), .data[[outcome]] > log(threshold_cut)) %>% mutate(s = .data[[outcome]])
    cl <- ~ plant + year
    list(
        n = nrow(d),
        reg_all        = feols(s ~ sw(i(corp, year, "Corp"), corp + i(corp, year, "Corp", ref2 = 83)) | sic_3, cluster = cl, data = d),
        rg_lvl_crp     = feols(s ~ i(corp_exempt_year, "Base") | sic_3, cluster = cl, data = d),
        rg_lvl_jo      = feols(s ~ i(jo_exempt_year, "Base") | sic_3, cluster = cl, data = d),
        rg_lvl_b83_crp = feols(s ~ corp + i(corp, exempt_ind, "Corp") + i(corp_exempt_y83, "Base") | sic_3, cluster = cl, data = d),
        rg_lvl_b83_jo  = feols(s ~ jo + i(jo, exempt_ind, "Corporation") + i(jo_exempt_y83, "Base") | sic_3, cluster = cl, data = d)
    )
}
did <- list(gross = fit_all("log_mats_share"), net = fit_all("log_mats_share_net"))

## The tax wedge: gross minus net log share, on the common sample, same difference models (relative to 1983). Its path is
## exactly the gross-minus-net gap in the event study: the mechanical effect of the rate change on the gross share, which
## the net share removes. Clustered by plant only: with 11 year clusters the two-way variance matrix is not positive definite.
dw <- base %>% filter(is.finite(log_mats_share_net), log_mats_share_net > log(threshold_cut), log_mats_share > log(threshold_cut)) %>%
    mutate(wedge = log_mats_share - log_mats_share_net)
wedge <- list(
    n = nrow(dw), mean = mean(dw$wedge),
    crp = feols(wedge ~ corp + i(corp, exempt_ind, "Corp") + i(corp_exempt_y83, "Base") | sic_3, cluster = ~plant, data = dw),
    jo  = feols(wedge ~ jo + i(jo, exempt_ind, "Corporation") + i(jo_exempt_y83, "Base") | sic_3, cluster = ~plant, data = dw))
save(did, wedge, file = "Code/Products/1530-fiscal-did.RData")

## Comparison of the paths the ch. 7 figures report (coefficient, SE)
path <- function(m, prefix) { ct <- coeftable(m); nm <- paste0(prefix, 81:91)
    tibble(year = 81:91, b = ifelse(nm %in% rownames(ct), ct[pmin(match(nm, rownames(ct)), nrow(ct)), 1], NA),
           se = ifelse(nm %in% rownames(ct), ct[pmin(match(nm, rownames(ct)), nrow(ct)), 2], NA)) }
series <- list(
    all_level   = list("reg_all", 1, "corp::Other:year::"),
    liable_diff = list("rg_lvl_b83_crp", NA, "corp_exempt_y83::Other:Taxed:"),
    exempt_diff = list("rg_lvl_b83_crp", NA, "corp_exempt_y83::Other:Exempt:"),
    llc_liable_diff = list("rg_lvl_b83_jo", NA, "jo_exempt_y83::Ltd. Co.:Taxed:"),
    prt_liable_diff = list("rg_lvl_b83_jo", NA, "jo_exempt_y83::Proprietorship:Taxed:"))
cmp <- imap_dfr(series, function(sp, nm) map_dfr(c("gross", "net"), function(v) {
    m <- did[[v]][[sp[[1]]]]; if (!is.na(sp[[2]])) m <- m[[sp[[2]]]]
    path(m, sp[[3]]) %>% mutate(series = nm, variant = v) })) %>%
    mutate(cell = ifelse(is.na(b), "--", sprintf("%.3f (%.3f)", b, se))) %>%
    select(series, year, variant, cell) %>% pivot_wider(names_from = variant, values_from = cell)
options(width = 200); cat("n: gross", did$gross$n, "| net", did$net$n, "\n"); print(cmp, n = Inf)
write.csv(cmp, "Code/Products/1530-fiscal-did-compare.csv", row.names = FALSE)
cat("Saved: Code/Products/1530-fiscal-did.RData, 1530-fiscal-did-compare.csv\n")
