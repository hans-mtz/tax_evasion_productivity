## PRODUCT: Thesis/tables/appD-beta-single-tax.png := appendix D robustness: the output elasticity of materials from the
## corporations' share under the two-tax model (net of sales taxes, the headline) and the single-tax model (gross share).
## Replaces ch06-beta-net-tax (old 1470 diagnostic: earlier sample with codes 6-9 and the old industry set). 2026-10-12 (Hans:
## ch. 6 outline, single tax goes to the appendix). Same production sample as the headline first stage (1501: codes 6-9 out,
## net share above the threshold, 369 also below the upper cut); beta = exp(mean log share | corporations) in both columns.
## Check: the net column reproduces 1501's fs_net_ls beta.
source("Code/Thesis/001-setup.R")
threshold_cut <- 0.05; upper_threshold_cut <- 0.75   # sample cuts as in 021-deconv-funs.R / 1501 / 1510 (0.75 for 369 only)
load(file.path(PRODUCTS_DIR, "931.1-fs-se-het.RData"))   # df (raw panel)
load(file.path(PRODUCTS_DIR, "1501-fs-net.RData"))       # fs_net_ls (headline beta, for the check)
five <- c("321", "322", "324", "342", "369")   # ch. 5 and ch. 6
base <- df %>% filter(!juridical_organization %in% 6:9) %>%
    mutate(t1 = sales_tax_rate_sales * nom_sales, t2 = sales_tax_rate_purchases * nom_mats,
           s_net = log((nom_mats - t2) / (nom_gross_output - t1)), s_gross = log(nom_mats / nom_gross_output))
ok <- is.finite(base$s_net) & base$s_net > log(threshold_cut) & ifelse(base$sic_3 == 369, base$s_net < log(upper_threshold_cut), TRUE)
corp <- base[ok, ] %>% filter(juridical_organization == 3, as.character(sic_3) %in% five, is.finite(s_gross))
res <- corp %>% group_by(sic_3 = as.character(sic_3)) %>%
    summarise(n_corp = n(), beta_net = exp(mean(s_net)), beta_gross = exp(mean(s_gross)), .groups = "drop") %>%
    mutate(d_beta = beta_gross - beta_net, beta_1501 = sapply(sic_3, \(s) fs_net_ls[[s]]$beta))
print(as.data.frame(res), digits = 4)
stopifnot(max(abs(res$beta_net - res$beta_1501)) < 1e-6)
write.csv(res, file.path(PRODUCTS_DIR, "appD-beta-single-tax.csv"), row.names = FALSE)
f3 <- \(x) sprintf("%.3f", x)
tbl <- res %>% transmute(Industry = sic_3, `Corporations` = formatC(n_corp, big.mark = ",", format = "d"),
                         `Two-tax (net share)` = f3(beta_net), `Single-tax (gross share)` = f3(beta_gross),
                         Difference = ifelse(d_beta < 0, paste0("$-$", f3(-d_beta)), f3(d_beta)))
tt_obj <- tt(tbl, align = "lcccc", width = c(0.8, 1, 1.4, 1.4, 1), notes = paste0(
    "Output elasticity of materials, $\\hat\\beta=\\exp(\\mathbb E[s_{it}\\mid\\text{corporation}])$. Two-tax: log materials share net of sales taxes, ",
    "$s_{it}=\\ln[(\\rho_tM_{it}-t_{2,it})/(P_tY_{it}-t_{1,it})]$, the headline (Chapter 2). Single-tax: the gross share $\\ln(\\rho_tM_{it}/P_tY_{it})$. ",
    "Same corporations (corporate firm-years in the production sample); difference = single-tax minus two-tax. Industries: the five of Chapters 5 and 6.")) |>
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "appD-beta-single-tax")
cat("Saved: Thesis/tables/appD-beta-single-tax.{png,pdf}; Code/Products/appD-beta-single-tax.csv\n")
