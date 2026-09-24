## PRODUCT: Thesis/tables/ch06-omega.png := Table 6.x, deconvolved productivity by industry and instrument
## Reads:   Code/Products/omega_deconv_current_pf.RData (293-omega-deconv-current-pf.R): P-spline
##          (penalized cubic B-spline logspline) deconvolution of W~ = omega + (1-beta) eps, f_eps a kernel
##          density of corporations' residuals, with beta and the single-instrument alphas of ch. 6's
##          PF table (m*_{it-1} = lag_m, W~_{it-2} = lag_2_w_eps). All firms.
source("Code/Thesis/001-setup.R")

load(file.path(PRODUCTS_DIR, "omega_deconv_current_pf.RData"))   # omega_cur_stats_df

five <- c("331", "322", "369", "313", "321")
ind_names <- c("331" = "Wood products", "322" = "Wearing apparel", "369" = "Non-metallic minerals",
               "313" = "Beverages", "321" = "Textiles")

st <- omega_cur_stats_df %>%
    rownames_to_column("key") %>%
    separate(key, c("sic_3", "ins"), sep = " ") %>%
    mutate(ins = recode(ins, lag_m = "m", lag_2_w_eps = "w"))
f2 <- \(x) sprintf("%.2f", x)
tbl <- st %>%
    pivot_wider(names_from = ins, values_from = c(mean, sd, skewness)) %>%
    mutate(sic_3 = factor(sic_3, five)) %>% arrange(sic_3) %>%
    transmute(Industry = paste(sic_3, ind_names[as.character(sic_3)]),
              mean_m = f2(mean_m), sd_m = f2(sd_m), sk_m = f2(skewness_m),
              mean_w = f2(mean_w), sd_w = f2(sd_w), sk_w = f2(skewness_w))
print(tbl)

tt_obj <- tt(tbl, align = "lcccccc", width = c(3, 1, 1, 1, 1, 1, 1),
             notes = "Moments of the deconvolved density of productivity $\\omega_{it}$ (all firms), from $\\widetilde{\\mathcal W}_{it}=\\omega_{it}+(1-\\beta)\\varepsilon_{it}$ with $\\beta$ and $(\\alpha_K,\\alpha_L)$ from the corresponding single-instrument estimates in the production function table. Penalized B-spline (logspline) deconvolution; the density of $\\varepsilon$ is a kernel estimate from corporations' residuals. The level of $\\omega$ depends on $(\\alpha_K,\\alpha_L)$, so means are comparable within a column, not across instruments.") %>%
    group_tt(j = list(" " = 1, "$m^*_{it-1}$" = 2:4, "$\\tilde{\\mathcal W}_{it-2}$" = 5:7)) %>%
    style_tt(i = "notes", fontsize = 0.65)
colnames(tt_obj) <- c("Industry", "Mean", "SD", "Skewness", "Mean", "SD", "Skewness")
render_thesis_table(tt_obj, "ch06-omega")
