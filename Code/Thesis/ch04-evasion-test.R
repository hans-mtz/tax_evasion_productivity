## PRODUCT: Thesis/tables/ch04-evasion-test.png := Table 4.x, test for overreporting by industry
## Reads:   Code/Products/boot_test_comp_tbl.RData -> pref_tax_ev_test_tbl (206-boot-test.R, "Fix corps,
##          others"): plants resampled separately within corporations and within unincorporated firms
##          (resample_by_group), beta from corporations, mean of V over unincorporated firms only;
##          basic (pivotal) bootstrap CIs, B = 250; stars from the one-sided bootstrap p-value
##          p = 1 - F*(theta_hat) of the centred replicates (render_tbl in 206-boot-test.R). Taken as-is, no re-estimation.
source("Code/Thesis/001-setup.R")

load(file.path(PRODUCTS_DIR, "boot_test_comp_tbl.RData"))   # pref_tax_ev_test_tbl
## Short ISIC Rev. 2 industry names (ciiu_3's own descriptions are long, repeat "Food
## manufacturing" for both 311 and 312, and carry a typo for 369).
names_df <- tribble(
    ~sic_3, ~name,
    "311", "Food products", "312", "Other food products", "313", "Beverages",
    "321", "Textiles", "322", "Wearing apparel", "323", "Leather products",
    "324", "Footwear", "331", "Wood products", "332", "Furniture",
    "341", "Paper products", "342", "Printing and publishing", "351", "Industrial chemicals",
    "352", "Other chemicals", "356", "Plastic products", "369", "Non-metallic minerals",
    "381", "Metal products", "382", "Non-electrical machinery", "383", "Electrical machinery",
    "384", "Transport equipment", "390", "Other manufacturing"
)

tst <- as.data.frame(pref_tax_ev_test_tbl) %>%
    mutate(sic_3 = as.character(sic_3)) %>%
    select(sic_3, type, v = mean_V_log_mats_share) %>%
    pivot_wider(names_from = type, values_from = v) %>%
    left_join(names_df, by = "sic_3") %>%
    arrange(sic_3)

tbl <- tst %>% transmute(
    Industry = paste(sic_3, name),
    `Mean of $\\mathcal V$` = coeff,
    `95\\% CI` = CI
)
print(tbl, n = Inf)

tt_obj <- tt(tbl, align = "lcc", width = c(4, 1.2, 1.6),
             notes = "Mean of $\\mathcal V_{it}=\\ln(\\rho_tM^*_{it}/P_tY_{it})-\\ln\\hat\\beta$ among unincorporated firms, where $\\ln\\hat\\beta$ is the mean log materials share of corporations. Under the null of no overreporting the mean is zero; the alternative is a positive mean. Basic (pivotal) bootstrap confidence intervals (250 replications; plants resampled separately within corporations and within unincorporated firms). *, **, *** denote significance at the 10\\%, 5\\%, and 1\\% levels (one-sided). The intervals are two-sided, so a one-sided rejection at 5\\% can coexist with an interval that includes zero.") %>%
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, "ch04-evasion-test")
