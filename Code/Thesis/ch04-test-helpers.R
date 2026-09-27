## Shared by the ch. 4 / appendix test tables (ch04-evasion-test.R, appF-evasion-test-*.R): industry labels and formats.
## Short ISIC Rev. 2 industry names (ciiu_3's own descriptions are long, repeat "Food manufacturing" for 311 and 312,
## and carry a typo for 369).
test_names <- tribble(
    ~sic_3, ~name,
    "311", "Food products", "312", "Other food products", "313", "Beverages",
    "321", "Textiles", "322", "Wearing apparel", "323", "Leather products",
    "324", "Footwear", "331", "Wood products", "332", "Furniture",
    "341", "Paper products", "342", "Printing and publishing", "351", "Industrial chemicals",
    "352", "Other chemicals", "356", "Plastic products", "369", "Non-metallic minerals",
    "381", "Metal products", "382", "Non-electrical machinery", "383", "Electrical machinery",
    "384", "Transport equipment", "390", "Other manufacturing"
)
## Test-inversion region on the grid mu in [0, 1]: open lower end when mu = 0 passes; dagger when nothing passes
fmt_region <- function(lo, hi) ifelse(is.na(lo), "$\\dagger$",
                                      ifelse(lo == 0, sprintf("$(\\,\\cdot\\,,\\ %.3f]$", hi), sprintf("$[%.3f,\\ %.3f]$", lo, hi)))
test_inv_note <- function(chi, df) paste0(
    "$\\hat\\mu$: mean of $\\mathcal V_{it}=\\ln(\\rho_tM^*_{it}/P_tY_{it})-\\ln\\hat D$ among unincorporated firms, with the ",
    "log materials share net of sales taxes and $\\ln\\hat D$ the mean among corporations. 95\\% test-inversion region for ",
    "$\\mu=E[\\mathcal V_{it}]$ over the grid $\\mu\\in[0,1]$ (under the model $\\mu\\ge0$), taking $\\ln\\hat D$ as the true value; ",
    "plant-clustered, efficiently weighted moments; ", chi, " critical value (", df, "). ",
    "Where the region excludes 0, the test rejects no overreporting. $(\\,\\cdot\\,,\\ b]$: $\\mu=0$ is not rejected. ",
    "$\\dagger$ Empty region: every $\\mu\\in[0,1]$ is rejected.")
