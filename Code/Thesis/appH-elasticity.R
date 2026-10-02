## PRODUCT: Thesis/tables/appH-elasticity.png := appendix table, midpoint elasticity of overreporting with respect to tau_P,
## 1983 to each post-reform year, with the test-inversion set (@tbl-app-elasticity, appendix H). Built 2026-10-01.
## Source: Code/Products/ch07-overreporting-elasticity-inversion.csv (Code/Thesis/ch07-overreporting-elasticity-inversion.R).
## Unincorporated firms in ST-liable industries (all but 311, 312); mean tau_P; net share against the corporations' mean log share
## by industry; variance clustered by plant.
source("Code/Thesis/001-setup.R")
res <- read.csv(file.path(PRODUCTS_DIR, "ch07-overreporting-elasticity-inversion.csv"), colClasses = c(post = "character"))
tbl <- res %>% transmute(
    Year = paste0("19", post),
    `Overreporting 1983` = sprintf("%.1f\\%%", 100 * r0),
    `Overreporting, year` = sprintf("%.1f\\%%", 100 * r1),
    `Mean $\\tau_P$ 1983` = sprintf("%.1f\\%%", 100 * T0),
    `Mean $\\tau_P$, year` = sprintf("%.1f\\%%", 100 * T1),
    Elasticity = sprintf("%.1f", E_mid),
    `95\\% set` = sprintf("[%.1f, %.1f]", ci_lo, ci_hi)
)
print(tbl)
note <- paste0(
    "Midpoint elasticity of overreporting, as a share of true materials, with respect to the sales tax rate on purchases $\\tau_P$, 1983 to each year: ",
    "the change in overreporting over its midpoint, divided by the change in $\\tau_P$ over its midpoint. Unincorporated firms in sales-tax-liable industries. ",
    "The 95\\% set collects the elasticities that a $\\chi^2_1$ test does not reject at 5\\%, taking the corporations' mean log share by industry, ",
    "the overreporting levels and the mean rates as the truth; variance clustered by plant. Overreporting cannot be negative, which caps the elasticity at ",
    sprintf("%.1f in 1987.", res$cap[res$post == "87"]))
tt_obj <- tt(tbl, width = c(0.7, 1, 1, 1, 1, 0.9, 1.2), notes = note) |>
    style_tt("notes", fontsize = 0.8)
render_thesis_table(tt_obj, "appH-elasticity")
