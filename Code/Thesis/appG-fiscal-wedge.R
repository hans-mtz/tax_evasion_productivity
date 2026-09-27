## PRODUCT: Thesis/tables/appG-fiscal-wedge.png := appendix table, the tax wedge in the 1983-reform event study.
## Outcome: gross minus net-of-sales-tax log materials share (the wedge ln((1-tau_S)/(1-tau_P)) that the two-tax model puts in
## the gross share), same difference-to-1983 models as ch. 7 (Code/Deconvolution/1530-fiscal-did.R, object `wedge`), common
## sample, codes 6-9 dropped. Its path is exactly the gap between the gross-share and net-share event studies: the mechanical
## effect of the rate change that the net share removes. SEs clustered by plant (with 11 year clusters the two-way variance
## matrix is not positive definite). Built 2026-09-27 (Hans: macro reviewers may argue that prices adjust mechanically after
## a tax change; this isolates the part the correct model captures).
source("Code/Thesis/ch07-fiscal-table-helpers.R")   # 001-setup, fixest, YEARS, year_path()
load(file.path(PRODUCTS_DIR, "1530-fiscal-did.RData"))   # wedge

cols <- list(
    `Liable` = list(model = wedge$crp, prefix = "corp_exempt_y83::Other:Taxed:"),
    `Exempt` = list(model = wedge$crp, prefix = "corp_exempt_y83::Other:Exempt:"),
    `LLCs, liable` = list(model = wedge$jo, prefix = "jo_exempt_y83::Ltd. Co.:Taxed:"),
    `Proprietorships, liable` = list(model = wedge$jo, prefix = "jo_exempt_y83::Proprietorship:Taxed:"))
tbl <- data.frame(Year = paste0("19", YEARS), check.names = FALSE)
for (lab in names(cols)) tbl[[lab]] <- year_path(cols[[lab]]$model, cols[[lab]]$prefix)
print(tbl)
note <- paste0(
    "Outcome: the gross minus the net-of-sales-tax log materials share, $\\ln(\\rho_tM^*_{it}/P_tY_{it})-\\ln\\big((1-\\tau_{P,it})\\rho_tM^*_{it}/(P_tY_{it}-\\tau_{S,it}\\,\\text{sales}_{it})\\big)$, ",
    "the tax wedge that the gross share carries. Same specification as the differences in the event study of the 1983 reform: ",
    "difference relative to 1983 (reference, --), industry fixed effects, unincorporated firms (or LLCs, proprietorships) relative to corporations, ",
    "in sales-tax-liable and exempt industries. The coefficients equal the gap between the event study on the gross share and on the net share. ",
    "Standard errors in parentheses, clustered by plant. * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Observations: ",
    format(wedge$n, big.mark = ","), "; juridical organization codes 6--9 excluded.")
tt_obj <- tt(tbl, width = c(0.8, 1, 1, 1, 1.2), notes = note) |>
    group_tt(j = list(" " = 1, "Unincorporated firms" = 2:3, " " = 4:5)) |>
    style_tt("notes", fontsize = 0.8)
render_thesis_table(tt_obj, "appG-fiscal-wedge")
