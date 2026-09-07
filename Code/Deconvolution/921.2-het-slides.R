## %% Setup ---------------------
library(tidyverse)
library(fixest)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/910-reg-results.RData") # wip_df, dict
load("Code/Products/921-DD2.RData") # sales_tax_83_group
load("Code/Products/921.1-DD.RData")

# Palettes for color-blind friendly plots

tol_cb_palette <- c(
  "#332288", "#76b1cf", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)

wong_cb_palette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536",
  "#0072B2", "#D55E00", "#CC79A7"
)

ibm_cb_palette <- c(
  "#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000"
)

kelly_max_contrast_palette <- c(
  "#FFB300", "#803E75", "#FF6800", "#A6BDD7", "#C10020",
  "#CEA262", "#817066", "#007D34", "#F6768E", "#00538A",
  "#FF7A5C", "#53377A", "#FF8E00", "#B32851", "#F4C800",
  "#7F180D", "#93AA00", "#593315", "#F13A13", "#232C16"
)

palette(wong_cb_palette)

# my_par <- par(
#     mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
#     mgp = c(1.5, 0.4, 0),
#     family = "serif", cex.main = 1.3, cex.sub = 1.1
# )

# old_par <- par(fig = c(0, 1, 0, 1), new = TRUE, xpd = NA)

width <- 720
height <- 480

## %% Segmenting Grid Plot for Slides ---------------------


png(
    file = "Paper/images/921-2-joint.png",
    width = width,
    height = height,
    # res = 300
)

# par(mfcol=c(1,2), oma=c(4,4,4,2), family = "serif", 
#     cex.main = 1.3, cex.sub = 1.1)
# par(my_par)
par(
    mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1
)

rg_lvl_crp |> coef() |> names()


# par(mfcol=c(3,2), oma=c(4,4,4,2))

coefplot(
    rg_lvl_crp,
    keep = "Exempt",
    group = list(
        ` ` = "^^corp_exempt_year::Other:Exempt:"
    ),
    dict = dict,
    col = 2,
    ylim = c(-0.075, 0.15),
    main = "Levels",
    grid = FALSE,
    value.lab = ""
)

coefplot(
    rg_lvl_crp,
    keep = "Taxed",
    group = list(
        ` ` = "^^corp_exempt_year::Other:Taxed:"
    ),
    add = TRUE,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    grid = FALSE,
    value.lab = ""
)

abline(v=3, lty=2, col="gray")

rg_lvl_b83_crp |> coef() |> names()

ref_vec <- rg_lvl_b83_crp |> coef() |> names() |>grep("::Other:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp_exempt_y83::Other:Taxed:83"

coefplot(
    rg_lvl_b83_crp,
    keep = "Other:Taxed",
    group = list(
        ` ` = "^^corp_exempt_y83::Other:Taxed:"
    ),
    ref = ref_vec,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    # ylim = c(-0.04, 0.10),
    ylim = c(-0.075, 0.15),
    main = "Diff v. 1983",
    grid = FALSE,
    value.lab = ""
)

ref_vec <- rg_lvl_b83_crp |> coef() |> names() |>grep("::Other:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp_exempt_y83::Other:Exempt:83"


coefplot(
    rg_lvl_b83_crp,
    keep = "Other:Exempt",
    group = list(
        ` ` = "^^corp_exempt_y83::Other:Exempt:"
    ),
    ref = ref_vec,
    add = TRUE,
    dict = dict,
    col = 2,
    grid = FALSE,
    value.lab = ""
)
abline(v=3, lty=2, col="gray")

mtext(
    "Tax Evasion by Unincorporated Firms",
    # side = 2, 
    line = 2, outer = TRUE,
    cex = 1.5, family = "serif", font = 2
)
mtext(
    "Input Overreporting in ST-Exempt and ST-Liable Industries",
    #  side = 3, 
     line = 0.6, outer = TRUE,
     cex = 1.3, family = "serif")
# mtext("Tax Evasion by Materials Overreporting.\nUnincorporated Firms in ST-Liable and ST-Exempt Industries", side = 3, line = 1, outer = TRUE)
title(
    # main = list("Tax Evasion by Unincorporated Firms", cex = 1.5), 
    # sub="Input Overreporting in Industries Exempt and Liable of ST.",
    xlab = "Year",
    ylab = "Coefficient Estimate and 95% CI",
    family = "serif",
    line = 1,
    outer = TRUE
)
old_par <- par(fig = c(0, 1, 0, 1), new = TRUE, xpd = NA)
plot.new()
legend(
    "top",
    inset = c(0, -0.1),
    horiz = TRUE,
    bty = "n",
    legend = c("Exempt", "Liable"),
    col = c(2, 1),
    pch = 16,
    xjust = 0.5
)
par(old_par)

dev.off()

## %% Plot Joint ---------------------

png(
    file = "Paper/images/921-2-joint-2.png",
    width = width,
    height = height,
    # res = 300
)

# par(mfcol=c(1,2), oma=c(4,4,4,2), family = "serif", cex.main = 1.3, cex.sub = 1.1)
par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

rg_lvl_jo|> coef() |> names()

coefplot(
    rg_lvl_jo,
    keep = "Proprietorship:Exempt",
    group = list(
        ` ` = "^^jo_exempt_year::Proprietorship:Exempt:"
    ),
    dict = dict,
    col = 2,
    ylim = c(-0.12, 0.15),
    main = "Levels",
    grid = FALSE,
    value.lab = ""
)

coefplot(
    rg_lvl_jo,
    keep = "Proprietorship:Taxed",
    group = list(
        ` ` = "^^jo_exempt_year::Proprietorship:Taxed:"
    ),
    add = TRUE,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    grid = FALSE,
    value.lab = ""
)
abline(v=3, lty=2, col="gray")

rg_lvl_b83_jo|> coef() |> names()

ref_vec <- rg_lvl_b83_jo |> coef() |> names() |>grep("::Proprietorship:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_y83::Proprietorship:Exempt:83"

coefplot(
    rg_lvl_b83_jo,
    keep = "Proprietorship:Exempt:",
    group = list(
        ` ` = "^^jo_exempt_y83::Proprietorship:Exempt:"
    ),
    dict = dict,
    ref = ref_vec,
    col = 2,
    ylim = c(-0.12, 0.15),
    main = "Diff v. 1983",
    grid = FALSE,
    value.lab = ""
)

ref_vec <- rg_lvl_b83_jo |> coef() |> names() |>grep("::Proprietorship:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_y83::Proprietorship:Taxed:83"


coefplot(
    rg_lvl_b83_jo,
    keep = "Proprietorship:Taxed",
    group = list(
        ` ` = "^^jo_exempt_y83::Proprietorship:Taxed:"
    ),
    add = TRUE,
    ref = ref_vec,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    grid = FALSE,
    value.lab = ""
)
abline(v=3, lty=2, col="gray")

mtext(
    "Tax Evasion by Proprietorships",
    # side = 2, 
    line = 2, outer = TRUE,
    cex = 1.5, family = "serif", font = 2
)
mtext(
    "Input Overreporting in ST-Exempt and ST-Liable Industries",
    #  side = 3, 
     line = 0.6, outer = TRUE,
     cex = 1.3, family = "serif")
# mtext("Tax Evasion by Materials Overreporting.\nProprietorships in ST-Liable and ST-Exempt Industries", side = 3, line = 1, outer = TRUE)
title(
    # main = list("Tax Evasion by Proprietorships", cex = 1.5), 
    # sub="Input Overreporting in Industries Exempt and Liable of ST.",
    xlab = "Year",
    ylab = "Coefficient Estimate and 95% CI",
    family = "serif",
    line = 1,
    outer = TRUE
)
old_par <- par(fig = c(0, 1, 0, 1), new = TRUE, xpd = NA)
plot.new()
legend(
    "top",
    inset = c(0, -0.1),
    horiz = TRUE,
    bty = "n",
    legend = c("Exempt", "Liable"),
    col = c(2, 1),
    pch = 16,
    xjust = 0.5
)

par(old_par)

dev.off()

## %% Plot Joint, LLCs ---------------------

png(
    file = "Paper/images/921-2-joint-3.png",
    width = width,
    height = height,
    # res = 300
)

# par(mfcol=c(1,2), oma=c(4,4,4,2), family = "serif", cex.main = 1.3, cex.sub = 1.1)
# par(my_par)
par(
    mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1
)

coefplot(
    rg_lvl_jo,
    keep = "Ltd. Co.:Exempt",
    group = list(
        ` ` = "^^jo_exempt_year::Ltd. Co.:Exempt:"
    ),
    dict = dict,
    col = 2,
    ylim = c(-0.10, 0.15),
    main = "Levels",
    grid = FALSE,
    value.lab = ""
)

coefplot(
    rg_lvl_jo,
    keep = "Ltd. Co.:Taxed",
    group = list(
        ` ` = "^^jo_exempt_year::Ltd. Co.:Taxed:"
    ),
    add = TRUE,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    grid = FALSE,
    value.lab = ""
)
abline(v=3, lty=2, col="gray")


ref_vec <- rg_lvl_b83_jo |> coef() |> names() |>grep("::Ltd. Co.:Exempt:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_y83::Ltd. Co.:Exempt:83"

coefplot(
    rg_lvl_b83_jo,
    keep = "Ltd. Co.:Exempt",
    group = list(
        ` ` = "^^jo_exempt_y83::Ltd. Co.:Exempt:"
    ),
    dict = dict,
    ref = ref_vec,
    col = 2,
    # ylim = c(-0.06, 0.12),
    ylim = c(-0.10, 0.15),
    main = "Diff v. 1983",
    grid = FALSE,
    value.lab = ""
)

ref_vec <- rg_lvl_b83_jo |> coef() |> names() |>grep("::Ltd. Co.:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_y83::Ltd. Co.:Taxed:83"

coefplot(
    rg_lvl_b83_jo,
    keep = "Ltd. Co.:Taxed",
    group = list(
        ` ` = "^^jo_exempt_y83::Ltd. Co.:Taxed:"
    ),
    add = TRUE,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    ref = ref_vec,
    grid = FALSE,
    value.lab = ""
)

abline(v=3, lty=2, col="gray")

mtext(
    "Tax Evasion by LLCs",
    # side = 2, 
    line = 2, outer = TRUE,
    cex = 1.5, family = "serif", font = 2
)
mtext(
    "Input Overreporting in ST-Exempt and ST-Liable Industries",
    #  side = 3, 
     line = 0.6, outer = TRUE,
     cex = 1.3, family = "serif")
# mtext("Tax Evasion by Materials Overreporting.\nLLCs in ST-Liable and ST-Exempt Industries", side = 3, line = 1, outer = TRUE)
title(
    # main = list("Tax Evasion by LLCs", cex = 1.5), 
    # sub="Input Overreporting in Industries Exempt and Liable of ST.",
    xlab = "Year",
    ylab = "Coefficient Estimate and 95% CI",
    family = "serif",
    line = 1,
    outer = TRUE
)
old_par <- par(fig = c(0, 1, 0, 1), new = TRUE, xpd = NA)
plot.new()
legend(
    "top",
    inset = c(0, -0.1),
    horiz = TRUE,
    bty = "n",
    legend = c("Exempt", "Liable"),
    col = c(2, 1),
    pch = 16,
    xjust = 0.5
)

par(old_par)

dev.off()


## %% Feedback -----------------------

# load("Code/Products/921.1-DD.RData")
# First evidence: All industries together

reg_crp_all_inds <- wip_df %>%
    feols(
        log_mats_share ~ sw(
            i(corp,year, "Corp"),
            corp + i(corp, year, "Corp", ref2 = 83)

        )
        | sic_3,
        cluster = ~ plant + year,
        data = .
    )

reg_crp_all_inds |> etable(dict = dict)

## %% Plot All Industries ---------------------

png(
    file = "Paper/images/921-2-all-inds.png",
    width = width,
    height = height,
    # res = 300
)

# par(mfcol=c(1,2), oma=c(4,4,4,2), family = "serif", 
#     cex.main = 1.3, cex.sub = 1.1)
# par(my_par)
par(
    mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1
)

reg_crp_all_inds[[1]] |> coef() |> names()
reg_crp_all_inds[[2]] |> coef() |> names()


# par(mfcol=c(3,2), oma=c(4,4,4,2))

coefplot(
    reg_crp_all_inds[[1]],
    # keep = "Exempt",
    group = list(
        ` ` = "^^corp::Other:year::"
    ),
    dict = dict,
    col = 2,
    ylim = c(-0.075, 0.15),
    main = "Levels",
    grid = FALSE,
    value.lab = ""
)

# coefplot(
#     rg_lvl_crp,
#     keep = "Taxed",
#     group = list(
#         ` ` = "^^corp_exempt_year::Other:Taxed:"
#     ),
#     add = TRUE,
#     dict = dict,
#     col = 1,
#     x.shift = 0.2,
#     grid = FALSE,
#     value.lab = ""
# )

abline(v=3, lty=2, col="gray")

reg_crp_all_inds[[2]] |> coef() |> names()

ref_vec <- reg_crp_all_inds[[2]] |> coef() |> names() |>grep("year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp::Other:year::83"

coefplot(
    reg_crp_all_inds[[2]],
    keep = "corp::Other:year",
    group = list(
        ` ` = "^^corp::Other:year::"
    ),
    ref = ref_vec,
    dict = dict,
    col = 1,
    # x.shift = 0.2,
    # ylim = c(-0.04, 0.10),
    ylim = c(-0.075, 0.15),
    main = "Diff v. 1983",
    grid = FALSE,
    value.lab = ""
)

# ref_vec <- rg_lvl_b83_crp |> coef() |> names() |>grep("::Other:Exempt:82$",x=_, value=FALSE)
# ref_vec <- ref_vec+1
# names(ref_vec) <- "corp_exempt_y83::Other:Exempt:83"


# coefplot(
#     rg_lvl_b83_crp,
#     keep = "Other:Exempt",
#     group = list(
#         ` ` = "^^corp_exempt_y83::Other:Exempt:"
#     ),
#     ref = ref_vec,
#     add = TRUE,
#     dict = dict,
#     col = 2,
#     grid = FALSE,
#     value.lab = ""
# )

abline(v=3, lty=2, col="gray")

mtext(
    "Tax Evasion by Unincorporated Firms",
    # side = 2, 
    line = 2, outer = TRUE,
    cex = 1.5, family = "serif", font = 2
)
mtext(
    "Overall Input Overreporting Across All Industries",
    #  side = 3, 
     line = 0.6, outer = TRUE,
     cex = 1.3, family = "serif")
# mtext("Tax Evasion by Materials Overreporting.\nUnincorporated Firms in ST-Liable and ST-Exempt Industries", side = 3, line = 1, outer = TRUE)
title(
    # main = list("Tax Evasion by Unincorporated Firms", cex = 1.5), 
    # sub="Input Overreporting in Industries Exempt and Liable of ST.",
    xlab = "Year",
    ylab = "Coefficient Estimate and 95% CI",
    family = "serif",
    line = 1,
    outer = TRUE
)
# old_par <- par(fig = c(0, 1, 0, 1), new = TRUE, xpd = NA)
# plot.new()
# legend(
#     "top",
#     inset = c(0, -0.1),
#     horiz = TRUE,
#     bty = "n",
#     legend = c("Exempt", "Liable"),
#     col = c(2, 1),
#     pch = 16,
#     xjust = 0.5
# )
# par(old_par)

dev.off()

