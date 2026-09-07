## %% Setup ---------------------
library(tidyverse)
library(fixest)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/910-reg-results.RData") # wip_df, dict
load("Code/Products/921-DD2.RData") # sales_tax_83_group

# Colorblind-friendly palette
tol_cb_palette <- c(
  "#332288", "#76b1cf", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)
palette(tol_cb_palette)

## %% Following updated Empirical Approach ---------------------

wip_df <- wip_df %>%
    mutate(
        corp_exempt_year = NULL,
        corp_exempt_year = factor(
            ifelse(
                corp == "Corp",
                "Base",
                paste(corp, exempt_ind, year, sep = ":")
                )
        ),
        jo_exempt_year = NULL,
        jo_exempt_year = factor(
            ifelse(
                jo == "Corporation",
                "Base",
                paste(jo, exempt_ind, year, sep = ":")
                )
        ),
        corp_exempt_y83 = factor(
            ifelse(
                corp == "Corp" | year == "83",
                "Base",
                paste(corp, exempt_ind, year, sep = ":")
                )
        ),
        jo_exempt_y83 = factor(
            ifelse(
                jo == "Corporation" | year == "83",
                "Base",
                paste(jo, exempt_ind, year, sep = ":")
                )
        )
    )

## %% First Specification ---------------------

rg_lvl_crp <- feols(
    log_mats_share ~ i(corp_exempt_year, "Base") | sic_3,
    cluster = ~ plant + year,
    data = wip_df
) 

rg_lvl_crp |> etable()

rg_lvl_jo <- feols(
    log_mats_share ~ i(jo_exempt_year, "Base") | sic_3,
    cluster = ~ plant + year,
    data = wip_df
) 

rg_lvl_jo |> etable()

## %% First Specification difference vs 1983 ---------------------

rg_lvl_b83_crp <- feols(
    log_mats_share ~ corp+i(corp, exempt_ind,"Corp")+i(corp_exempt_y83, "Base") | sic_3,
    cluster = ~ plant + year,
    data = wip_df
)

rg_lvl_b83_crp |> etable()

rg_lvl_b83_jo <- feols(
    log_mats_share ~ jo+i(jo, exempt_ind,"Corporation")+i(jo_exempt_y83, "Base") | sic_3,
    cluster = ~ plant + year,
    data = wip_df
)

rg_lvl_b83_jo |> etable()

## %% Second Specification: Data ---------------------

wip_df <- wip_df %>%
    mutate(
        corp_exempt_diff_year = factor(
            ifelse(
                corp == "Corp" | exempt_ind == "Exempt",
                "Base",
                paste(corp, exempt_ind, year, sep = ":")
                )
        ),
        jo_exempt_diff_year = factor(
            ifelse(
                jo == "Corporation" | exempt_ind == "Exempt",
                "Base",
                paste(jo, exempt_ind, year, sep = ":")
                )
        ),
        corp_exempt_diff_y83 = factor(
            ifelse(
                corp == "Corp" | exempt_ind == "Exempt" | year == "83",
                "Base",
                paste(corp, exempt_ind, year, sep = ":")
                )
        ),
        jo_exempt_diff_y83 = factor(
            ifelse(
                jo == "Corporation" | exempt_ind == "Exempt" | year == "83",
                "Base",
                paste(jo, exempt_ind, year, sep = ":")
                )
        )
    )

## %% Second Specification: Regressions ---------------------

rg_sep_crp <-feols(
    log_mats_share ~ i(corp, year, "Corp") + i(corp_exempt_diff_year, "Base") | sic_3,
    cluster = ~ plant + year,
    data = wip_df
)
rg_sep_crp |> etable()

rg_sep_jo <- feols(
    log_mats_share ~ i(jo, year, "Corporation") + i(jo_exempt_diff_year, "Base") | sic_3,
    cluster = ~ plant + year,
    data = wip_df
)

rg_sep_jo |> etable()

## %% Second Specification difference vs 1983 ---------------------

rg_sep_b83_crp <- feols(
    log_mats_share ~ corp+i(corp,exempt_ind,"Corp")+i(corp, year, ref="Corp",ref2="83") + i(corp_exempt_diff_y83, "Base") | sic_3,
    cluster = ~ plant + year,
    data = wip_df
)

rg_sep_b83_crp |> etable()

rg_sep_b83_jo <- feols(
    log_mats_share ~ jo+i(jo,exempt_ind,"Corporation")+i(jo, year, ref="Corporation",ref2="83") + i(jo_exempt_diff_y83, "Base") | sic_3,
    cluster = ~ plant + year,
    data = wip_df
)

rg_sep_b83_jo |> etable()



## %% Table and Plotting ---------------------
# load("Code/Products/921.1-DD.RData")

dict[grepl("Proprietorship",dict)]<-"Prop"
dict2 <- c(
    "exempt_ind = Exempt" = "Exempt",
    "exempt_ind = Taxed" = "Taxed",
    "corp_exempt_year = Other" = "Non-Corp",
    "corp_exempt_y83 = Other" = "Non-Corp",
    "jo_exempt_year = Proprietorship" = "Prop",
    "jo_exempt_year = Ltd.Co." = "LLC",
    "jo_exempt_year = Other" = "Other",
    "jo_exempt_y83 = Other" = "Other",
    "jo_exempt_y83 = Proprietorship" = "Prop",
    "jo_exempt_y83 = Ltd.Co." = "LLC",
    "jo_exempt_diff_y83 = Other" = "Other",
    "jo_exempt_diff_y83 = Proprietorship" = "Prop",
    "jo_exempt_diff_y83 = Ltd.Co." = "LLC",
    "corp_exempt_diff_y83 = Other" = "Non-Corp",
    "corp_exempt_diff_year = Other" = "Non-Corp",
    "jo_exempt_diff_year = Other" = "Other",
    "jo_exempt_diff_year = Proprietorship" = "Prop",
    "jo_exempt_diff_year = Ltd.Co." = "LLC"
)

etable(
    rg_lvl_crp,
    rg_lvl_jo,
    # rg_sep_crp,
    # rg_sep_jo,
    rg_lvl_b83_crp,
    rg_lvl_b83_jo,
    # rg_sep_b83_crp,
    # rg_sep_b83_jo,
    # tex = TRUE,
    # file = "Paper/tables/921.1-DD.tex",
    # digits = 4,
    # signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    dict = c(dict,dict2)
)

etable(
    # rg_lvl_crp,
    # rg_lvl_jo,
    rg_sep_crp,
    rg_sep_jo,
    # rg_lvl_b83_crp,
    # rg_lvl_b83_jo,
    rg_sep_b83_crp,
    rg_sep_b83_jo,
    # tex = TRUE,
    # file = "Paper/tables/921.1-DD.tex",
    # digits = 4,
    # signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    dict = c(dict,dict2)
)
## %% Save Results ---------------------

dict <- c(dict, dict2)

save(
    rg_lvl_crp,
    rg_lvl_jo,
    rg_lvl_b83_crp,
    rg_lvl_b83_jo,
    rg_sep_crp,
    rg_sep_jo,
    rg_sep_b83_crp,
    rg_sep_b83_jo,
    dict,
    file = "Code/Products/921.1-DD.RData"
)
## %% Plots ---------------------

png(
    file = "Paper/images/921-1-joint.png",
    width = 720,
    height = 940,
    # res = 300
)

rg_lvl_crp |> coef() |> names()

# par(mfcol=c(3,2))
par(mfcol=c(3,2), oma=c(4,4,4,2))

coefplot(
    rg_lvl_crp,
    keep = "Exempt",
    group = list(
        ` ` = "^^corp_exempt_year::Other:Exempt:"
    ),
    dict = dict,
    col = 3,
    ylim = c(-0.075, 0.15),
    main = "Levels, Non-Corporation",
    grid = FALSE
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
    grid = FALSE
)

abline(v=c(3,7), lty=2, col="gray")

rg_lvl_jo|> coef() |> names()

coefplot(
    rg_lvl_jo,
    keep = "Proprietorship:Exempt",
    group = list(
        ` ` = "^^jo_exempt_year::Proprietorship:Exempt:"
    ),
    dict = dict,
    col = 3,
    ylim = c(-0.12, 0.15),
    main = "Levels, Proprietorships",
    grid = FALSE
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
    grid = FALSE
)
abline(v=c(3,7), lty=2, col="gray")

coefplot(
    rg_lvl_jo,
    keep = "Ltd. Co.:Exempt",
    group = list(
        ` ` = "^^jo_exempt_year::Ltd. Co.:Exempt:"
    ),
    dict = dict,
    col = 3,
    ylim = c(-0.10, 0.15),
    main = "Levels, LLCs",
    grid = FALSE
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
    grid = FALSE
)
abline(v=c(3,7), lty=2, col="gray")
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
    main = "v. 1983, Non-Corporation",
    grid = FALSE
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
    col = 3,
    grid = FALSE
)
abline(v=c(3,7), lty=2, col="gray")
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
    col = 3,
    ylim = c(-0.12, 0.15),
    main = "v. 1983, Proprietorships",
    grid = FALSE
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
    grid = FALSE
)
abline(v=c(3,7), lty=2, col="gray")
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
    col = 3,
    # ylim = c(-0.06, 0.12),
    ylim = c(-0.10, 0.15),
    main = "v. 1983, LLCs",
    grid = FALSE
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
    grid = FALSE
)

abline(v=c(3,7), lty=2, col="gray")

mtext("Year", side = 1, line = 2, outer = TRUE)
mtext("Coefficient", side = 2, line = 2, outer = TRUE)
mtext("First Specification; Joint Effects: Levels and Diff v. 1983", side = 3, line = 1, outer = TRUE)
old_par <- par(fig = c(0, 1, 0, 1), new = TRUE, xpd = NA)
plot.new()
legend(
    "top",
    inset = c(0, -0.04),
    horiz = TRUE,
    bty = "n",
    legend = c("Exempt", "Taxed"),
    col = c(3, 1),
    pch = 16,
    xjust = 0.5
)

par(old_par)
dev.off()

## %% Plot Sep ---------------------

png(
    file = "Paper/images/921-1-sep.png",
    width = 720,
    height = 940,
    # res = 300
)

rg_sep_crp |> coef() |> names()

# par(mfcol=c(3,2))
par(mfcol=c(3,2), oma=c(4,4,4,2))

coefplot(
    rg_sep_crp,
    keep = "Other:year",
    group = list(
        ` ` = "^^corp::Other:year::"
    ),
    dict = dict,
    col = 3,
    ylim = c(-0.075, 0.15),
    main = "Levels, Non-Corporation",
    grid = FALSE
)

coefplot(
    rg_sep_crp,
    keep = "Taxed",
    group = list(
        ` ` = "^^corp_exempt_diff_year::Other:Taxed:"
    ),
    add = TRUE,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    grid = FALSE
)

abline(v=c(3,7), lty=2, col="gray")

rg_sep_jo|> coef() |> names()

coefplot(
    rg_sep_jo,
    keep = "Proprietorship:year::",
    group = list(
        ` ` = "^^jo::Proprietorship:year::"
    ),
    dict = dict,
    col = 3,
    ylim = c(-0.15, 0.15),
    main = "Levels, Proprietorships",
    grid = FALSE
)

coefplot(
    rg_sep_jo,
    keep = "Proprietorship:Taxed",
    group = list(
        ` ` = "^^jo_exempt_diff_year::Proprietorship:Taxed:"
    ),
    add = TRUE,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    grid = FALSE
)

abline(v=c(3,7), lty=2, col="gray")

coefplot(
    rg_sep_jo,
    keep = "Ltd. Co.:year::",
    group = list(
        ` ` = "^^jo::Ltd. Co.:year::"
    ),
    dict = dict,
    col = 3,
    ylim = c(-0.10, 0.18),
    main = "Levels, LLCs",
    grid = FALSE
)

coefplot(
    rg_sep_jo,
    keep = "Ltd. Co.:Taxed",
    group = list(
        ` ` = "^^jo_exempt_diff_year::Ltd. Co.:Taxed:"
    ),
    add = TRUE,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    grid = FALSE
)

abline(v=c(3,7), lty=2, col="gray")

rg_sep_b83_crp |> coef() |> names()

ref_vec <- rg_sep_b83_crp |> coef() |> names() |>grep("::Other:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp_exempt_diff_y83::Other:Taxed:83"

coefplot(
    rg_sep_b83_crp,
    keep = "Other:Taxed",
    group = list(
        ` ` = "^^corp_exempt_diff_y83::Other:Taxed:"
    ),
    ref = ref_vec,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    # ylim = c(-0.04, 0.10),
    ylim = c(-0.075, 0.15),
    main = "v. 1983, Non-Corporation",
    grid = FALSE
)

ref_vec <- rg_sep_b83_crp |> coef() |> names() |>grep("::Other:year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "corp::Other:year::83"


coefplot(
    rg_sep_b83_crp,
    keep = "Other:year::",
    group = list(
        ` ` = "^^corp::Other:year::"
    ),
    ref = ref_vec,
    add = TRUE,
    dict = dict,
    col = 3,
    grid = FALSE
)

abline(v=c(3,7), lty=2, col="gray")

rg_sep_b83_jo|> coef() |> names()

ref_vec <- rg_sep_b83_jo |> coef() |> names() |>grep("::Proprietorship:year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo::Proprietorship:year::83"

coefplot(
    rg_sep_b83_jo,
    keep = "Proprietorship:year::",
    group = list(
        ` ` = "^^jo::Proprietorship:year::"
    ),
    dict = dict,
    ref = ref_vec,
    col = 3,
    ylim = c(-0.15, 0.15),
    main = "v. 1983, Proprietorships",
    grid = FALSE
)

ref_vec <- rg_sep_b83_jo |> coef() |> names() |>grep("::Proprietorship:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_diff_y83::Proprietorship:Taxed:83"


coefplot(
    rg_sep_b83_jo,
    keep = "Proprietorship:Taxed",
    group = list(
        ` ` = "^^jo_exempt_diff_y83::Proprietorship:Taxed:"
    ),
    add = TRUE,
    ref = ref_vec,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    grid = FALSE
)

abline(v=c(3,7), lty=2, col="gray")

ref_vec <- rg_sep_b83_jo |> coef() |> names() |>grep("::Ltd. Co.:year::82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo::Ltd. Co.:year::83"

coefplot(
    rg_sep_b83_jo,
    keep = "Ltd. Co.:year::",
    group = list(
        ` ` = "^^jo::Ltd. Co.:year::"
    ),
    dict = dict,
    ref = ref_vec,
    col = 3,
    # ylim = c(-0.06, 0.12),
    ylim = c(-0.10, 0.18),
    main = "v. 1983, LLCs",
    grid = FALSE
)

ref_vec <- rg_sep_b83_jo |> coef() |> names() |>grep("::Ltd. Co.:Taxed:82$",x=_, value=FALSE)
ref_vec <- ref_vec+1
names(ref_vec) <- "jo_exempt_diff_y83::Ltd. Co.:Taxed:83"

coefplot(
    rg_sep_b83_jo,
    keep = "Ltd. Co.:Taxed",
    group = list(
        ` ` = "^^jo_exempt_diff_y83::Ltd. Co.:Taxed:"
    ),
    add = TRUE,
    dict = dict,
    col = 1,
    x.shift = 0.2,
    ref = ref_vec,
    grid = FALSE
)

abline(v=c(3,7), lty=2, col="gray")

mtext("Year", side = 1, line = 2, outer = TRUE)
mtext("Coefficient", side = 2, line = 2, outer = TRUE)
mtext("Second Specification; Separated Effects: Levels and Diff v. 1983", side = 3, line = 1, outer = TRUE)
old_par <- par(fig = c(0, 1, 0, 1), new = TRUE, xpd = NA)
plot.new()
legend(
    "top",
    inset = c(0, -0.04),
    horiz = TRUE,
    bty = "n",
    legend = c("CIT (All)", "ST (Taxed)"),
    col = c(3, 1),
    pch = 16,
    xjust = 0.5
)
par(old_par)
dev.off()





