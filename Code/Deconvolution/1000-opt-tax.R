## Analyzing different measures of Sales Tax Rate and their correlation with my measure of tax evasion


## %% loading Packages and data
library(tidyverse)
library(fixest)
load("Code/Products/colombia_data.RData")
load("Code/Products/931.1-fs-se-het.RData")
# sys.source("Code/Deconvolution/021-deconv-funs.R", attach(NULL, name = "env-deconv"))
# sys.source("Code/Deconvolution/030-np-deconv-funs.R", attach(NULL, name = "env-np-deconv"))
# sys.source("Code/Deconvolution/050-render-tbls.R", attach(NULL, name = "env-render"))

## Did I trim data from outliers? 369 I got weird results when I don't
## R: I did. Check out line 37 in 911-all-inds-2.R

## %% Correlation of different ways to define sales tax rate and
# my measure of tax evasion ---------------------

# Two main tax rates I could use to recover q and kappa
# 1) sales tax rate on purchases: According to the model, this is the 
# relevant tax rate for the firm when choosing its tax evasion level 
# (in a model with two different sales tax rates: on sales and on purchases)
# 2) effective sales tax rate: I can define a single tax rate, for simplification,
# that consists of the share of the difference between sales taxes paid on sales and
# purchases over total sales. This tax rates also captures the incentives generated
# by an increase in the sales tax rate on sales, a missing feature in the other alternative

# In the data, the firms declared the sales tax paid on sales and purchases.
# If I use the the sales tax rate on purchases, I can directly estimate the 
# sales tax rate on purchases as the ratio of sales tax paid on purchases over input purchases,
# as the overreporting of purchases will cancel out in the numerator and denominator.
# If I use however the effective sales tax rate, I need to correct the ratio of taxes paid on purchases over sales
# because of the overreporting of purchases. I can do that by noting that the share of purchases over sales is
# is equal to the output elasticity of (material) purchases.

beta_df <- lapply(
    fs_all_ls,
    \(x) data.frame(sic_3 = x$sic_3 |> as.character() , beta=x$beta)
) |> bind_rows()

df1 <- df %>% left_join(beta_df, by = "sic_3") |>
    mutate(
        eff_tax_rate_c = sales_tax_rate_sales - sales_tax_rate_purchases * beta
    )

tax_regs <- feols(
    cal_V ~ sw(
        sales_tax_rate_sales,
        sales_tax_rate_purchases,
        sales_tax_rate_sales+sales_tax_rate_purchases,
        effective_sales_tax_rate,
        eff_tax_rate_c
    ) | sic_3+year,
    data = df1,
    cluster = ~plant + year
)

tax_regs |> etable()


## %% Render PNG tables from Fixest Regression etables

dict <- c(
    "sales_tax_rate_sales" = "$\\tau_{S}$",
    "sales_tax_rate_purchases" = "$\\tau_{P}$",
    "effective_sales_tax_rate" = "$\\tau_{R}$",
    "eff_tax_rate_c" = "$\\tau_{C}$",
    "cal_V" = "$\\mathcal{V}$",
    "sic_3" = "Industry (3-digit)",
    "year" = "Year"
)

tax_regs |> etable(dict = dict)

tax_regs |> 
    render_png_etbl(file_name = "1000-tax-regs", dict = dict) # no need to add extension

