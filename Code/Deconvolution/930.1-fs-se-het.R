# %% Load data and packages ---------------
library(tidyverse)
library(parallel)
library(fixest)
library(ggplot2)
library(tinytable)

load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/915.1-size.RData")
# source("Code/Deconvolution/021-deconv-funs.R")

## %% Define Variables and setting seed for reproducibility ----------------------

set.seed(66636)
B <- 250
size_vars <- c(
    "labour_ntile",
    "capital_ntile",
    "rev_ntile",
    "rev_l_ntile"#,
    # "rev_l2_ntile",
    # "exports_ntile",
    # "imports_ntile"
)

size_vars_lbl <- c(
    "labour" = "Labour",
    "capital" = "Capital",
    "rev" = "Revenue",
    "rev_l" = "Revenue$_{t-1}$"#,
    # "rev_l2" = "Revenue$_{t-2}$",
    # "exports" = "Exports",
    # "imports" = "Imports"
)

size_vars_lbl_plot <- c(
    "labour" = "Labour",
    "capital" = "Capital",
    "rev" = "Revenue",
    "rev_l" = "Revenue (t-1)"#,
    # "rev_l2" = "Revenue (t-2)",
    # "exports" = "Exports",
    # "imports" = "Imports"
)

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

## %% -------------------------------------

# Get all industries that have Corporations

# test_data %>%
#     dplyr::filter(
#         is.finite(log_mats_share),
#         is.finite(k),
#         is.finite(l),
#         is.finite(m),
#         is.finite(y),
#         log_mats_share > log(threshold_cut)
#     ) %>%
#     mutate(
#         corp = as.factor(ifelse(juridical_organization == 3, "Corporation", "Other"))
#     ) %>%
#     group_by(sic_3, corp) %>%
#     reframe(
#         N= unique(plant) |> length()
#     ) %>%
#     pivot_wider(
#         names_from = corp,
#         values_from = N
#     ) |> View()


## %% All industries ----------------------

fs_all_ls <- mcmapply(
    first_stage_panel_me,
    sic=unique(wip_df$sic_3),
    MoreArgs = list(
        var="log_mats_share",
        r_var = "materials",
        data=wip_df),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

names(fs_all_ls) <- unique(wip_df$sic_3)

data_all_ls <-lapply(
    unique(wip_df$sic_3),
    \(x) fs_all_ls[[as.character(x)]]$data
)

df <- wip_df %>% 
    left_join(
    do.call(
        rbind,
        data_all_ls
    ) %>% dplyr::select(sic_3, year, plant, cal_V, cal_W, epsilon),
    by = c("sic_3", "year", "plant")
)

## %% Save data ---------------------

save(
    df, fs_all_ls, data_all_ls, 
    file = "Code/Products/931.1-fs-se-het.RData"
)



