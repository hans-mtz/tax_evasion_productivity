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

test_data %>%
    dplyr::filter(
        is.finite(log_mats_share),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        is.finite(y),
        log_mats_share > log(threshold_cut)
    ) %>%
    mutate(
        corp = as.factor(ifelse(juridical_organization == 3, "Corporation", "Other"))
    ) %>%
    group_by(sic_3, corp) %>%
    reframe(
        N= unique(plant) |> length()
    ) %>%
    pivot_wider(
        names_from = corp,
        values_from = N
    ) |> View()


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

## %% Regressions ----------------------

## BUG: Fixest keeps adding the constant in the regression
# reg_sh_deciles <- feols(
#     cal_V ~ sw(
#         i(corp, i.labour_ntile, ref = "Corp"),
#         i(corp,i.capital_ntile, ref="Corp"),
#         i(corp,i.rev_ntile, ref="Corp") + rev_ntile,
#         i(corp,i.rev_l_ntile, ref="Corp"),
#         i(corp,i.rev_l2_ntile, ref="Corp"),
#         i(corp,i.exports_ntile, ref="Corp"),
#         i(corp,i.imports_ntile, ref="Corp")
#     ),
#   cluster = ~ plant + year,
#   panel.id = ~ plant + year,
#   data = df
#     )

## FEOLS works when I do not use the sw() function

feols(
    cal_V ~ -1 + i(corp,i.rev_ntile, ref="Corp") + rev_ntile,
    cluster = ~ plant + year,
    panel.id = ~ plant + year,
    data = df
)

fmls <- c(
    "cal_V ~ -1 + i(corp,i.labour_ntile, ref = 'Corp')",
    "cal_V ~ -1 + i(corp,i.capital_ntile, ref='Corp')",
    "cal_V ~ -1 + i(corp,i.rev_ntile, ref='Corp')",
    "cal_V ~ -1 + i(corp,i.rev_l_ntile, ref='Corp')"#,
    # "cal_V ~ -1 + i(corp,i.rev_l2_ntile, ref='Corp')",
    # "cal_V ~ -1 + i(corp,i.exports_ntile, ref='Corp')",
    # "cal_V ~ -1 + i(corp,i.imports_ntile, ref='Corp')"
)


## This works
reg_ls <- lapply(
    fmls,
    \(x){
        feols(
            as.formula(x),
            cluster = ~ plant + year,
            panel.id = ~ plant + year,
            data = df
        )
    }
)

reg_ls |> etable() 

coef_df_ls <- lapply(
    seq_along(reg_ls),
    \(x) {
        tmp<-reg_ls[[x]] |> coef()
        data.frame(
            var = names(tmp),
            coef = tmp
        )
    }
)

coef_df_t0 <- do.call(rbind, coef_df_ls)

## %% Alternatively, I can do it manually, but i got different results when
# trying to include the set of dummies that are not interacted with D^N



df %>%
    group_by(rev_ntile, corp) %>%
    filter(corp == "Other") %>%
    summarise(
        mean_cal_V = mean(cal_V, na.rm=TRUE)
    )

lapply(
    size_vars,
    \(x) {
        df %>%
        filter(
           !is.na(.data[[x]])
        ) %>%
        group_by(.data[[x]]) %>%
        filter(corp == "Other") %>%
        summarise(
            mean_cal_V = mean(cal_V, na.rm=TRUE)
        )
    }
)


## %% Boostraping SEs ----------------------

lean_df <- wip_df %>%
    dplyr::select(
        sic_3, year, plant, log_mats_share, k, l, m, y,
        log_sales, lag_log_sales, materials,
        corp, metro_area_code, section_country_code, juridical_organization,
    )

fs_all_boot_ls <- mclapply(
    1:B,
    function(i){

        resampled_data <- resample_by_group(lean_df,sic_3)

        # unique(resampled_data$sic_3) |> print()

        fs_all_tmp_ls <- mapply(
            first_stage_panel_me,
            sic=unique(resampled_data$sic_3),
            MoreArgs = list(
                var="log_mats_share",
                r_var = "materials",
                data=resampled_data),
            SIMPLIFY = FALSE
        )

        names(fs_all_tmp_ls) <- unique(resampled_data$sic_3)

        data_all_tmp_ls <-lapply(
            unique(resampled_data$sic_3),
            \(x) fs_all_tmp_ls[[as.character(x)]]$data
            )

        tmp_df <- resampled_data %>%
            left_join(
                do.call(
                    rbind,
                    data_all_tmp_ls
                ) %>% dplyr::select(sic_3, year, plant, cal_V, cal_W, epsilon),
                by = c("sic_3", "year", "plant")
            )

        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}

        return(tmp_df)
    },
    mc.cores = mc_cores
)

fs_all_boot_ls[[1]]
gc()
## %% Save bootstrapped data ---------------------

save(
    df, fs_all_boot_ls,
    file = "Code/Products/930-boot-se-het.RData"
)

## %% Estimate SE with bootstrapped data ---------------------


load("Code/Products/930-boot-se-het.RData")

fs_all_boot_ls[[1]] %>%
            ungroup() %>%
            mutate(
                labour_ntile = ntile(l, 10),
                capital_ntile = ntile(k, 10),
                rev_ntile = ntile(log_sales, 10),
                rev_l_ntile = ntile(lag_log_sales, 10)
            )

coef_df_boot_ls <- mclapply(
    1:3,
    \(i){

        # resampled_data <- resample_by_group(df,sic_3)

        resampled_data <- fs_all_boot_ls[[i]] %>%
            ungroup() %>%
            mutate(
                labour_ntile = ntile(l, 10),
                capital_ntile = ntile(k, 10),
                rev_ntile = ntile(log_sales, 10),
                rev_l_ntile = ntile(lag_log_sales, 10)
            )
        ## This works
        tmp_ls <- lapply(
            fmls,
            \(x){
                feols(
                    as.formula(x),
                    cluster = ~ plant + year,
                    panel.id = ~ plant + year,
                    data = resampled_data
                )
            }
        )

        tmp_df_ls <- lapply(
            seq_along(tmp_ls),
            \(x) {
                tmp<-tmp_ls[[x]] |> coef()
                data.frame(
                    var = names(tmp),
                    coef = tmp
                )
            }
        )

        df_out <- do.call(rbind, tmp_df_ls)
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(df_out)
    },
    mc.cores = mc_cores
)

coef_df_boot_ls[[1]]

## %% Save bootstrapped data ---------------------

save(
    df, fs_all_boot_ls, coef_df_boot_ls, coef_df_t0,
    file = "Code/Products/930-boot-se-het.RData"
)

## %% Results table with bootstrapped SEs------------

res_df <- do.call(
    rbind,
    coef_df_boot_ls
    ) %>%
    as.tibble() %>%
    left_join(
        coef_df_t0,
        by = "var",
        suffix = c("", ".t0")
    ) %>%
    mutate(
        bc_coef = coef-coef.t0
    ) %>%
    group_by(var) %>%
    reframe(
        val_coef = quantile(bc_coef, c(0.975, 0.025)),
        # val_coef = quantile(bc_coef, c(0.95, 0.05)),
        probs = c(0.975, 0.025),
        CI = c("LCI", "UCI"),
        CI_coef = max(coef.t0)- val_coef,
        coef_0 = max(coef.t0),
        # coeff_se = 2*max(se.t0)-mean(se),
        prob = ecdf(bc_coef)(max(coef.t0)),
        p_val = 2*min(
            # prob >= 0.5,
            1 - prob,
            prob
        )
    ) %>%
    pivot_wider(
        id_cols = c(var, coef_0, p_val),
        names_from = CI,
        values_from = CI_coef
    )

res_df %>% View()

res_tbl <- res_df %>%
    mutate(
        stars = case_when(
            p_val <= 0.01 ~ "***",
            p_val <= 0.05 ~ "**",
            p_val <= 0.1 ~ "*",
            TRUE ~ ""
        ),
        CI_coef = glue::glue("[{round(LCI, 4)}, {round(UCI, 4)}]"),
        coeff_coef= glue::glue("{round(coef_0, 4)}{stars}"),
        pval_coef = glue::glue("{round(p_val, 4)}"),
        size = gsub("corp::Other:(.*)_ntile:*.*", "\\1", var),
        Decile = gsub(".*_ntile:*([0-9]*)$", "\\1", var) |> as.numeric()
    ) %>%
    filter(
        size %in% c("labour", "capital", "rev_l", "rev_l2","exports", "imports")
    ) %>%
    mutate(
        size_desc = size_vars_lbl[size],
        size_desc = factor(size_desc, levels = size_vars_lbl[-3])
    ) %>%
    select(size_desc, Decile, coeff_coef, CI_coef) %>%
     ungroup() %>%
     arrange(size_desc, Decile) %>%
     pivot_longer(
        cols = c(coeff_coef, CI_coef),
        names_to = "type",
        values_to = "val",
        names_pattern = "(.*)_.*"
     ) %>%
    pivot_wider(
        names_from = size_desc,
        values_from = val
    )


res_tbl %>% View()

preamble <- "
\\documentclass{standalone}
\\usepackage{xcolor}
\\usepackage{tabularray}
\\UseTblrLibrary{booktabs}
\\UseTblrLibrary{rotating}
\\UseTblrLibrary{siunitx}
\\usepackage{float}
\\usepackage{graphicx}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\UseTblrLibrary{siunitx}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}
"


tt(res_tbl[,-2]) |>
    style_tt(
        i = c(1,3,5,7,9,11,13,15,17,19), j = 1,
        rowspan = 2, alignv = "t"
    ) |>
    save_tt(
        output = "Paper/images/930.1-boot-se-het-table.tex",
        overwrite = TRUE
    )

tex_tbl <- readLines("Paper/images/930.1-boot-se-het-table.tex")

cat(
    preamble,
    "\\begin{document}",
    tex_tbl,
    "\\end{document}",
    file = "Paper/tbls/930.1-boot-se-het-table.tex",
    sep = "\n"
)

## %% saving results ---------------------

save(
    df, fs_all_boot_ls, coef_df_boot_ls, coef_df_t0,
    res_df, res_tbl,
    file = "Code/Products/930-boot-se-het.RData"
)

## Plotting results ----------------------

# load("Code/Products/930-boot-se-het.RData")

# Plotting coefficients with confidence intervals
# x-axis: deciles, y-axis: coefficients, 
# a different plot with different color for each size variable

plot_df <- res_df %>%
    mutate(
        stars = case_when(
            p_val <= 0.01 ~ "***",
            p_val <= 0.05 ~ "**",
            p_val <= 0.1 ~ "*",
            TRUE ~ ""
        ),
        CI_coef = glue::glue("[{round(LCI, 4)}, {round(UCI, 4)}]"),
        coeff_coef= glue::glue("{round(coef_0, 4)}{stars}"),
        pval_coef = glue::glue("{round(p_val, 4)}"),
        size = gsub("corp::Other:(.*)_ntile:*.*", "\\1", var),
        decile = gsub(".*_ntile:*([0-9]*)$", "\\1", var) |> as.numeric()
    ) %>%
    mutate(
        size_desc = size_vars_lbl_plot[size]
    ) %>%
    ungroup() %>%
    arrange(size_desc, decile)
    
plot_df %>%
    filter(
        # size %in% c("labour", "capital", "rev_l", "rev_l2", "imports", "exports")
        size %in% c("labour", "capital")
    ) %>%
    ggplot(aes(x = as.factor(decile), y = coef_0, color = size_desc)) +
        geom_point() +
        geom_errorbar(aes(ymin = LCI,
                        ymax = UCI),
                    width = 0.2) +
        geom_hline(yintercept = 0, color = "lightgray")+
        theme_minimal() +
        facet_grid(cols=vars(size_desc), scales = "free", axes = "all") +
        labs(x = "Decile", y = "Coefficient", color = "Size Variable") +
        ggtitle(
            "Average Tax Evasion by Size Deciles", 
            subtitle = "95% Bootstrap CI with 250 replications"
        ) +
        theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.text = element_text(size = 12, face = "bold", family = "Times"),
            axis.line = element_line(color = "black", linetype = "solid"),
            axis.text = element_text(family = "Times", size = 10),
            axis.title = element_text(family = "Times", size = 12, face = "bold"),
            plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(family = "Times", size = 12, hjust = 0.5)
        ) +
        scale_color_manual(values = wong_cb_palette[1:2])

ggsave(
    "930-boot-se-size-plot.png",
    path = "Paper/images/",
    width = 10.1, height = 6.2, units = "in"
)


plot_df %>%
    filter(
        # size %in% c("labour", "capital", "rev_l", "rev_l2", "imports", "exports")
        size %in% c("rev", "rev_l")
    ) %>%
    ggplot(aes(x = as.factor(decile), y = coef_0, color = size_desc)) +
        geom_point() +
        geom_errorbar(aes(ymin = LCI,
                        ymax = UCI),
                    width = 0.2) +
        geom_hline(yintercept = 0, color = "lightgray")+
        theme_minimal() +
        facet_grid(cols=vars(size_desc), scales = "free", axes = "all") +
        labs(x = "Decile", y = "Coefficient", color = "Size Variable") +
        ggtitle(
            "Average Tax Evasion by Size Deciles", 
            subtitle = "95% Bootstrap CI with 250 replications"
        ) +
        theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.text = element_text(size = 12, face = "bold", family = "Times"),
            axis.line = element_line(color = "black", linetype = "solid"),
            axis.text = element_text(family = "Times", size = 10),
            axis.title = element_text(family = "Times", size = 12, face = "bold"),
            plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(family = "Times", size = 12, hjust = 0.5)#,
            # panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5)
        ) +
        scale_color_manual(values = wong_cb_palette[3:4])

ggsave(
    "930-boot-se-size-plot-1.png",
    path = "Paper/images/",
    width = 10.1, height = 6.2, units = "in"
)

# plot_df %>%
#     filter(
#         # size %in% c("labour", "capital", "rev_l", "rev_l2", "imports", "exports")
#         size %in% c("imports", "exports")
#     ) %>%
#     ggplot(aes(x = as.factor(decile), y = coef_0, color = size_desc)) +
#         geom_point() +
#         geom_errorbar(aes(ymin = LCI,
#                         ymax = UCI),
#                     width = 0.2) +
#         geom_hline(yintercept = 0, color = "lightgray")+
#         theme_minimal() +
#         facet_grid(cols=vars(size_desc), scales = "free", axes = "all") +
#         labs(x = "Decile", y = "Coefficient", color = "Size Variable") +
#         ggtitle(
#             "Average Tax Evasion by Size Deciles", 
#             subtitle = "95% Bootstrap CI with 250 replications"
#         ) +
#         theme(
#             legend.position = "none",
#             panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank(),
#             strip.text = element_text(size = 12, face = "bold", family = "Times"),
#             axis.line = element_line(color = "black", linetype = "solid"),
#             axis.text = element_text(family = "Times", size = 10),
#             axis.title = element_text(family = "Times", size = 12, face = "bold"),
#             plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5),
#             plot.subtitle = element_text(family = "Times", size = 12, hjust = 0.5)#,
#             # panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5)
#         ) +
#         scale_color_manual(values = wong_cb_palette[5:6])

# ggsave(
#     "930-boot-se-size-plot-2.png",
#     path = "Paper/images/",
#     width = 10.1, height = 6.2, units = "in"
# )


# test_data %>%
#         dplyr::filter(
#             is.finite(log_mats_share),
#             is.finite(k),
#             is.finite(l),
#             is.finite(m),
#             is.finite(y),
#             log_mats_share > log(threshold_cut)
#         ) %>%
#         ungroup() %>%
#         mutate(
#             Corp = ifelse(juridical_organization==3,"Corp","Other")
#         ) %>%
#         group_by(sic_3, Corp) %>%
#         reframe(
#             plant = sample(unique(plant), size = unique(plant) |> length(), replace = TRUE)
#         ) %>% 
#         group_by(sic_3, Corp) %>%
#         summarise(
#             N = n()
#         ) %>%
#         pivot_wider(
#             names_from = Corp,
#             values_from = N
#         ) |> View()
