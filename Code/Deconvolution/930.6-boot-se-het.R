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
load("Code/Products/930-boot-se-het.RData")

df <- df %>%
    ungroup() %>%
    mutate(
        exports_ntile = ntile(share_exports, 10),
        imports_ntile = ntile(share_imports, 10),
        rev_lab_ntile = ntile(I(log_sales-l), 10),
        rev_cap_ntile = ntile(I(log_sales-k), 10)
    ) %>%
    rename(
        mats_l_ntile = mats_ntile
    )

## %% Define Variables and setting seed for reproducibility ----------------------

set.seed(66636)

fmls <- c(
    "cal_V ~ -1 + i(labour_ntile)",
    "cal_V ~ -1 + i(capital_ntile)",
    "cal_V ~ -1 + i(mats_l_ntile)",
    "cal_V ~ -1 + i(rev_l_ntile)",
    "cal_V ~ -1 + i(rev_l2_ntile)",
    "cal_V ~ -1 | rev_ntile ~ i(rev_l_ntile)",
    "cal_V ~ -1 | rev_ntile ~ i(mats_l_ntile)",
    "cal_V ~ -1 | rev_l_ntile ~ i(rev_l2_ntile)"#,
    # "cal_V ~ -1 + i(exports_ntile)",
    # "cal_V ~ -1 + i(imports_ntile)"
)

fmls_ols <- list(
    c("cal_V", "labour_ntile", "labour_ntile"),
    c("cal_V", "capital_ntile", "capital_ntile"),
    c("cal_V", "mats_l_ntile", "mats_l_ntile"),
    c("cal_V", "rev_l_ntile", "rev_l_ntile"),
    c("cal_V", "rev_l2_ntile", "rev_l2_ntile"),
    c("cal_V", "rev_ntile", "rev_l_ntile"),
    c("cal_V", "rev_ntile", "mats_l_ntile"),
    c("cal_V", "rev_l_ntile", "rev_l2_ntile")#,
    # c("cal_V", "rev_ntile", "rev_l2_ntile"),
    # c("cal_V", "exports_ntile", "exports_ntile"),
    # c("cal_V", "imports_ntile", "imports_ntile")#,
    # c("cal_V", "rev_ntile", "labour_ntile"),
    # c("cal_V", "rev_ntile", "capital_ntile"),
    # c("cal_V", "rev_ntile", "lab_lag_ntile"),
    # c("cal_V", "rev_ntile", "cap_lag_ntile")
)

fmls_ols_names <- c(
    "OLS: Labour",
    "OLS: Capital",
    "OLS: Materials (t-1)",
    "OLS: Revenue (t-1)",
    "OLS: Revenue (t-2)",
    "IV: Revenue; Revenue (t-1)",
    "IV: Revenue; Materials (t-1)",
    "IV: Revenue (t-1); Revenue (t-2)"#,
    # "OLS: Revenue/ Capital",
    # "IV: Revenue; Revenue (t-1)",
    # "OLS: Exports",
    # "OLS: Imports"#,
    # "IV: Revenue; Labour",
    # "IV: Revenue; Capital",
    # "IV: Revenue; Labour (t-1)",
    # "IV: Revenue; Capital (t-1)"
)
names(fmls_ols) <- fmls_ols_names

wong_cb_palette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#d0c536",
  "#0072B2", "#D55E00", "#CC79A7"
)

my_cols <- wong_cb_palette[1:length(fmls_ols)]
names(my_cols) <- fmls_ols_names

## OLS function 


## %% Testing 

# df %>% filter(corp == "Other") %>%
#     mutate(
#         fit_rev_ntile = predict(
#             feols(
#                 rev_ntile ~ i(rev_l_ntile),
#     )
#         ))

# feols(
#     cal_V ~ -1  | rev_ntile ~ i(rev_l_ntile),
#     cluster = ~ plant + year,
#     panel.id = ~ plant + year,
#     data = df %>% filter(corp == "Other")
# ) |> etable()

# feols(
#     cal_V ~ -1  | rev_ntile ~ 1+i(lab_lag_ntile),
#     cluster = ~ plant + year,
#     panel.id = ~ plant + year,
#     data = df %>% filter(corp == "Other")
# ) |> etable(stage =1:2)

# feols(
#     cal_V ~ -1  + rev_ntile,
#     cluster = ~ plant + year,
#     panel.id = ~ plant + year,
#     data = df %>% filter(corp == "Other",
#         !is.na(cal_V), !is.na(rev_ntile), 
#         !is.na(rev_l_ntile ))
# ) |> etable()

# lapply(
#     fmls,
#     \(x){
#         feols(
#             as.formula(x),
#             cluster = ~ plant + year,
#             panel.id = ~ plant + year,
#             data = df %>% filter(corp == "Other")
#         )
#     }
# )

OLS <- function(y,X,Z,mdl,data=df){
    df_tmp <- data %>% 
        filter(
            # corp == "Other",
            !is.na(.data[[y]]),!is.na(.data[[X]]), 
            !is.na(.data[[Z]])
        ) %>%
        select({{y}}, {{X}}, {{Z}})
    # print(df_tmp)
    X_tmp <- df_tmp %>%
        model.frame(
            as.formula(paste0('~ -1+i(',X,')')),
            data = .
        )
    Z_tmp <- df_tmp %>%
        model.frame(
            as.formula(paste0('~ -1+i(',Z,')')),
            data = .
        )
    y_tmp <- df_tmp %>%
        pull({{y}})

    beta_iv <- solve(t(as.matrix(Z_tmp)) %*% as.matrix(X_tmp)) %*% t(as.matrix(Z_tmp)) %*% y_tmp |> drop()
    return(
        data.frame(
            Coefficient = beta_iv,
            Decile = paste(1:length(beta_iv)),
            Model = mdl,
            row.names = NULL
            )
    )
}

OLS(fmls_ols[[3]][1], fmls_ols[[3]][2], fmls_ols[[3]][3],names(fmls_ols)[3], data = df)

regs_df <- names(fmls_ols) |> map(
    \(x){
            OLS(fmls_ols[[x]][1], fmls_ols[[x]][2], fmls_ols[[x]][3],x, data = df)
        }
    ) |> 
    bind_rows() %>% 
    mutate(
        Decile = factor(Decile, levels = paste(1:10)),
        Model = factor(Model, levels = names(fmls_ols))
    )
    
regs_df |> 
    ggplot(aes(x = Decile, y = Coefficient, group = Model, color = Model)) +
    facet_grid(~Model) +
    geom_line() + geom_point() + theme_minimal() + labs(title = "OLS Coefficients by Decile and Model")


# stop("End of testing")

## %% Bootstrap standard errors

reg_df_boot_ls <- mclapply(
    1:B,
    \(i){
        resampled_data <- resample_by_group(df, sic_3)

        regs_df <- names(fmls_ols) |> map(
            \(x){
                OLS(fmls_ols[[x]][1], fmls_ols[[x]][2], fmls_ols[[x]][3],x, data = resampled_data)
            }
            ) |> 
            bind_rows() %>%
            mutate(
                Decile = factor(Decile, levels = paste(1:10)),
                Model = factor(Model, levels = names(fmls_ols))
            )

        if(i %% 20 == 0){cat("Done with bootstrap replicate:", i, "\n")}
        return(regs_df)
        
    }
)


## %% Save bootstrap results

save(
    reg_df_boot_ls, regs_df,
    file = "Code/Products/930.6-boot-se-het.RData"
)

## %% Tables and Plots 
# stop("End of bootstrap")

load("Code/Products/930.6-boot-se-het.RData")

dec_aux_df <- bind_rows(reg_df_boot_ls, .id = "Bootstrap_Replicate") %>%
    mutate(
        Bootstrap_Replicate = as.integer(Bootstrap_Replicate)
        ) %>%
    left_join(
        regs_df,
        by = c("Model", "Decile"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        coef_r = Coefficient - Coefficient.t0
    ) %>%
    group_by(Model, Decile) %>%
    reframe(
        probs = c(0.975, .025),
        q_val = quantile(coef_r, probs = probs),
        CI = c("LCI", "UCI"),
        CI_val = first(Coefficient.t0) - q_val,
        Coefficient = first(Coefficient.t0),
        p_x = ecdf(coef_r)(first(Coefficient.t0)),
        p_val = 2*min(p_x, 1-p_x),
        Decile = first(Decile)
    ) %>%
    pivot_wider(
        id_cols = c(Model, Decile, Coefficient, p_val),
        names_from = CI,
        values_from = CI_val
    )

## %% Rendering table

make_tbl <- function(x, x_desc, order_vec, aux_df){
    tmp_tbl <- aux_df %>%
        mutate(
            stars = case_when(
                p_val < 0.01 ~ "***",
                p_val < 0.05 ~ "**",
                p_val < 0.1 ~ "*",
                .default = ""
            ),
            x_coef = glue::glue("{round(.data[[x]], 4)}{stars}"),
            CI_coef = glue::glue("[{round(LCI, 4)}, {round(UCI, 4)}]"),
            # name_coef = factor(.data[[x_desc]], levels = order_vec)
        ) %>%
        select(Model, Decile, x_coef, CI_coef) %>%
        pivot_longer(
            cols = c(x_coef, CI_coef),
            names_to = "type",
            values_to = "value",
            names_pattern = "(.*)_coef"
        ) %>%
        mutate(
            type = factor(type, levels = c("x", "CI"))
        ) %>%
        pivot_wider(
            names_from = Model,
            values_from = value,
            values_fill = "",
        ) %>%
        arrange(Decile, type)# %>%
        # rename(
        #     ` ` = name_coef
        # )
    return(tmp_tbl[-2])
}

make_tbl(
    x = "Coefficient",
    x_desc = "Decile",
    order_vec = order_vec,
    aux_df = dec_aux_df
) |> View()


render_png_tbl <- function(tbl_in, file_name, out_dir = "Paper/tbls"){
    tmp_tex <- tempfile(fileext = ".tex", tmpdir = out_dir)
    on.exit(unlink(tmp_tex), add = TRUE)
    print(tmp_tex)

    tbl_in |>
        tt() |>
        style_tt(
            i = seq(1,nrow(tbl_in)-1, by=2),
            j = 1,
            rowspan = 2,
            alignv = "t"
        ) |>
        save_tt(
            output = tmp_tex,
            overwrite = TRUE
        )

    tex_lines <- readLines(tmp_tex)

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
    cat(
        preamble,
        "\\begin{document}",
        tex_lines,
        "\\end{document}",
        file = tmp_tex,
        sep = "\n"
    )
    tmp_sh <- tempfile(fileext = ".sh")
    on.exit(unlink(tmp_sh), add = TRUE)
    cat(
        "#!/bin/zsh",
        "export PATH=\"/Library/TeX/texbin:$PATH\"",
        "export PATH=\"/usr/local/bin:$PATH\"",
        "echo $PATH",
        paste0("'/Library/TeX/texbin/pdflatex' -synctex=1 -interaction=nonstopmode -file-line-error -recorder -output-directory=", out_dir, " '", tmp_tex,"'"),
        paste("mv", gsub("\\.tex", ".pdf", tmp_tex), paste0(out_dir,"/", file_name, ".pdf")),
        paste("'/usr/local/bin/magick' -density 300", paste0(out_dir,"/", file_name, ".pdf"),paste0(out_dir,"/", file_name, ".png")),
        sep = "\n",
        file = tmp_sh
    )
    system(paste("chmod +x", tmp_sh), intern = TRUE)
    system2(tmp_sh, wait = TRUE)

}

## %% Render table as PNG


dec_aux_df %>% 
    filter(
        # Model %in% c("OLS: Labour", "OLS: Capital"),
        Model %in% grep("OLS", fmls_ols_names, value = TRUE)
    ) |>
    make_tbl(
        x = "Coefficient",
        x_desc = "Decile",
        order_vec = order_vec,
        aux_df = _
    ) |>
    render_png_tbl(
        file_name = "930.6-dec-boot-se-1"
    )

dec_aux_df %>% 
    filter(
        # Model %in% c("IV: Revenue; Revenue (t-1)", "OLS: Revenue (t-1)"),
        Model %in% grep("IV", fmls_ols_names, value = TRUE)
    ) |>
    make_tbl(
        x = "Coefficient",
        x_desc = "Decile",
        order_vec = order_vec,
        aux_df = _
    ) |>
    rename(
        `IV: Revenue; Revenue$_{t-1}$` = `IV: Revenue; Revenue (t-1)`,
        `OLS: Revenue$_{t-1}$` = `OLS: Revenue (t-1)`
     ) |>
    render_png_tbl(
        file_name = "930.6-dec-boot-se-2"
    )

# dec_aux_df %>% 
#     filter(
#         Model %in% c("OLS: Exports", "OLS: Imports")
#     ) |>
#     make_tbl(
#         x = "Coefficient",
#         x_desc = "Decile",
#         order_vec = order_vec,
#         aux_df = _
#     ) |>
#     render_png_tbl(
#         file_name = "930.6-dec-boot-se-3"
#     )


dec_aux_df %>%
    filter(
        Model %in% setdiff(fmls_ols_names,c("OLS: Exports", "OLS: Imports"))
    ) |>
    make_tbl(
        x = "Coefficient",
        x_desc = "Decile",
        order_vec = order_vec,
        aux_df = _
    ) |>
    rename(
        `IV: Revenue; Revenue$_{t-1}$` = "IV: Revenue; Revenue (t-1)",
        `OLS: Revenue$_{t-1}$` = "OLS: Revenue (t-1)"
     ) |>
    render_png_tbl(
        file_name = "930.6-dec-boot-se"
    )

## %% Plotting 

dec_aux_df %>% 
    filter(
        Model %in% c("OLS: Labour", "OLS: Capital")
    ) |>
    ggplot(aes(x = Decile, y = Coefficient, group = Model, color = Model)) +
    facet_grid(~Model, axes = "all") +
    # geom_line() +
    geom_point() + 
    geom_errorbar(
        aes(ymin = LCI, ymax = UCI), width = 0.2
    ) +
    geom_hline(yintercept = 0, color = "lightgray") +
    labs(
        title = "Average Tax Evasion by Size Decile",
        subtitle = "Coefficient and 95% Bootstrap CI with 250 replications",
        y = ""
    ) +
    theme_minimal() +
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
    scale_color_manual(values = my_cols)

ggsave(
    "930.6-boot-se-se-plot.png",
    path = "Paper/images/",
    width = 10.1, height = 6.2, units = "in"
)


dec_aux_df %>% 
    filter(
        # Model %in% c("IV: Revenue; Revenue (t-1)", "OLS: Revenue (t-1)"),
        Model %in% grep("IV", fmls_ols_names, value = TRUE)
    ) |>
    ggplot(aes(x = Decile, y = Coefficient, group = Model, color = Model)) +
    facet_grid(~Model, axes = "all") +
    geom_point() + 
    geom_errorbar(
        aes(ymin = LCI, ymax = UCI), width = 0.2
    ) +
    geom_hline(yintercept = 0, color = "lightgray") +
    labs(
        title = "Average Tax Evasion by Size Decile",
        subtitle = "Coefficient and 95% Bootstrap CI with 250 replications",
        y = ""
    ) +
    theme_minimal() +
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
    scale_color_manual(values = my_cols)

ggsave(
    "930.6-boot-se-se-plot-1.png",
    path = "Paper/images/",
    width = 10.1, height = 6.2, units = "in"
)

dec_aux_df %>% 
    filter(
        # Model %in% c("OLS: Exports", "OLS: Imports"),
        Model %in% grep("OLS", fmls_ols_names, value = TRUE)
    ) |>
    ggplot(aes(x = Decile, y = Coefficient, group = Model, color = Model)) +
    facet_grid(~Model, axes = "all") +
    geom_point() + 
    geom_errorbar(
        aes(ymin = LCI, ymax = UCI), width = 0.2
    ) +
    geom_hline(yintercept = 0, color = "lightgray") +
    labs(
        title = "Average Tax Evasion by Size Decile",
        subtitle = "Coefficient and 95% Bootstrap CI with 250 replications",
        y = ""
    ) +
    theme_minimal() +
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
    scale_color_manual(values = my_cols)

ggsave(
    "930.6-boot-se-se-plot-2.png",
    path = "Paper/images/",
    width = 10.1, height = 6.2, units = "in"
)


