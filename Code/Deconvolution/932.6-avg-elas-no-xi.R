## %% Packages and Data -------------------
library(tidyverse)
library(parallel)
library(fixest)
library(ggplot2)
library(tinytable)

load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/930-boot-se-het.RData")
load("Code/Products/931-boot-se-marg.RData")

## %% Defining vars ----------------------------------

set.seed(66636)

mdl_names <- c(
    "IV: Revenue; Revenue$_{t-1}$",
    # "IV: Revenue; Capital$_{t-1}$",
    # "IV: Revenue; Labour$_{t-1}$",
    "OLS: Revenue$_{t-1}$"
)

fmls_nc <- c(
    "cal_V ~ (l_pct+k_pct)^2 + l_pct^2 + k_pct^2 | rev_pct + rev_pct^2 +(l_pct+k_pct):rev_pct~ lag_rev_pct + lag_rev_pct^2 +(l_pct+k_pct):lag_rev_pct",
    # "cal_V ~ (l_pct+k_pct)^2 + l_pct^2 + k_pct^2 | rev_pct + rev_pct^2 +(l_pct+k_pct):rev_pct~ cap_l_pct + cap_l_pct^2 +(l_pct+k_pct):cap_l_pct",
    # "cal_V ~ (l_pct+k_pct)^2 + l_pct^2 + k_pct^2 | rev_pct + rev_pct^2 +(l_pct+k_pct):rev_pct~ lab_l_pct + lab_l_pct^2 +(l_pct+k_pct):lab_l_pct",
    "cal_V ~ (lag_rev_pct+l_pct+k_pct)^2 + lag_rev_pct^2 + l_pct^2 + k_pct^2 "
)

names(fmls_nc) <- mdl_names

var_elas_iv <- c(
    "Revenue",
    "Labour",
    "Capital"
)
var_elas_ols <- c(
    "Revenue (t-1)",
    "Labour",
    "Capital"
)
var_elas <- c(
    "Revenue",
    "Revenue (t-1)",
    "Labour",
    "Capital"
)

union(var_elas_iv, var_elas_ols)
fmls_elas_iv <- c(
    "~ I(2*fit_rev_pct)+l_pct + k_pct ",
    "~ I(2*l_pct)+fit_rev_pct + k_pct ",
    "~ I(2*k_pct)+fit_rev_pct + l_pct "
)
names(fmls_elas_iv) <- var_elas_iv
fmls_elas_ols <- c(
    "~ I(2*lag_rev_pct)+l_pct + k_pct",
    "~ I(2*l_pct)+lag_rev_pct + k_pct",
    "~ I(2*k_pct)+lag_rev_pct + l_pct"
)
names(fmls_elas_ols) <- var_elas_ols

coef_elas_iv <- list(
    "fit_rev_pct" = c("fit_rev_pct", "fit_I(rev_pct^2)","fit_rev_pct:l_pct", "fit_rev_pct:k_pct"),
    "l_pct" = c("l_pct", "I(l_pct^2)", "fit_rev_pct:l_pct", "l_pct:k_pct"),
    "k_pct" = c("k_pct", "I(k_pct^2)","fit_rev_pct:k_pct", "l_pct:k_pct")
)
names(coef_elas_iv) <- var_elas_iv

coef_elas_ols <- list(
    "lag_rev_pct" = c("lag_rev_pct", "I(lag_rev_pct^2)","lag_rev_pct:l_pct", "lag_rev_pct:k_pct"),
    "l_pct" = c("l_pct", "I(l_pct^2)", "lag_rev_pct:l_pct", "l_pct:k_pct"),
    "k_pct" = c("k_pct", "I(k_pct^2)","lag_rev_pct:k_pct", "l_pct:k_pct")
)
names(coef_elas_ols) <- var_elas_ols

size_var_mdl <- c(
    "fit_rev_pct",
    "fit_I(rev_pct^2)",
    "lag_rev_pct",
    "I(lag_rev_pct^2)",
    "l_pct",
    "I(l_pct^2)",
    "k_pct",
    "I(k_pct^2)",
    "fit_rev_pct:l_pct",
    "fit_rev_pct:k_pct",
    "lag_rev_pct:l_pct",
    "lag_rev_pct:k_pct",
    "l_pct:k_pct",
    "(Intercept)"
)
var_desc_ltx <- c(
    "Revenue",
    "Revenue$^2$",
    "Revenue$_{t-1}$",
    "Revenue$^2_{t-1}$",
    "Labour",
    "Labour$^2$",
    "Capital",
    "Capital$^2$",
    "Revenue $\\times$ Labour",
    "Revenue $\\times$ Capital",
    "Revenue$_{t-1}$ $\\times$ Labour",
    "Revenue$_{t-1}$ $\\times$ Capital",
    "Labour $\\times$ Capital",
    "Const."
)
names(var_desc_ltx) <- size_var_mdl
var_desc_plt <- c(
    "Revenue",
    "Revenue Sq.",
    "Revenue (t-1)",
    "Revenue (t-1) Sq.",
    "Labour",
    "Labour Sq.",
    "Capital",
    "Capital Sq.",
    "Revenue x Labour",
    "Revenue x Capital",
    "Revenue (t-1) x Labour",
    "Revenue (t-1) x Capital",
    "Labour x Capital",
    "Const."
)
names(var_desc_plt) <- size_var_mdl

var_desc_ltx_elas <- var_desc_ltx
names(var_desc_ltx_elas) <- var_desc_plt

var_desc_plt_coef <- var_desc_plt
names(var_desc_plt_coef) <- var_desc_ltx

comb_vars <- data.frame(
    size_v = c(var_elas_iv, var_elas_ols),
    mdl = c(rep("IV: Revenue; Revenue$_{t-1}$", length(var_elas_iv)),
            rep("OLS: Revenue$_{t-1}$", length(var_elas_ols))
    )
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

## %% Estimating models on full sample ----------------------

df %>%
    ungroup() %>%
    mutate(
        # exports_ntile = ntile(share_exports, 10),
        # imports_ntile = ntile(share_imports, 10),
        exports_pct = percent_rank(share_exports),
        imports_pct = percent_rank(share_imports)
    )

reg_mrg_ls <- lapply(
    fmls_nc,
    \(x){
        df %>% filter(
            corp == "Other"
        ) %>%
        feols(
            as.formula(x),
            cluster = ~ plant + year,
            panel.id = ~ plant + year,
            data = .
        )
    }
)

reg_mrg_ls |> etable()

coef_df_mrg_ls <- lapply(
    seq_along(reg_mrg_ls),
    \(x) {
        tmp<-reg_mrg_ls[[x]] |> coef()

        data.frame(
            var = names(tmp),
            coef = tmp,
            model = names(fmls_nc)[x],
            row.names = NULL
        )
    }
)

coef_df_mrg_t0 <- do.call(rbind, coef_df_mrg_ls)
coef_df_mrg_t0 |> mutate(
    var_desc = var_desc_ltx[var]
) |> View()

## %% Estimating Average Size Elasticities of Tax Evasion ----------------------


## I need to estimate E[dcal_V/dX_i]=E[de/dX_i+depsilon/dX_i]=E[de/dX_i]=E[mu_Xi+mu2_Xi*2X_i + sum_j Z_j*mu_Zj] where Z_j are the interaction terms of X_i with other covariates, and mu_Xi, mu2_Xi, mu_Zj are the coefficients of X_i, X_i^2, and the interaction terms of X_i with other covariates in the regression model.
## I also need to estimate the CI of this elasticity using bootstrap empirical distribution

## OLS model: simplest 
# For each X (Revenue, Labour, Capital, Imports, Exports):
# 1) Get coefficients
# 2) Get fitted values of elasticity at each observation: de/dX = mu_Xi+mu2_Xi*2X_i + sum_j Z_j*mu_Zj 
# 3) Compute elasticity at mean: E[\hat{de/dX }]

## IV model: For exogenous vars, same as OLS
# for endogenous var (Revenue), I need to get the fitted values \hat{X_i}
# of the first stage regression, and then compute the elasticity 
# For revenue,
# 1) Get predictions of first stage regression and add to data
# 2) Get coefficients of second stage regression
# 3) Get fitted values of elasticity at each observation: de/dX = mu_Xi+mu2_Xi*2\hat{X_i} + sum_j Z_j*mu_Zj
# 3) Get mean of fitted values of elasticity: E[\hat{de/dX }]


## Defining function to compute average elasticity

compute_avg_elas <- function(size_v, mdl, reg_ls, dta, verbose=FALSE){
    tmp_dta <- dta %>% filter(corp == "Other")
    coefs_tmp <- reg_ls[[mdl]] |> coef()
    coef_elas_tmp <- if(mdl == "OLS: Revenue$_{t-1}$"){
        coef_elas_ols[[size_v]]
    } else {
        coef_elas_iv[[size_v]]
    }
    if (verbose) print(coefs_tmp[coef_elas_tmp])
    if(any(is.na(coefs_tmp[coef_elas_tmp]))){
        stop(paste("Coefficients for", size_v, "not found in model", mdl))
    }
    fml_elas_tmp <- ifelse(mdl == "OLS: Revenue$_{t-1}$",
        fmls_elas_ols[[size_v]],
        fmls_elas_iv[[size_v]]
    )
    if (verbose) print(fml_elas_tmp)
    if (mdl != "OLS: Revenue$_{t-1}$") {
        tmp_dta$fit_rev_pct <- reg_ls[[mdl]]$iv_first_stage$`rev_pct` |> predict(sample="original")
    }
    avg_elas <- model.matrix(
        fml_elas_tmp |> as.formula(),
        data = tmp_dta
    ) %*% coefs_tmp[coef_elas_tmp] |> mean()

    # mdl_df <- model.frame(
    #     fml_elas_tmp |> as.formula(),
    #     data = tmp_dta,
    #     na.action = na.exclude
    # ) 

    # print(str(mdl_df))
    # tmp_dta$elas_i <- as.matrix(mdl_df) %*% coefs_tmp[coef_elas_tmp] |> napredict(mdl_df, x=_)
    
    # tmp_dta |> summarise(
    #     Mean = mean(elas_i, na.rm = TRUE),
    # ) |> print()

    return(
        tibble(
            elasticity = avg_elas,
            vbl = size_v,
            model = mdl,
            row.names = NULL
        )
    )
}

compute_avg_elas("Revenue", "IV: Revenue; Revenue$_{t-1}$", reg_mrg_ls, df, verbose = TRUE)

compute_avg_elas("Capital", "OLS: Revenue$_{t-1}$", reg_mrg_ls, df, verbose = TRUE)

## %% Computing average elasticities for all models and variables ----------------------

avg_elas_ls <-  mcmapply(
    size_v=comb_vars$size_v,
    mdl=comb_vars$mdl,
    compute_avg_elas,
    MoreArgs = list(
        dta = df,
        reg_ls = reg_mrg_ls),
    USE.NAMES = FALSE,
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

# (avg_elas_df <- do.call(rbind, avg_elas_ls))

avg_elas_df <- list_rbind(avg_elas_ls) 

## %% Bootstrap Coefficients & Avg Elasticities --------------------

boot_ls <- mclapply(
    1:B,
    function(i){
        resampled_data <- resample_by_group(df,sic_3)
        tmp_reg_ls <- lapply(
            fmls_nc,
            \(x){
                resampled_data %>% filter(
                    corp == "Other"
                ) %>%
                feols(
                    as.formula(x),
                    cluster = ~ plant + year,
                    panel.id = ~ plant + year,
                    data = .
                )
            }
        )

        tmp_df_ls <- lapply(
            seq_along(tmp_reg_ls),
            \(x) {
                tmp<-tmp_reg_ls[[x]] |> coef()

                data.frame(
                    var = names(tmp),
                    coef = tmp,
                    model = names(fmls_nc)[x],
                    row.names = NULL
                )
            }
        )
        coef_df_out <- do.call(rbind, tmp_df_ls)
        tmp_out_ls <- mapply(
            size_v=comb_vars$size_v,
            mdl=comb_vars$mdl,
            compute_avg_elas,
            MoreArgs = list(
                dta = resampled_data,
                reg_ls = tmp_reg_ls),
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        )
        elas_out_df <- do.call(rbind, tmp_out_ls)
        if(i %% 20 == 0){cat("Done with bootstrap replicate:", i, "\n")}
        return(
            list(
                coeffs = coef_df_out,
                elas = elas_out_df
            )
        )
    },
    mc.cores = mc_cores
)

## %% Saving results

save(
    boot_ls,
    avg_elas_df,
    reg_mrg_ls,
    file = "Code/Products/932.6-avg-elas-interactions.RData"
)

## %% Results table with bootstrapped SEs------------

elas_aux_df <- seq_along(boot_ls) |> map(
        \(x) boot_ls[[x]]$elas
    ) |> list_rbind() %>% # Faster than do.call(rbind, args=_)
    left_join(
        avg_elas_df,
        by = c("vbl", "model"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        elas_r = elasticity - elasticity.t0
    ) %>%
    group_by(model, vbl) %>%
    reframe(
        probs = c(0.975, .025),
        q_val = quantile(elas_r, probs = probs),
        CI = c("LCI", "UCI"),
        CI_val = first(elasticity.t0) - q_val,
        Elasticity = first(elasticity.t0),
        p_x = ecdf(elas_r)(first(elasticity.t0)),
        p_val = 2*min(p_x, 1-p_x),
        var_tbl = first(var_desc_ltx_elas[vbl])
    ) %>%
    pivot_wider(
        id_cols = c(model, var_tbl, Elasticity, p_val),
        names_from = CI,
        values_from = CI_val
    )

elas_aux_df |> View()

coef_aux_df <- seq_along(boot_ls) |> map(
    \(x) boot_ls[[x]]$coeffs
) |> list_rbind() |> as_tibble() %>% # Faster than do.call(rbind, args=_)
    left_join(
        coef_df_mrg_t0,
        by = c("var", "model"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        coef_r = coef - coef.t0,
    ) %>%
    group_by(model, var) %>%
    reframe(
        probs = c(0.975, .025),
        q_val = quantile(coef_r, probs = probs),
        CI = c("LCI", "UCI"),
        CI_val = first(coef.t0) - q_val,
        Coefficient = first(coef.t0),
        p_x = ecdf(coef_r)(first(coef.t0)),
        p_val = 2*min(p_x, 1-p_x),
        var_desc = first(var_desc_ltx[var])
    ) %>%
    pivot_wider(
        id_cols = c(model, var_desc, Coefficient, p_val),
        names_from = CI,
        values_from = CI_val
    )

coef_aux_df |> View()


## %% Saving results



save(
    boot_ls,
    avg_elas_df,
    reg_mrg_ls,
    coef_df_mrg_ls,
    coef_df_mrg_t0,
    elas_aux_df,
    coef_aux_df,
    file = "Code/Products/932.6-avg-elas-interactions.RData"
)

## %% Rendering tables with bootstrapped SEs

render_tbl <- function(x,x_desc,order_vec,aux_df,file_name, out_dir = "Paper/tbls"){
    tmp_tex <- tempfile(fileext = ".tex", tmpdir = out_dir)
    on.exit(unlink(tmp_tex), add = TRUE)
    print(tmp_tex)
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
            name_coef = factor(.data[[x_desc]], levels = order_vec)
        ) %>%
        select(model, name_coef, x_coef, CI_coef) %>%
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
            names_from = model,
            values_from = value,
            values_fill = "",
        ) %>%
        arrange(name_coef, type) %>%
        rename(
            ` ` = name_coef
        )
    print(tmp_tbl)
    tmp_tbl[-2] |>
        tt() |>
        style_tt(
            i = seq(1,nrow(tmp_tbl)-1, by=2),
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
        # "source ~/.zshrc",
        "export PATH=\"/Library/TeX/texbin:$PATH\"",
        "export PATH=\"/usr/local/bin:$PATH\"",
        "echo $PATH",
        paste0("'/Library/TeX/texbin/pdflatex' -synctex=1 -interaction=nonstopmode -file-line-error -recorder -output-directory=", out_dir, " '", tmp_tex,"'"),
        paste("mv", gsub("\\.tex", ".pdf", tmp_tex), paste0(out_dir,"/", file_name, ".pdf")),
        paste("'/usr/local/bin/magick' -density 300", paste0(out_dir,"/", file_name, ".pdf"),paste0(out_dir,"/", file_name, ".png")),
        # paste("'/usr/local/bin/magick' -density 300", gsub("\\.tex", ".pdf", tmp_tex),gsub("\\.tex", ".png", tmp_tex)),
        sep = "\n",
        file = tmp_sh
    )
    system(paste("chmod +x", tmp_sh), intern = TRUE)
    system2(tmp_sh, wait = TRUE)
    # system2("/usr/local/bin/magick", args = c("-density 300", paste0(out_dir,"/", file_name, ".pdf"), paste0(out_dir,"/", file_name, ".png")))
    # system(paste0("'/Library/TeX/texbin/xelatex' -synctex=1 -interaction=nonstopmode file-line-error -recorder -output-directory=", out_dir, " '", tmp_tex,"'"), intern = TRUE, wait = TRUE)
    # system(paste("'/usr/local/bin/magick' -density 300", paste0(out_dir,"/", file_name, ".pdf"),paste0(out_dir,"/", file_name, ".png")), intern = TRUE)
    # file.rename(gsub("\\.tex", ".png", tmp_tex), paste0("Paper/tbls/", file_name, ".png"))
}

render_tbl(
    x = "Elasticity",
    x_desc = "var_tbl",
    order_vec = var_desc_ltx,
    aux_df = elas_aux_df,
    file_name = "932.6-avg-elas"
)


render_tbl(
    x = "Coefficient",
    x_desc = "var_desc",
    order_vec = var_desc_ltx,
    aux_df = coef_aux_df %>% filter(model %in% c("IV: Revenue; Revenue$_{t-1}$", "OLS: Revenue$_{t-1}$")),
    file_name = "932.6-coefs" 
)

## %% Plotting results -----------------------------

coef_aux_df %>%
    mutate(
        # var_desc = factor(var_desc, levels = var_desc_ltx[length(var_desc_ltx):1]),
        var_plot = factor(var_desc_plt_coef[var_desc], levels = var_desc_plt_coef[length(var_desc_plt_coef):1])
    ) %>%
    filter(
        model %in% c("IV: Revenue; Revenue$_{t-1}$", "OLS: Revenue$_{t-1}$")
    ) %>%
    arrange(model, desc(var_plot)) %>%
    ggplot(
        aes(y = var_plot, x = Coefficient, color = model))+
    geom_point(position = position_dodge(width = 0.25))+
    geom_errorbar(
        aes(xmin = LCI, xmax = UCI),
        position = position_dodge(width = 0.25),
        width = 0.2
    )+
    geom_vline(xintercept = 0, color = "lightgray")+
    theme_minimal() +
    labs(x = "", y = "", color = "Model") +
    ggtitle(
        "Marginal Effect of Size Measures on Tax Evasion", 
        subtitle = "Coefficient and 95% Bootstrap CI with 250 replications"
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
    scale_color_manual(values = wong_cb_palette)

ggsave(
    "932.6-boot-se-mrg-plot.png",
    path = "Paper/images/",
    width = 10.1, height = 6.2, units = "in"
)
