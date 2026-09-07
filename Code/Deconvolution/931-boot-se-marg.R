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

## %% Define Variables and setting seed for reproducibility ----------------------

set.seed(66636)

mrg_vars <- c(
    "rev" = "Revenue",
    "rev2" = "Revenue$^2$",
    "lag_rev"="Revenue$_{t-1}$",
    "lag_rev2"="Revenue$_{t-1}^2$",
    "l"="Labour",
    "l2"="Labour$^2$",
    "k"="Capital",
    "k2"="Capital$^2$",
    "imports"="Imports",
    "imports2"="Imports$^2$",
    "exports"="Exports",
    "exports2"="Exports$^2$",
    "corp::Other" = "Non-Corp"
)

mrg_vars_plot <- c(
    "rev" = "Revenue",
    "rev2" = "Revenue Sq.",
    "lag_rev"="Revenue (t-1)",
    "lag_rev2"="Revenue (t-1) Sq.",
    "l"="Labour",
    "l2"="Labour Sq.",
    "k"="Capital",
    "k2"="Capital Sq.",
    "imports"="Imports",
    "imports2"="Imports Sq.",
    "exports"="Exports",
    "exports2"="Exports Sq.",
    "corp::Other" = "Non-Corp"
)

# Plot vars: Palettes for color-blind friendly plots

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

size_var <- c("Revenue", "Labour", "Capital", "Imports", "Exports")

my_colors <- wong_cb_palette[1:length(size_var)]
names(my_colors) <- size_var

# Estimation vars

fml_names <- c(
    "IV: Lagged Revenue",
    "IV: Lagged Capital",
    "IV: Lagged Labour",
    "OLS: Lagged Revenue"
)

fmls <- c(
    "cal_V ~ -1+i(corp, 'Corp')+
    i(corp, l_pct, ref='Corp')+i(corp,l_pct^2, ref='Corp')+
    i(corp, k_pct, ref='Corp')+i(corp,k_pct^2, ref='Corp')+
    i(corp, exports_pct, ref='Corp')+i(corp, exports_pct^2, ref='Corp')+
    i(corp, imports_pct, ref='Corp')+i(corp, imports_pct^2, ref='Corp') | i(corp, rev_pct, ref='Corp')+i(corp, rev_pct^2, ref='Corp') ~ i(corp, lag_rev_pct, ref='Corp')+i(corp, lag_rev_pct^2, ref='Corp')",
    "cal_V ~ -1+i(corp, 'Corp')+
    i(corp, l_pct, ref='Corp')+i(corp,l_pct^2, ref='Corp')+
    i(corp, k_pct, ref='Corp')+i(corp,k_pct^2, ref='Corp')+
    i(corp, exports_pct, ref='Corp')+i(corp, exports_pct^2, ref='Corp')+
    i(corp, imports_pct, ref='Corp')+i(corp, imports_pct^2, ref='Corp') | i(corp, rev_pct, ref='Corp')+i(corp, rev_pct^2, ref='Corp') ~ i(corp, cap_l_pct, ref='Corp')+i(corp, cap_l_pct^2, ref='Corp')",
    "cal_V ~ -1+i(corp, 'Corp')+
    i(corp, l_pct, ref='Corp')+i(corp,l_pct^2, ref='Corp')+
    i(corp, k_pct, ref='Corp')+i(corp,k_pct^2, ref='Corp')+
    i(corp, exports_pct, ref='Corp')+i(corp, exports_pct^2, ref='Corp')+
    i(corp, imports_pct, ref='Corp')+i(corp, imports_pct^2, ref='Corp') | i(corp, rev_pct, ref='Corp')+i(corp, rev_pct^2, ref='Corp') ~ i(corp, lab_l_pct, ref='Corp')+i(corp, lab_l_pct^2, ref='Corp')",
    "cal_V ~ -1+i(corp, 'Corp')+
    i(corp, lag_rev_pct, ref='Corp')+i(corp, lag_rev_pct^2, ref='Corp')+
    i(corp, l_pct, ref='Corp')+i(corp,l_pct^2, ref='Corp')+
    i(corp, k_pct, ref='Corp')+i(corp,k_pct^2, ref='Corp')+
    i(corp, exports_pct, ref='Corp')+i(corp, exports_pct^2, ref='Corp')+
    i(corp, imports_pct, ref='Corp')+i(corp, imports_pct^2, ref='Corp')"
)



names(fmls) <- fml_names


## %% Marghinal Effects of Size Variables -------------------

df |> names() |> grep("pct", x=_, value=TRUE)

feols(
  cal_V ~ -1+i(corp, "Corp")+
    i(corp, l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp")+
    i(corp, k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp")+
    i(corp, exports_pct, ref="Corp")+i(corp, exports_pct^2, ref="Corp")+
    i(corp, imports_pct, ref="Corp")+i(corp, imports_pct^2, ref="Corp")
  | i(corp, rev_pct, ref="Corp")+i(corp, rev_pct^2, ref="Corp") ~ i(corp, lag_rev_pct, ref="Corp")+i(corp, lag_rev_pct^2, ref="Corp"),
  cluster = ~ plant + year,
  data = df
) |> etable()

feols(
  cal_V ~ -1+i(corp, "Corp")+
    i(corp, l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp")+
    i(corp, k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp")+
    i(corp, exports_pct, ref="Corp")+i(corp, exports_pct^2, ref="Corp")+
    i(corp, imports_pct, ref="Corp")+i(corp, imports_pct^2, ref="Corp")
  | i(corp, rev_pct, ref="Corp")+i(corp, rev_pct^2, ref="Corp") ~ i(corp, cap_l_pct, ref="Corp")+i(corp, cap_l_pct^2, ref="Corp"),
  cluster = ~ plant + year,
  data = df
) |> etable()

feols(
  cal_V ~ -1+i(corp, "Corp")+
    i(corp, l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp")+
    i(corp, k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp")+
    i(corp, exports_pct, ref="Corp")+i(corp, exports_pct^2, ref="Corp")+
    i(corp, imports_pct, ref="Corp")+i(corp, imports_pct^2, ref="Corp")
  | i(corp, rev_pct, ref="Corp")+i(corp, rev_pct^2, ref="Corp") ~ i(corp, lab_l_pct, ref="Corp")+i(corp, lab_l_pct^2, ref="Corp"),
  cluster = ~ plant + year,
  data = df
) |> etable()

feols(
  cal_V ~ -1+i(corp, "Corp")+
    i(corp, lag_rev_pct, ref="Corp")+i(corp, lag_rev_pct^2, ref="Corp")+
    i(corp, l_pct, ref="Corp")+i(corp,l_pct^2, ref="Corp")+
    i(corp, k_pct, ref="Corp")+i(corp,k_pct^2, ref="Corp")+
    i(corp, exports_pct, ref="Corp")+i(corp, exports_pct^2, ref="Corp")+
    i(corp, imports_pct, ref="Corp")+i(corp, imports_pct^2, ref="Corp"),
  cluster = ~ plant + year,
  data = df
) |> etable()



reg_mrg_ls <- lapply(
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

reg_mrg_ls |> etable()
# Capital does not pass the test of weak instruments

coef_df_mrg_ls <- lapply(
    seq_along(reg_mrg_ls),
    \(x) {
        tmp<-reg_mrg_ls[[x]] |> coef()
        data.frame(
            var = names(tmp),
            coef = tmp,
            model = names(fmls)[x],
            row.names = NULL
        )
    }
)

coef_df_mrg_t0 <- do.call(rbind, coef_df_mrg_ls)
coef_df_mrg_t0 

## %% Boostraping SEs ----------------------



coef_df_boot_mrg_ls <- mclapply(
    1:B,
    \(i){
        resampled_data <- resample_by_group(df,sic_3)

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
                    coef = tmp,
                    model = names(fmls)[x],
                    row.names = NULL
                )
            }
        )

        df_out <- do.call(rbind, tmp_df_ls)
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(df_out)
    },
    mc.cores = mc_cores
)

## Results table with bootstrapped SEs------------

res_mrg_df <- do.call(
    rbind,
    coef_df_boot_mrg_ls
    ) %>%
    as.tibble() %>%
    left_join(
        coef_df_mrg_t0,
        by = c("var", "model"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        bc_coef = coef-coef.t0,
        id = paste(model, var)
    ) %>%
    group_by(model, var) %>%
    reframe(
        val_coef = quantile(bc_coef, c(0.975, 0.025), na.rm = TRUE),
        # val_coef = quantile(bc_coef, c(0.95, 0.05)),
        probs = c(0.975, 0.025),
        CI = c("LCI", "UCI"),
        CI_coef = max(coef.t0, na.rm = TRUE)- val_coef,
        coef_0 = max(coef.t0, na.rm = TRUE),
        # coeff_se = 2*max(se.t0)-mean(se),
        prob = ecdf(bc_coef)(max(coef.t0, na.rm = TRUE)),
        p_val = 2*min(
            # prob >= 0.5,
            1 - prob,
            prob
        )
    ) %>%
    pivot_wider(
        id_cols = c(model, var, coef_0, p_val),
        names_from = CI,
        values_from = CI_coef
    )

## %% Table with bootstrapped SEs ----------------------

aux_df <- res_mrg_df %>%
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
        aux = gsub(
            ".*corp.*:I?\\(?(.*)_pct\\^?(2)?.*", "\\1\\2",
            var
        ),
        coef_name = mrg_vars[aux],
        coef_name = factor(coef_name, levels = mrg_vars)
        ) %>% 
        select(
            model, coef_name, coeff_coef, CI_coef
        ) %>% arrange(model, coef_name)

        
aux_df  |> View()

res_mrg_all_tbl <- aux_df %>%
    pivot_longer(
        cols = c(coeff_coef, CI_coef),
        names_to = "type",
        values_to = "val",
        names_pattern = "(.*)_.*"
     ) %>%
    mutate(
        type = factor(type, levels = c("coeff", "CI"))
    ) %>%
    pivot_wider(
        names_from = model,
        values_from = val,
        values_fill = ""
    ) %>% arrange(coef_name, type)

res_mrg_all_tbl |> View()

res_mrg_tbl_1 <- aux_df %>%
    pivot_longer(
        cols = c(coeff_coef, CI_coef),
        names_to = "type",
        values_to = "val",
        names_pattern = "(.*)_.*"
     ) %>%
    mutate(
        type = factor(type, levels = c("coeff", "CI"))
    ) %>%
    filter(
        model %in% c("IV: Lagged Revenue", "OLS: Lagged Revenue")
    ) %>%
    pivot_wider(
        names_from = model,
        values_from = val,
        values_fill = ""
    ) %>% arrange(coef_name, type)

res_mrg_tbl_1 |> View()

res_mrg_tbl_2 <- aux_df %>%
    pivot_longer(
        cols = c(coeff_coef, CI_coef),
        names_to = "type",
        values_to = "val",
        names_pattern = "(.*)_.*"
     ) %>%
    mutate(
        type = factor(type, levels = c("coeff", "CI"))
    ) %>%
    filter(
        model %in% c("IV: Lagged Capital", "IV: Lagged Labour")
    ) %>%
    pivot_wider(
        names_from = model,
        values_from = val,
        values_fill = ""
    ) %>% arrange(coef_name, type)
    
res_mrg_tbl_2 |> View()




## %% Save tables in PNG format ----------------------


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

res_mrg_tbl_1[,-2] %>% rename(
    ` ` = coef_name
) %>%
tt() |>
    style_tt(
        i = seq(1,nrow(res_mrg_tbl_1)-1, length.out = nrow(res_mrg_tbl_1)/2), j = 1,
        rowspan = 2, alignv = "t"
    )|>
    save_tt(
        output = "Paper/images/931-boot-se-mrg-tbl-1.tex",
        overwrite = TRUE
    )

tex_tbl <- readLines("Paper/images/931-boot-se-mrg-tbl-1.tex")

cat(
    preamble,
    "\\begin{document}",
    tex_tbl,
    "\\end{document}",
    file = "Paper/tbls/931-boot-se-mrg-tbl-1.tex",
    sep = "\n"
)


res_mrg_tbl_2[,-2] %>% rename(
    ` ` = coef_name
) %>%
tt() |>
    style_tt(
        i = seq(1,nrow(res_mrg_tbl_2)-1, length.out = nrow(res_mrg_tbl_2)/2), 
        j = 1,
        rowspan = 2, alignv = "t"
    )|>
    save_tt(
        output = "Paper/images/931-boot-se-mrg-tbl-2.tex",
        overwrite = TRUE
    )

tex_tbl <- readLines("Paper/images/931-boot-se-mrg-tbl-2.tex")

cat(
    preamble,
    "\\begin{document}",
    tex_tbl,
    "\\end{document}",
    file = "Paper/tbls/931-boot-se-mrg-tbl-2.tex",
    sep = "\n"
)

res_mrg_all_tbl[,-2] %>% rename(
    ` ` = coef_name
) %>%
tt() |>
    style_tt(
        i = seq(1,nrow(res_mrg_all_tbl)-1, length.out = nrow(res_mrg_all_tbl)/2), 
        j = 1,
        rowspan = 2, alignv = "t"
    )|>
    save_tt(
        output = "Paper/images/931-boot-se-mrg-tbl-all.tex",
        overwrite = TRUE
    )

tex_tbl <- readLines("Paper/images/931-boot-se-mrg-tbl-all.tex")

cat(
    preamble,
    "\\begin{document}",
    tex_tbl,
    "\\end{document}",
    file = "Paper/tbls/931-boot-se-mrg-tbl-all.tex",
    sep = "\n"
)

## %% Plotting Coefficients and CIs ----------------------

# load("Code/Products/931-boot-se-marg.RData")

aux_plot_df <- res_mrg_df %>% 
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
        aux = gsub(
            ".*corp.*:I?\\(?(.*)_pct\\^?(2)?.*", "\\1\\2",
            var
        ),
        coef_name = mrg_vars_plot[aux],
        coef_name = factor(coef_name, levels = mrg_vars_plot)
        ) %>% 
        arrange(model, coef_name)

aux_plot_df |> View()

aux_plot_df %>%
    mutate(
        coef_name = factor(coef_name, levels = mrg_vars_plot[length(mrg_vars_plot):1])
    ) %>%
    filter(
        model %in% c("IV: Lagged Revenue", "OLS: Lagged Revenue")
    ) %>%
    arrange(model, desc(coef_name)) %>%
    ggplot(
        aes(y = coef_name, x = coef_0, color = model))+
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
    "931-boot-se-mrg-plot.png",
    path = "Paper/images/",
    width = 6.2, height = 6.2, units = "in"
)

## %% Plotting Expected Tax Evasion Conditional on Size ----------------------

# Define a function that computes the expected tax evasion, holding
# all other variables at their median, and varying the size variable of interest 
# for a given value of the percentile of the size variable [0-1]

predict_ev <- function(x,var_ty,size_var,mdl,df){
    
    
    coefs_tmp <- df %>%
        filter(model == mdl) %>%
        select(coef_0) %>%
        pull()
    


    names(coefs_tmp) <- df %>%
        filter(model == mdl) %>%
        select(coef_name) %>%
        pull()

    sel <- grepl(size_var, names(coefs_tmp))
    coefs_tmp[sel] <- df %>%
        filter(model == mdl, grepl(size_var, coef_name)) %>%
        select({{var_ty}}) %>%
        pull()
    # print(coefs_tmp)
    values <- c(
        rep(c(0.5,0.5^2), (length(coefs_tmp)-1)/2),
        1
    )
    names(values) <- names(coefs_tmp)
    # sel <- grepl(size_var, names(values))
    values[sel] <- c(x, x^2)
    # print(values)
    expected_evasion <- values %*% coefs_tmp |> drop()
    return(expected_evasion)
}

predict_ev(0.8,"coef_0" ,"Revenue", "IV: Lagged Revenue", aux_plot_df)

Vectorize(predict_ev, vectorize.args = "x") -> predict_ev_vec


curve(
    predict_ev_vec(x, "coef_0", "Revenue", "IV: Lagged Revenue", aux_plot_df),
    from = 0, to = 1,
    xlab = "",
    ylab = "",
    main = "",
    col = wong_cb_palette[1],
    lwd = 2,
    lty = "solid"
)

curve(
    predict_ev_vec(x, "LCI", "Revenue", "IV: Lagged Revenue", aux_plot_df),
    from = 0, to = 1,
    xlab = "",
    ylab = "",
    main = "",
    col = wong_cb_palette[1],
    lwd = 2,
    lty = "dashed",
    add = TRUE
)

curve(
    predict_ev_vec(x, "UCI", "Revenue", "IV: Lagged Revenue", aux_plot_df),
    from = 0, to = 1,
    xlab = "",
    ylab = "",
    main = "Revenue",
    col = wong_cb_palette[1],
    lwd = 2,
    lty = "dashed",
    add = TRUE
)

plot_mrg_curve <- function(size_var, mdl, df){
    plot_name <- ifelse(
        size_var == "Revenue" & mdl == "OLS: Lagged Revenue",
        "Revenue (t-1)",
        size_var
    )
    print(plot_name)
    plot(
        NA, NA,
        xlim = c(0,1), ylim = c(-0.4, 0.4),
        xlab = "",
        ylab = "",
        main = plot_name,
        frame.plot = FALSE
    )
    curve(
        predict_ev_vec(x, "coef_0", size_var, mdl, df),
        from = 0, to = 1,
        xlab = "",
        ylab = "",
        main = plot_name,
        col = my_colors[size_var],
        lwd = 2,
        lty = "solid",
        add = TRUE
    )

    curve(
        predict_ev_vec(x, "LCI", size_var, mdl, df),
        from = 0, to = 1,
        # xlab = "",
        # ylab = "",
        # main = "",
        col = my_colors[size_var],
        lwd = 2,
        lty = "dashed",
        add = TRUE
    )

    curve(
        predict_ev_vec(x, "UCI", size_var, mdl, df),
        from = 0, to = 1,
        # xlab = "",
        # ylab = "",
        # main = paste0(size_var),
        col = my_colors[size_var],
        lwd = 2,
        lty = "dashed",
        add = TRUE
    )
    # recordPlot()
}




## %% Saving plots in PNG format ----------------------

png(
  file = "Paper/images/931-mrg-curves-1.png",
  width = 720, height = 480
)

par(mfcol = c(1, 3), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)


plot_mrg_curve("Revenue", "IV: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")
plot_mrg_curve("Labour", "IV: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")
plot_mrg_curve("Imports", "IV: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")

title(
    line = 1,
    outer = TRUE,
    family = "serif",
    xlab = "Size Percentile",
    ylab = "",
)
mtext(
    "Predicted Tax Evasion - IV: Lagged Revenue",
    line = 2,
    outer = TRUE,
    family = "serif",
    cex = 1.5,
    font = 2
)
mtext(
    "Conditional Expectation Function. Other Variables Fixed at the Median (0.5)",
    line = 0.2,
    outer = TRUE,
    family = "serif",
    cex = 1.3,
    font = 1
)
dev.off()

png(
  file = "Paper/images/931-mrg-curves-2.png",
  width = 640, height = 480
)

par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

plot_mrg_curve("Capital", "IV: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")
plot_mrg_curve("Exports", "IV: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")

title(
    line = 1,
    outer = TRUE,
    family = "serif",
    xlab = "Size Percentile",
    ylab = "",
)
mtext(
    "Predicted Tax Evasion - IV: Lagged Revenue",
    line = 2,
    outer = TRUE,
    family = "serif",
    cex = 1.5,
    font = 2
)
mtext(
    "Conditional Expectation Function. Other Variables Fixed at the Median (0.5)",
    line = 0.2,
    outer = TRUE,
    family = "serif",
    cex = 1.3,
    font = 1
)
dev.off()

## %% Plot OLS Model Curves ----------------------


png(
  file = "Paper/images/931-mrg-curves-ols-1.png",
  width = 720, height = 480
)

par(mfcol = c(1, 3), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)


plot_mrg_curve("Revenue", "OLS: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")
plot_mrg_curve("Labour", "OLS: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")
plot_mrg_curve("Imports", "OLS: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")

title(
    line = 1,
    outer = TRUE,
    family = "serif",
    xlab = "Size Percentile",
    ylab = "",
)
mtext(
    "Predicted Tax Evasion - OLS: Lagged Revenue",
    line = 2,
    outer = TRUE,
    family = "serif",
    cex = 1.5,
    font = 2
)
mtext(
    "Conditional Expectation Function. Other Variables Fixed at the Median (0.5)",
    line = 0.2,
    outer = TRUE,
    family = "serif",
    cex = 1.3,
    font = 1
)
dev.off()

png(
  file = "Paper/images/931-mrg-curves-ols-2.png",
  width = 640, height = 480
)

par(mfcol = c(1, 2), oma = c(2,2,4,0), mar = c(1,0.5,1.5,1),
    mgp = c(1.5, 0.4, 0),
    family = "serif", cex.main = 1.3, cex.sub = 1.1)

plot_mrg_curve("Capital", "OLS: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")
plot_mrg_curve("Exports", "OLS: Lagged Revenue", aux_plot_df)
abline(h = 0, col = "lightgray", lty = "dashed")

title(
    line = 1,
    outer = TRUE,
    family = "serif",
    xlab = "Size Percentile",
    ylab = "",
)
mtext(
    "Predicted Tax Evasion - OLS: Lagged Revenue",
    line = 2,
    outer = TRUE,
    family = "serif",
    cex = 1.5,
    font = 2
)
mtext(
    "Conditional Expectation Function. Other Variables Fixed at the Median (0.5)",
    line = 0.2,
    outer = TRUE,
    family = "serif",
    cex = 1.3,
    font = 1
)
dev.off()

## %% Save results ----------------------

save(
    coef_df_mrg_t0,
    coef_df_boot_mrg_ls,
    reg_mrg_ls,
    res_mrg_df,
    res_mrg_all_tbl,
    res_mrg_tbl_1,
    res_mrg_tbl_2,
    aux_df, aux_plot_df,
    file = "Code/Products/931-boot-se-marg.RData"
)

## %% Estimating Average Size Elasticities of Tax Evasion ----------------------
load("Code/Products/931-boot-se-marg.RData")


## I need to estimate E[dcal_V/dX]=E[de/dX+depsilon/dX]=E[de/dX]=mu_z+mu2_z*2*E[X]
## I also need to estimate the CI of this elasticity using bootstrap empirical distribution

## OLS model: simplest 
# For each X (Revenue, Labour, Capital, Imports, Exports):
# 1) Get coefficients
# 2) Get mean of X
# 3) Compute elasticity at mean: mu_z+mu2_z*2*E[X]

## IV model: For exogenous vars, same as OLS
# for endogenous var (Revenue), I need to get the fitted values \hat{X}
# of the first stage regression, and then compute the elasticity 
# For revenue,
# 1) Get coefficients of first stage regression 

size_var_mdl <- c(
    "rev_pct",
    "l_pct",
    "k_pct",
    "exports_pct",
    "imports_pct"
)

allcoefs <- reg_mrg_ls[["OLS: Lagged Revenue"]] |> coef()
sel <- grepl("rev_pct", names(allcoefs))
allcoefs[sel]
tmp_var<-gsub("corp.*:I?\\(?(.*_pct).*", "\\1", names(allcoefs)[sel])[1]

df %>%
    mutate(
        tmp = allcoefs[sel][1] + 2*allcoefs[sel][2]*.data[[tmp_var]]
    ) %>%
    filter(corp == "Other") %>%
    summarise(
        elasticity = mean(tmp, na.rm = TRUE),
        vbl = tmp_var
    )

compute_avg_elas <- function(size_v, mdl, dta){
    allcoefs <- reg_mrg_ls[[mdl]] |> coef()
    sel <- grepl(size_v, names(allcoefs))
    var_coef <- allcoefs[sel]
    tmp_var<-gsub(".*corp.*:I?\\(?(.*_pct).*", "\\1", names(allcoefs)[sel])[1]

    if (mdl == "IV: Lagged Revenue" & size_v == "rev_pct"){
        tmp_var <- "fitted_rev_pct"
        # Predict does not work because of the interactions
        # data$fitted_rev_pct <- reg_mrg_ls[["IV: Lagged Revenue"]]$iv_first_stage$`corp::Other:rev_pct` |> predict(newdata = data)
        # I'll do it manually
        fs_dt <- reg_mrg_ls[["IV: Lagged Revenue"]]$iv_first_stage$`corp::Other:rev_pct` |> model.matrix(
            data = dta,
            type = "rhs"
        )
        fs_dt <- fs_dt |> as.data.frame() %>% filter(`corp::Other` == 1)
        fs_coefs <- reg_mrg_ls[["IV: Lagged Revenue"]]$iv_first_stage$`corp::Other:rev_pct` |> 
            coef()
        fitted_rev_pct <- as.matrix(fs_dt) %*% fs_coefs |> drop()
        print(head(fitted_rev_pct ))
        elasticity <- var_coef[1] + 2*var_coef[2]*fitted_rev_pct
        return(
            data.frame(
                elasticity = mean(elasticity, na.rm = TRUE),
                vbl = tmp_var,
                model = mdl,
                row.names = NULL
            ) %>% as_tibble()
        )
    }

    tmp_out <- dta %>%
        mutate(
            tmp = var_coef[1] + 2*var_coef[2]*.data[[tmp_var]]
        ) %>%
        filter(corp == "Other") %>%
        summarise(
            elasticity = mean(tmp, na.rm = TRUE),
            vbl = tmp_var,
            model = mdl
        )
    return(tmp_out)
}

compute_avg_elas("l_pct", "IV: Lagged Revenue", df)

comb_vars <- expand.grid(
    size_var = size_var_mdl,
    mdl = c("IV: Lagged Revenue", "OLS: Lagged Revenue"),
    stringsAsFactors = FALSE
)

avg_elas_ls <-  mcmapply(
    size_v=comb_vars$size_var,
    mdl=comb_vars$mdl,
    compute_avg_elas,
    MoreArgs = list(dta = df),
    USE.NAMES = FALSE,
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

avg_elas_df <- do.call(rbind, avg_elas_ls)

tmp_data <- resample_by_group(df, sic_3)


## %% Notes:-------------------------------------------

# Does not work because of the interactions and the way the predict function is implemented for ivreg objects in fixest
reg_mrg_ls[['IV: Lagged Revenue']] |> predict(newdata = tmp_data)
reg_mrg_ls[["IV: Lagged Revenue"]]$iv_first_stage$`corp::Other:rev_pct` |> predict(newdata= tmp_data)

# Does work
reg_mrg_ls[["OLS: Lagged Revenue"]] |> predict(newdata = tmp_data)

reg_mrg_ls[["IV: Lagged Revenue"]] |> model.matrix(
    data = tmp_data,
    # stage = 1,
    type = "rhs"
) |> head()

reg_mrg_ls[["IV: Lagged Revenue"]]$iv_first_stage$`corp::Other:rev_pct` |> model.matrix(
    data = tmp_data,
    stage = 1,
    type = "rhs"
) |> head()

reg_mrg_ls[["IV: Lagged Revenue"]]$iv_first_stage$`corp::Other:rev_pct` |> 
    coef()

## %% Avg Elasticity Bootstrap CI--------------------

boot_avg_elas_ls <- mclapply(
    1:B,
    function(i){
        resampled_data <- resample_by_group(df,sic_3)
        tmp_out_ls <- mapply(
            size_v=comb_vars$size_var,
            mdl=comb_vars$mdl,
            compute_avg_elas,
            MoreArgs = list(dta = resampled_data),
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        )
        tmp_out_df <- do.call(rbind, tmp_out_ls)
        if(i %% 20 == 0){cat("Done with bootstrap replicate:", i, "\n")}
        return(tmp_out_df)
    },
    mc.cores = mc_cores
)

## %% Results table with bootstrapped SEs------------

do.call(
        rbind,
        boot_avg_elas_ls
    ) %>%
    as_tibble() %>%
    left_join(
        avg_elas_df,
        by = c("model","vbl"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        elas = elasticity-elasticity.t0
    ) %>% 
    group_by(model, vbl) %>%
    reframe(
        val_elas = quantile(elas, c(0.975, 0.025)),
        # val_elas = quantile(elas, c(0.95, 0.05)),
        probs = c(0.975, 0.025),
        CI = c("LCI", "UCI"),
        CI_elas = first(elasticity.t0)- val_elas,
        elas_0 = first(elasticity.t0),
        # elasf_se = 2*first(se.t0)-mean(se),
        prob = ecdf(elas)(first(elasticity.t0)),
        p_val = 2*min(
            # prob >= 0.5,
            1 - prob,
            prob
        )
    ) %>%
    pivot_wider(
        id_cols = c(model,vbl, elas_0, p_val),
        names_from = CI,
        values_from = CI_elas
    )

 # Does not make sense, t0 outside the CIs. Review
do.call(
        rbind,
        boot_avg_elas_ls
    ) %>%
    group_by(model, vbl) %>%
    reframe(
        val_elas = quantile(elasticity, c(0.975, 0.5, 0.025)),
        # val_elas = quantile(elas, c(0.95, 0.05)),
        probs = c(0.975, 0.5, 0.025),
        desc = c("LCI", "Median", "UCI")
    ) %>%
    pivot_wider(
        id_cols = c(model,vbl),
        names_from = desc,
        values_from = val_elas
    ) %>%
    left_join(
        avg_elas_df,
        by = c("model", "vbl")
    )
