## %% Packages and Data -------------------
library(tidyverse)
library(data.table)
library(tinytable)


load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/930-boot-se-het.RData")

## %% Define variables for deciles

g_vars <- c("Labour", "Capital", "Revenue")
dec_var <- c("labour_ntile", "capital_ntile", "rev_ntile")
names(dec_var) <- g_vars
desc_var <- c("Revenue (log)", "Capital (log)", "Labour (log)", "Materials (log)", "Imports (\\%)", "Exports (\\%)")
char_var <- c("log_sales", "k", "l", "m", "share_imports", "share_exports")
names(desc_var) <- char_var

## %% Comparing deciles

df %>%
    filter(
        !is.na(k),
        !is.na(l),
        !is.na(m),
        !is.na(y),
        !is.na(log_mats_share)
    ) %>%
    group_by(corp) %>%
    summarise(
        across(
            c(log_sales, k, l, m, share_imports, share_exports),
            list(
                P10 = ~ quantile(., 0.1, na.rm = TRUE),
                Q1 = ~ quantile(., 0.25, na.rm = TRUE),
                # Mean = ~ mean(., na.rm = TRUE),
                Median = ~ median(., na.rm = TRUE),
                Q3 = ~ quantile(., 0.75, na.rm = TRUE),
                P90 = ~ quantile(., 0.9, na.rm = TRUE)
            ),
            .names = "{.col}_{.fn}"
        )
    ) %>%
    pivot_longer(
        -corp,
        names_to = c("var", "stat"),
        names_pattern = "(.*)_(Min|Q1|Mean|Median|Q3|Max|P10|P90)$"
    ) %>%
    pivot_wider(
        names_from = stat,
        values_from = value
    ) %>%
    arrange(var, corp)

df %>%
    ungroup() %>%
    mutate(
        log_exports = log(exports),
        log_imports= log(imported_inputs),
        exports_ntile = ntile(share_exports, 10),
        imports_ntile = ntile(share_imports, 10)
    ) %>%
    group_by(exports_ntile) %>%
    summarise(
        across(
            c(log_sales, k, l, m, share_imports, share_exports, log_exports, log_imports),
            list(
                P10 = ~ quantile(., 0.1, na.rm = TRUE),
                Q1 = ~ quantile(., 0.25, na.rm = TRUE),
                # Mean = ~ mean(., na.rm = TRUE),
                Median = ~ median(., na.rm = TRUE),
                Q3 = ~ quantile(., 0.75, na.rm = TRUE),
                P90 = ~ quantile(., 0.9, na.rm = TRUE)
            ),
            .names = "{.col}_{.fn}"
        )
    ) %>%
    pivot_longer(
        -exports_ntile,
        names_to = c("var", "stat"),
        names_pattern = "(.*)_(Min|Q1|Mean|Median|Q3|Max|P10|P90)$"
    ) %>%
    pivot_wider(
        names_from = stat,
        values_from = value
    ) %>%
    arrange(exports_ntile,var) |> View()

## %% Corps vs. 8-10 Non-Corp Deciles

df_tmp <- df %>%
    filter(
        !is.na(k),
        !is.na(l),
        !is.na(m),
        !is.na(y),
        !is.na(log_mats_share),
        !is.na(labour_ntile),
        !is.na(capital_ntile),
        !is.na(rev_ntile)
    ) %>%
    mutate(
        Labour = case_when(
            corp == "Other" ~ paste0(labour_ntile),
            corp == "Corp" ~ "Corp",
            .default = NA_character_),
        Capital = case_when(
            corp == "Other" ~ paste0(capital_ntile), 
            corp == "Corp" ~ "Corp", 
            .default = NA_character_),
        Revenue = case_when(
            corp == "Other" ~ paste0(rev_ntile), 
            corp == "Corp" ~ "Corp", 
            .default = NA_character_),
        Labour = factor(Labour, levels = c(1:10, "Corp")),
        Capital = factor(Capital, levels = c(1:10, "Corp")),
        Revenue = factor(Revenue, levels = c(1:10, "Corp"))
    )

char_tbl <- lapply(
    c("Labour", "Capital", "Revenue"),
    function(g) {
        df_tmp %>%
            mutate(
                dec_tmp = factor(.data[[dec_var[[g]]]], levels = 1:10),
                log_exports = log(exports),
                log_imports= log(imported_inputs)
            ) %>%
            filter(dec_tmp %in% 8:10) %>%
            group_by(.data[[g]]) %>%
            summarise(
                Size = g,
                across(
                    c(log_sales, k, l, m,  share_imports, share_exports),
                    list(
                        Median = ~ median(., na.rm = TRUE)
                    ),
                    .names = "{.col}_{.fn}"
                )
            ) %>%
            rename(
                Type = .data[[g]]
            ) 
    }
) |> bind_rows() |>
pivot_longer(
    -c(Size, Type),
    names_to = c("var", "stat"),
    names_pattern = "(.*)_(Median)$"
) |>
pivot_wider(
    names_from = Type,
    values_from = value
) |>
mutate(
    Characteristic = desc_var[var]
) |>
select(
    Size, Characteristic, Corp, `10th`=`10`, `9th`=`9`, `8th`=`8`
)

char_tbl
tt(char_tbl, digits = 1)

char_tbl |>
    mutate(
        across(
            is.numeric,
            ~ round(.x, 1)
        )
    ) |>
    tt() |>
    style_tt(
        i = seq(1,nrow(char_tbl)-1, by=6),
        j = 1,
        rowspan = 6,
        alignv = "t"
    )
## %% Render Tables -------------------

render_png_tbl <- function(tbl_in, file_name, out_dir = "Paper/tbls"){
    tmp_tex <- tempfile(fileext = ".tex", tmpdir = out_dir)
    on.exit(unlink(tmp_tex), add = TRUE)
    print(tmp_tex)

    tbl_in |>
        tt() |>
        style_tt(
            i = seq(1,nrow(tbl_in)-1, by=6),
            j = 1,
            rowspan = 6,
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

char_tbl |>
    mutate(
        across(
            is.numeric,
            ~ round(.x, 1)
        )
    ) |>
    render_png_tbl(file_name = "940-char-dec-tbl")
