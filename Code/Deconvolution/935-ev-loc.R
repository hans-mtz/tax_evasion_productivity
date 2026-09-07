# %% Load data and packages ---------------
library(tidyverse)
library(fixest)
library(ggplot2)
library(tinytable)
library(parallel)

load("Code/Products/global_vars.RData")
load("Code/Products/930-boot-se-het.RData")


# install.packages("devtools")
# devtools::install_github("nebulae-co/colmaps")

library(colmaps)
library(raster)
library(geodata)
library(sf)
B <- 250
## %% Geography

# Differences in tax evasion incentives across regions
# Hypothesis: Firms with more access to the government might evade more
# because they are less likely to be audited, so their probability of detection
# is lower.
# Hypothesis 2: The tax authority cost of auditing firms in remote regions is higher,
# so the tax authority might audit less in those regions, which could lead to more evasion.

# Geography variables in the data:
# - section_country_code: Section of the country (eg. Antioquia, Atlantico, Bogota D.E., etc.)
# - metro_area_code: Metropolitan Area (eg. Bogota, Cali, Medellin, Barranquila)

## %% Data - Department level evasion

df <- df %>%
    mutate(
        # Fix typo in section_country_code (83 should be 86). There is no department 83, and department 86 is missing
        section_country_code = ifelse(section_country_code == 83, 86, section_country_code)
    )

loc_dept_df <- df %>%
    ungroup() %>%
    mutate(
        total_sales = sum(log_sales, na.rm = TRUE)
    ) %>%
    group_by(section_country_code) %>%
    summarise(
        N_dept = n(),
        AvgEv_dept = mean(cal_V, na.rm = TRUE),
        MedEv_dept = median(cal_V, na.rm = TRUE),
        Avg_Revenue_dept = mean(log_sales, na.rm = TRUE),
        Market_Share_dept = sum(log_sales/ total_sales, na.rm = TRUE)*100,
        AvgTaxRate_dept = mean(share_sales_tax, na.rm = TRUE)
    ) 
loc_dept_df |> View()
loc_df <- df %>%
    left_join(
        loc_dept_df,
        by = "section_country_code"
    ) %>%
    filter(
        N_dept >= 10
    )

loc_dept_df %>%
    arrange(desc(Market_Share_dept)) |> View()

colombia_depto <- gadm(country="COL", level = 1, path = "Data/Colombia/")
col_dept_sf <- st_as_sf(colombia_depto)

sec_country_key <- departamentos@data

sec_country_key[!(departamentos@data$depto %in% col_dept_sf$NAME_1 ), "depto"] <- c(
    "Norte de Santander",
    "Valle del Cauca",
    "San Andrés y Providencia",
    "Bogotá D.C."
)

id_depto_key <- sec_country_key$id
names(id_depto_key) <- sec_country_key$depto
depto_id_key <- sec_country_key$depto
names(depto_id_key) <- sec_country_key$id |> as.numeric()
all(
    sec_country_key$depto %in% col_dept_sf$NAME_1
)

dict_dept <- sec_country_key$depto 
names(dict_dept) <- paste0("section_country_code::", sec_country_key$id |> as.numeric())



dict <- c(
    "cal_V" = "$\\mathcal{V}$",
    "metro_area_code"= "Metropolitan Area",
    "section_country_code" = "Department",
    "metro_area_code::1" = "Bogotá D.C., Soacha",
    "metro_area_code::2" = "Cali, Yumbo",
    "metro_area_code::3" = "Medellín, Valle de Aburrá",
    "metro_area_code::4" = "Manizales, Villamaria",
    "metro_area_code::5" = "Barranquilla, Soledad",
    "metro_area_code::6" = "Bucaramanga, Giron, Floridablanca",
    "metro_area_code::7" = "Pereira, Santa Rosa de Cabal, Dosquebradas",
    "metro_area_code::8" = "Cartagena",
    "metro_area_code::9" = "Rest of Colombia",
    dict_dept
)

metro_area_key <- c(
    "1" = "Bogotá D.C., Soacha",
    "2" = "Cali, Yumbo",
    "3" = "Medellín, Valle de Aburrá",
    "4" = "Manizales, Villamaria",
    "5" = "Barranquilla, Soledad",
    "6" = "Bucaramanga, Giron, Floridablanca",
    "7" = "Pereira, Santa Rosa de Cabal, Dosquebradas",
    "8" = "Cartagena",
    "9" = "Rest of Colombia"
)

## %% OLS Analysis

fmls_geo <- list(
    metro = "cal_V ~ -1 + i(metro_area_code)",
    country_section = "cal_V ~ -1 + i(section_country_code)"
)


loc_regs <- lapply(fmls_geo,
    function(fml) {
        feols(
            as.formula(fml),
            data = loc_df %>% filter(corp == "Other"),
            cluster = ~ plant + year
        ) 
    }
) 

## %% Render PNG table from dplyr table


render_png_tt_tbl <- function(tt_tbl_in, file_name, out_dir = "Paper/tbls"){
    tmp_tex <- tempfile(fileext = ".tex", tmpdir = out_dir)
    on.exit(unlink(tmp_tex), add = TRUE)
    print(tmp_tex)

    tt_tbl_in |>
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

## %% Render PNG tables from Fixest Regression etables

render_png_etbl <- function(tbl_in, dict=dict, file_name, out_dir = "Paper/tbls"){
    tmp_tex <- tempfile(fileext = ".tex", tmpdir = out_dir)
    on.exit(unlink(tmp_tex), add = TRUE)
    print(tmp_tex)

    etable(tbl_in, dict = dict, file = tmp_tex)
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

render_png_etbl(loc_regs, "935-evasion-loc-regs")

render_png_etbl(loc_regs[[1]], "935-evasion-loc-metro")

render_png_etbl(loc_regs[[2]], "935-evasion-loc-dept")

## %% Plot Map

depto_data <- summary(loc_regs[[2]])$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(
        Department = dict[var],
        stars = case_when(
            `Pr(>|t|)` < 0.01 ~ "***",
            `Pr(>|t|)` < 0.05 ~ "**",
            `Pr(>|t|)` < 0.1 ~ "*",
            TRUE ~ ""
        ),
        label = ifelse(
            `Pr(>|t|)` < 0.1,
            glue::glue("{Department} ({round(Estimate*100,1)}){stars}"),
            glue::glue("{Department}")
        ),
        id_depto = grep("section_country_code::", var, value = TRUE) %>% str_replace("section_country_code::", "") %>% as.integer()
    )

col_dept_sf %>% 
    mutate(
        id_depto = id_depto_key[NAME_1] |> as.integer()
    ) %>%
    left_join(
        depto_data %>% filter(`Pr(>|t|)` < 0.1),
        by = "id_depto"
    ) %>%
    ggplot() +
    geom_sf(aes(fill = Estimate)) +
    geom_sf_label(aes(label = label), position = position_jitter(height = 1.1, seed = 2225)) +
    scale_fill_viridis_c() +
    labs(title = "Average Evasion by Department",fill="Tax Evasion") +
    theme_void() +
    theme(
        legend.position = "left",
        strip.text = element_text(size = 12, face = "bold", family = "Times"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 12, hjust = 0.5)
    )

ggsave(
    "935-ev-loc-dept-map.png",
    path = "Paper/images/",
    width = 11,
    height = 11,
    units = "in"
)

# loc_dept_df %>%
#     rename(
#         id_depto = section_country_code,

#     ) %>%
#     colmap(
#         departamentos,
#         data = .,
#         var = "MedEv",
#         data_id = "id_depto"
#     )

# colmap(
#     departamentos
# )

# plot(colombia_depto)

# plot(col_dept_sf["NAME_1"])

# map_sf <- col_dept_sf %>% 
#     mutate(
#             id_depto = id_depto_key[NAME_1] |> as.integer()
#     ) %>%
#     left_join(
#         loc_dept_df %>% rename(id_depto = section_country_code) %>% filter(N_dept >= 10),
#         by = "id_depto"
#     )


# map_sf["AvgEv"] |> plot(main = "Average Evasion by Department", nbreaks = 5)


# map_sf %>%
#     ggplot() +
#     geom_sf(aes(fill = MedEv)) +
#     geom_sf_label(aes(label = NAME_1)) +
#     scale_fill_viridis_c() +
#     labs(title = "Average Evasion by Department",fill="Tax Evasion") +
#     theme_void() +
#     theme(
#         legend.position = "left",
#         strip.text = element_text(size = 12, face = "bold", family = "Times"),
#         plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(family = "Times", size = 12, hjust = 0.5)
#     )

## %% Save bootstrapped data ---------------------

save(
    loc_regs, loc_df, loc_dept_df, col_dept_sf,
    depto_data, dict, metro_area_key, sec_country_key,
    id_depto_key,
    file = "Code/Products/935-ev-loc.RData"
)

## %% Metro Area level Sum Stats

metro_df <- df %>%
    ungroup() %>%
    mutate(
        total_sales = sum(log_sales, na.rm = TRUE)
    ) %>%
    group_by(metro_area_code) %>%
    summarise(
        N = n(),
        `Avg. Sales Tax Rate` = mean(share_sales_tax, na.rm = TRUE)*100,
        `Med. Sales Tax Rate` = median(share_sales_tax, na.rm = TRUE)*100,
        `Avg. Tax Evasion` = mean(cal_V, na.rm = TRUE)*100,
        `Med. Tax Evasion` = median(cal_V, na.rm = TRUE)*100,
        `Market Share` = sum( log_sales/ total_sales, na.rm = TRUE)*100
    ) %>%
    mutate(
        `Metro Area` = metro_area_key[as.character(metro_area_code)],
    )

metro_df |> View()

metro_df %>%
    mutate(
        across(where(is.numeric), ~ round(., 1))
    ) %>%
    dplyr::select(
        `Metro Area`, N, `Market Share`, everything(), -metro_area_code
    ) %>%
    arrange(desc(`Market Share`)) |>
    tt(
        # digits=1,
        width = c(4,1,2,2,2,2,2)) |>
    style_tt(
        i = 0, j = 1:7,
        alignv = "m",
        align = "c"
    ) |>
    style_tt(
        j = 1:7, i = 1:nrow(metro_df),
        align = "lrrrrrr"
    ) |>
    render_png_tt_tbl("935-ev-loc-metro-summary")


## %% Country Section level Sum Stats

depto_df <- df %>%
    ungroup() %>%
    mutate(
        total_sales = sum(log_sales, na.rm = TRUE)
    ) %>%
    group_by(section_country_code) %>%
    summarise(
        N = n(),
        `Avg. Sales Tax Rate` = mean(share_sales_tax, na.rm = TRUE)*100,
        `Med. Sales Tax Rate` = median(share_sales_tax, na.rm = TRUE)*100,
        `Avg. Tax Evasion` = mean(cal_V, na.rm = TRUE)*100,
        `Med. Tax Evasion` = median(cal_V, na.rm = TRUE)*100,
        `Market Share` = sum(log_sales/ total_sales, na.rm = TRUE)*100
    ) %>%
    mutate(
        `Depto` = depto_id_key[as.character(section_country_code)],
    )

depto_df %>%
    mutate(
        across(where(is.numeric), ~ round(., 1))
    ) %>%
    dplyr::select(
        `Depto`, N, `Market Share`, everything(), -section_country_code
    ) %>%
    arrange(desc(`Market Share`)) %>%
    tt(
        width = c(4,1,2,2,2,2,2)) |>
    style_tt(
        i = 0, j = 1:7,
        alignv = "m",
        align = "c"
    ) |>
    style_tt(
        j = 1:7, i = 1:nrow(depto_df),
        align = "lrrrrrr"
    ) 

depto_df %>%
    dplyr::select(
        `Depto`, N, `Market Share`, everything(), -section_country_code
    ) %>%
    arrange(desc(`Market Share`)) %>%
    mutate(
        across(where(is.numeric), ~ round(., 1))
    ) %>%
    tt(
        # digits=1,
        width = c(4,1,2,2,2,2,2)) |>
    style_tt(
        i = 0, j = 1:7,
        alignv = "m",
        align = "c"
    ) |>
    style_tt(
        j = 1:7, i = 1:nrow(depto_df),
        align = "lrrrrrr"
    ) |>
    render_png_tt_tbl("935-ev-loc-depto-summary")

## %% Bootstrap SE

metro_boot_ls <- mclapply(
    1:B,
    \(i){
        resampled_data <- resample_by_group(df, sic_3)
        tmp_metro_df <- resampled_data %>%
            ungroup() %>%
            mutate(
                total_sales = sum(log_sales, na.rm = TRUE)
            ) %>%
            group_by(metro_area_code) %>%
            summarise(
                N = n(),
                `Avg. Sales Tax Rate` = mean(share_sales_tax, na.rm = TRUE)*100,
                `Med. Sales Tax Rate` = median(share_sales_tax, na.rm = TRUE)*100,
                `Avg. Tax Evasion` = mean(cal_V, na.rm = TRUE)*100,
                `Med. Tax Evasion` = median(cal_V, na.rm = TRUE)*100,
                `Market Share` = sum(log_sales/ total_sales, na.rm = TRUE)*100
            ) %>%
            mutate(
                `Metro Area` = metro_area_key[as.character(metro_area_code)],
            )
        return(tmp_metro_df)
    },
    mc.cores = mc_cores
)

## %% Save bootstrapped data ---------------------

save(
    metro_df, metro_boot_ls,
    loc_regs, loc_df, loc_dept_df, col_dept_sf,
    depto_data, dict, metro_area_key, sec_country_key,
    id_depto_key,
    file = "Code/Products/935-ev-loc.RData"
)
