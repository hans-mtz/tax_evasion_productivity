## PRODUCT: Thesis/tables/ch03-income-tax-1983.png := average income-tax rates
## for individuals, 1982 schedule vs. the 1983 schedule that stayed
## (@tbl-inc-tax-1983, Setting and Data chapter). Also writes the transcription
## to Code/Products/PerryCardenas1986-CuadroIII1.csv for quick consultation
## (CSV files are git-ignored; THIS script is the tracked copy of the data).
##
## SOURCE (transcribed by hand, 2026-09-23, checked against the page by Hans):
## Perry and Cardenas (1986), "Diez anos de reformas tributarias en Colombia",
## Fedesarrollo, vol. 1, Cuadro III.1 "Tarifas promedio de impuesto sobre la
## renta", PDF p. 70 (Lit-Papers/PerryCardenas1986-DiezAnosReformasTributarias-v1.pdf).
## Taxable income in thousands of 1982 pesos; declarant with three dependants.
## Columns used: 1982 A (Decreto 2809, the schedule in force for 1982) and
## 1983 (Decreto 397, Ley 9 de 1983, the schedule that stayed). The table also
## reports 1967, 1974 and 1982 B (Decreto 3743, the short-lived December 1982
## emergency decree, mostly struck down); not transcribed here.

source("Code/Thesis/001-setup.R")

source("Code/Thesis/ch03-cuadro-III1-data.R")
cuadro <- cuadro_III1 %>%
    mutate(change_pp = rate_1983 - rate_1982A,
           change_pct = 100 * change_pp / rate_1982A)

write.csv(cuadro, file.path(PRODUCTS_DIR, "PerryCardenas1986-CuadroIII1.csv"), row.names = FALSE)

avg_pp <- mean(cuadro$change_pp)
avg_pct_ex200 <- mean(cuadro$change_pct[cuadro$income > 200])
cat(sprintf("Average change: %.2f pp (all brackets); %.1f%% relative, excluding 200k\n", avg_pp, avg_pct_ex200))

tbl <- cuadro %>%
    transmute(
        `Taxable income (thousand 1982 pesos)` = format(income, big.mark = ","),
        `1982 schedule` = sprintf("%.2f\\%%", rate_1982A),
        `1983 schedule` = sprintf("%.2f\\%%", rate_1983),
        `Change (pp)` = sprintf("%.2f", change_pp),
        `Change (\\%)` = sprintf("%.1f", change_pct)
    ) %>%
    bind_rows(tibble(
        `Taxable income (thousand 1982 pesos)` = "Average",
        `1982 schedule` = "", `1983 schedule` = "",
        `Change (pp)` = sprintf("%.2f", avg_pp),
        `Change (\\%)` = sprintf("%.1f\\textsuperscript{a}", avg_pct_ex200)
    ))

tt_obj <- tbl |>
    tt(width = c(2.2, 1, 1, 1, 1),
       notes = list(
           "Average income-tax rates for an individual declarant with three dependants. 1982: Decreto 2809; 1983: Decreto 397 (Ley 9 de 1983). Source: Perry and C\\'ardenas (1986), vol. 1, Cuadro III.1.",
           a = "Excluding the lowest bracket, whose rate falls to almost zero (-95.3\\%)."
       )) |>
    style_tt(i = nrow(tbl), bold = TRUE, line = "t", line_width = 0.05) |>
    style_tt("notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch03-income-tax-1983")
cat("Saved: Thesis/tables/ch03-income-tax-1983.{png,pdf}\n")
