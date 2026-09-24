## Transcription of Perry and Cardenas (1986), "Diez anos de reformas tributarias
## en Colombia", Fedesarrollo, vol. 1, Cuadro III.1 "Tarifas promedio de impuesto
## sobre la renta", PDF p. 70 (Lit-Papers/PerryCardenas1986-DiezAnosReformasTributarias-v1.pdf).
## Transcribed by hand 2026-09-23 and checked against the page by Hans (the OCR
## misread the 1983 rate at 1,000 as 24.55; the page says 24.85).
## Taxable income in thousands of 1982 pesos; individual declarant with dependants.
## 1982 A = Decreto 2809 (schedule in force for 1982); 1983 = Decreto 397
## (Ley 9 de 1983, the schedule that stayed). Not transcribed: 1967, 1974 and
## 1982 B (Decreto 3743, the short-lived December 1982 emergency decree).
## Sourced by ch03-income-tax-1983-table.R and ch03-marginal-tax-1983-table.R.
cuadro_III1 <- tibble::tibble(
    income     = c(200, 300, 400, 500, 600, 800, 1000, 1500, 2000),
    rate_1982A = c(5.14, 10.03, 15.65, 17.45, 21.01, 26.28, 30.06, 35.61, 39.03),
    rate_1983  = c(0.24, 6.92, 10.14, 12.80, 15.83, 21.03, 24.85, 31.46, 35.30)
)
