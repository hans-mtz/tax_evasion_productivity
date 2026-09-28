## PRODUCT: Thesis/tables/ch03-jo-rules.png := income-tax treatment, liability,
## capital and number of owners by juridical organization, under the rules in
## force from the 1974 reform until Ley 9 de 1983 (@tbl-jo-rules, Setting and
## Data chapter). Institutional facts, no data: this script is the tracked copy.
## Replaces the plain markdown table in 03-setting-data.qmd (2026-09-27, Hans:
## PNG for the house font, a short caption and notes under the table).
##
## SOURCES (checked 2026-09-27): rates, McLure (1989, p. 67) and Perry and Cardenas
## (1986), vol. 1, pp. 23 and 36; liability, capital and owners, Fiscal Survey
## of Colombia (1965), pp. 27-30, and DANE (2018), EAM 1992-1994 documentation,
## PDF p. 12 (Lit-Papers/DANE2018-EAM1992-1994-DDI.pdf).

source("Code/Thesis/001-setup.R")

tbl <- tibble::tribble(
    ~Organization, ~`Income tax (company)`, ~`Income tax (owners)`, ~Liability, ~Capital, ~Owners,
    "Corporation", "40\\%", "On dividends received", "Limited to capital contributed", "Tradable shares", "$N\\geq5$",
    "LLC", "20\\%", "On all profits", "Limited to capital contributed", "Non-tradable stakes (\\emph{cuotas})", "$2\\leq N\\leq20$ (25)",
    "Partnership", "20\\%", "On all profits", "Unlimited\\textsuperscript{a}", "Not a capital association", "$N\\geq2$",
    "Proprietorship", "--", "Individual schedule", "Unlimited", "Owner's own assets", "$N=1$"
)

tt_obj <- tbl |>
    tt(width = c(1.35, 1, 1.35, 1.6, 1.6, 1.5),
       notes = list(
           "Rules in force from the 1974 reform until Ley 9 de 1983, which cut the LLC rate to 18\\%. Company: entity-level income-tax rate. Owners: individual income tax; proprietors paid only the graduated individual schedule, with a top rate of 56\\%. The limit on LLC partners is 20 in the 1965 source and 25 in the 1992--1994 survey documentation. Sources: tax rates, McLure (1989), p. 67, and Perry and C\\'ardenas (1986), vol.~1, pp.~23 and 36; liability, capital and owners, Fiscal Survey of Colombia (1965), pp. 27--30, and DANE (2018), p. 12.",
           a = "All partners in general partnerships; the managing partners in ordinary limited partnerships."
       )) |>
    style_tt(j = c(1, 3:6), align = "l") |>
    style_tt(j = 2, align = "c") |>
    style_tt("notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch03-jo-rules")
cat("Saved: Thesis/tables/ch03-jo-rules.{png,pdf}\n")
