## PRODUCT: Thesis/tables/ch08-headline-estimates.png := stage-2 ELVIS estimates at the operating point.
## Source: Code/Products/1616-i-k0.75-kappa0.5.csv (fit) and its adiag at R = 1000 and R = 4000
## (1616-i-k0.75-kappa0.5-adiag-R{1000,4000}.txt): design i, power detection q = (e/(kappa Mbar))^k with
## (k, kappa) = (0.75, 0.5) fixed, 0.5% trim of interior firms by M*, 17 moment rows (rows 1, 5, 7, 12 dropped),
## cut=ak, IS + mix proposal, plant clusters, seed 30. Rebuilt 2026-10-03 (Hans); the single-tax version of this
## table (1288 cube, lambda grid) is in git history.
source("Code/Thesis/001-setup.R")

fit <- read.csv(file.path(PRODUCTS_DIR, "1616-i-k0.75-kappa0.5.csv"))
adiag_ts <- function(R) {
    x <- readLines(file.path(PRODUCTS_DIR, sprintf("1616-i-k0.75-kappa0.5-adiag-R%d.txt", R)))
    l <- grep("^Lhat \\(recomputed\\)", x, value = TRUE)[1]
    c(TS = as.numeric(sub(".*TS = 2 n Lhat = ([0-9.]+).*", "\\1", l)),
      n = as.numeric(sub(".*n = ([0-9]+).*", "\\1", l)))
}
a1 <- adiag_ts(1000); a4 <- adiag_ts(4000)
x <- readLines(file.path(PRODUCTS_DIR, "1616-i-k0.75-kappa0.5-adiag-R1000.txt"))
eig <- grep("^ +[0-9]+ +[0-9.e+-]+ +(kept|dropped) ", x, value = TRUE)
kept <- sum(grepl(" kept ", eig)); dg <- length(eig)
gam <- unlist(fit[1, grep("^gamma", names(fit))])
stopifnot(a1["n"] == fit$n, dg == 17)

crit <- qchisq(0.95, dg)
f3 <- function(v) formatC(v, format = "f", digits = 3)
f2 <- function(v) formatC(v, format = "f", digits = 2)
tbl <- tibble(
    ` ` = c("$k$ (fixed)", "$\\kappa$ (fixed)", "$\\hat\\delta_0$", "$\\hat\\delta_1$", "$\\hat\\delta_2$",
            "$\\hat\\omega^*=\\hat\\delta_1/2\\hat\\delta_2$",
            "Firm-years ($n$)", "Moment rows ($d_g$)", "Directions kept", "$\\max\\vert\\hat\\gamma\\vert$",
            "$TS_{\\text{cons}}$ ($R=1000$)", "$TS_{\\text{cons}}$ ($R=4000$)",
            sprintf("$\\chi^2_{%d,.95}$", dg), "Result"),
    Estimate = c(f2(fit$k_hat), f2(fit$kappa_hat), f3(fit$delta0_hat), f3(fit$delta1_hat), f3(fit$delta2_hat),
                 f3(fit$delta1_hat / (2 * fit$delta2_hat)),
                 formatC(fit$n, format = "d", big.mark = ","), dg, sprintf("%d of %d", kept, dg),
                 formatC(max(abs(gam)), format = "f", digits = 1),
                 f2(a1["TS"]), f2(a4["TS"]), f2(crit), ifelse(a1["TS"] < crit, "Passes", "Fails"))
)
print(tbl)

tt_obj <- tbl |> tt(width = 0.55, notes = paste0(
    "Interior firms in the nine industries where the test rejects, top 0.5\\% by $M^*$ trimmed. ",
    "Detection $q=(e/\\kappa\\bar M_{j,t-1})^k$ with $(k,\\kappa)$ fixed at the operating point; ",
    "$R$ = draws per firm. Directions kept = eigenvalues of $\\hat\\Omega$ retained in the objective.")) |>
    style_tt(i = nrow(tbl), j = 2, bold = TRUE) |>
    style_tt(i = 6, line = "b", line_width = 0.05) |>
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(tt_obj, "ch08-headline-estimates")
cat("Saved: Thesis/tables/ch08-headline-estimates.{png,pdf}\n")
