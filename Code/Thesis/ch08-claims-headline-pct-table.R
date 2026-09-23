## PRODUCT: Thesis/tables/ch08-claims-headline-pct.png := statistically distinguishable
## change in deduction claims, 8% cut vs. 0.5% increase (hard test, 95%) -- Claims analog
## of ch08-headline-pct-table.R, now the counterfactual's headline (see
## ch08-claims-ci-hard-plot.R for the full rationale).
source("Code/Thesis/001-setup.R")

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)

theory_files <- c(
    "1306-theory-coarse-delta0.csv", "1306-theory-coarse-delta1.csv",
    "1306-theory-coarse-delta2.csv", "1306-theory-coarse-delta3.csv",
    "1306-theory-anchor.csv", "1306-theory-remaining-delta0.csv",
    "1306-theory-remaining-deltam08.csv", "1306-theory-remaining-deltap005.csv",
    "1306-theory-remaining-deltap01.csv", "1306-theory-remaining-deltap02.csv"
)
df <- do.call(rbind, lapply(theory_files, function(f) read.csv(file.path(PRODUCTS_DIR, f))))
df <- df %>% mutate(Delta = round(Delta, 4), Claims = -R) %>%
    mutate(Claims_round = round(Claims, 4)) %>% distinct(Delta, Claims_round, .keep_all = TRUE) %>%
    select(-Claims_round) %>%
    mutate(TS_hard = 2 * n * Lhat, pass_hard = TS_hard <= qc)

bounds <- df %>% group_by(Delta) %>%
    summarise(lower = min(Claims[pass_hard]), upper = max(Claims[pass_hard]), .groups = "drop")
pts <- df %>% group_by(Delta) %>% slice_min(Lhat, n = 1) %>% ungroup() %>% select(Delta, Claims)

base <- bounds %>% filter(Delta == 0)
p0 <- pts %>% filter(Delta == 0) %>% pull(Claims)

mk_row <- function(delta, label) {
    b <- bounds %>% filter(Delta == delta)
    p <- pts %>% filter(Delta == delta) %>% pull(Claims)
    pct_a <- (b$lower - base$upper) / base$upper * 100
    pct_b <- (b$upper - base$lower) / base$lower * 100
    lo <- min(pct_a, pct_b); hi <- max(pct_a, pct_b)
    tibble(
        Scenario = label,
        `Tax change` = sprintf("%s%.1f%%", ifelse(delta > 0, "+", ""), delta * 100),
        `Claims CI (real COP)` = sprintf("[%.0f, %.0f]", b$lower, b$upper),
        `Bounds on the change` = sprintf("%+.2f%% to %+.2f%%", lo, hi),
        `Point-estimate change` = sprintf("%+.2f%%", (p - p0) / p0 * 100)
    )
}

tbl <- bind_rows(
    tibble(Scenario = "Baseline (current policy)", `Tax change` = "0.0%",
           `Claims CI (real COP)` = sprintf("[%.0f, %.0f]", base$lower, base$upper),
           `Bounds on the change` = "--", `Point-estimate change` = "--"),
    mk_row(-0.08, "Tax cut (smallest that separates)"),
    mk_row(0.005, "Tax increase (smallest tested)")
)
print(tbl)

tbl_tex <- tbl %>% mutate(across(everything(), ~ gsub("%", "\\\\%", .x)))

tt_obj <- tbl_tex |> tt(width = c(2.5, 1.0, 1.5, 2.1, 1.8), notes = "Bounds compare the two 95\\% conservative-test confidence sets.")

render_thesis_table(tt_obj, "ch08-claims-headline-pct")
cat("Saved: Thesis/tables/ch08-claims-headline-pct.{png,pdf}\n")
