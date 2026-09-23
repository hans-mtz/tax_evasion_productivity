## PRODUCT: Thesis/tables/ch08-headline-pct.png := statistically distinguishable revenue
## impact, 8% cut vs. 0.5% increase (hard test, 95%).
## Ported from Code/Deconvolution/1301-headline-pct-table.R (that script still feeds the
## old Paper/tbls/ pipeline for slides -- left untouched; this is a clean copy targeting
## the Thesis/JMP pipeline, same logic, same source CSV). Fixes the stale-PNG DPI-tagging
## bug (2026-09-22): the old copy in Thesis/tables/ predates 050-render-tbls.R's
## -units PixelsPerInch fix and was never regenerated -- confirmed via `identify`,
## units=Undefined at 300x300 vs. a freshly-rendered table's units=PixelsPerCentimeter.
source("Code/Thesis/001-setup.R")

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)

df <- read.csv(file.path(PRODUCTS_DIR, "1300-cv-16delta-final.csv")) %>%
    mutate(R_round = round(R, 4)) %>% distinct(Delta, R_round, .keep_all = TRUE) %>% select(-R_round) %>%
    mutate(TS_hard = 2 * n * Lhat, pass_hard = TS_hard <= qc)

bounds <- df %>% group_by(Delta) %>%
    summarise(lower = min(R[pass_hard]), upper = max(R[pass_hard]), .groups = "drop")
pts <- df %>% group_by(Delta) %>% slice_min(Lhat, n = 1) %>% ungroup() %>% select(Delta, R)

base <- bounds %>% filter(Delta == 0)
p0 <- pts %>% filter(Delta == 0) %>% pull(R)

mk_row <- function(delta, label) {
    b <- bounds %>% filter(Delta == delta)
    p <- pts %>% filter(Delta == delta) %>% pull(R)
    pct_a <- (b$lower - base$upper) / base$upper * 100
    pct_b <- (b$upper - base$lower) / base$lower * 100
    lo <- min(pct_a, pct_b); hi <- max(pct_a, pct_b)
    tibble(
        Scenario = label,
        `Tax change` = sprintf("%s%.1f%%", ifelse(delta > 0, "+", ""), delta * 100),
        `Revenue CI (real COP)` = sprintf("[%.0f, %.0f]", b$lower, b$upper),
        `Statistically guaranteed change` = sprintf("%+.2f%% to %+.2f%%", lo, hi),
        `Point-estimate change` = sprintf("%+.2f%%", (p - p0) / p0 * 100)
    )
}

tbl <- bind_rows(
    tibble(Scenario = "Baseline (current policy)", `Tax change` = "0.0%",
           `Revenue CI (real COP)` = sprintf("[%.0f, %.0f]", base$lower, base$upper),
           `Statistically guaranteed change` = "--", `Point-estimate change` = "--"),
    mk_row(-0.08, "Tax cut (smallest statistically distinguishable)"),
    mk_row(0.005, "Tax increase (smallest tested)")
)
print(tbl)

## LaTeX reads a raw "%" as a comment marker -- escape only the LaTeX-bound copy.
tbl_tex <- tbl %>% mutate(across(everything(), ~ gsub("%", "\\\\%", .x)))

## width=1: project default. No caption= (Quarto's own ![]{#tbl-x} is the single
## caption source) -- the real content that was in caption= moved to notes=.
tt_obj <- tbl_tex |> tt(width = 1, notes = "Statistically distinguishable revenue impact: an 8\\% cut vs. a 0.5\\% increase (conservative test, 95\\%).")

render_thesis_table(tt_obj, "ch08-headline-pct")
cat("Saved: Thesis/tables/ch08-headline-pct.{png,pdf}\n")
