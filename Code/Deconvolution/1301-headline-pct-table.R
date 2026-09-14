## Headline counterfactual table: statistically guaranteed revenue impact of
## the two confirmed hard-test results -- an 8% tax CUT (the smallest cut
## distinguishable from baseline) vs. a 0.5% tax INCREASE (already
## distinguishable from baseline). Percentage ranges are computed
## conservatively from the hard-test confidence intervals themselves (the
## target's own bound closest to baseline, against baseline's own bound on
## that side) -- not just a point-estimate difference. Point estimates
## (lowest Lhat per Delta) also reported for reference. 2026-09-14.

library(tidyverse)
library(tinytable)
source("Code/Deconvolution/050-render-tbls.R")

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)

df <- read.csv("Code/Products/1300-cv-16delta-final.csv") %>%
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
    ## conservative range: target's own bound closest to baseline vs baseline's own bound on that side
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

write.csv(tbl, "Code/Products/1301-headline-pct-table.csv", row.names = FALSE)
print(tbl)

## LaTeX renders a raw "%" as a comment character (swallows the rest of the
## line) -- hit directly: it silently merged all three table rows into one
## on first render. Escape every literal "%" as "\%" ONLY in the copy that
## goes to LaTeX; the CSV above keeps plain "%" for readability/reuse.
tbl_tex <- tbl %>% mutate(across(everything(), ~ gsub("%", "\\\\%", .x)))

render_png_tt_tbl(tbl_tex |> tt(caption = "Statistically distinguishable revenue impact: an 8\\% cut vs. a 0.5\\% increase (hard test, 95\\%)"),
                   "1301-headline-pct-table")
cat("Saved: Paper/tbls/1301-headline-pct-table.png\n")
