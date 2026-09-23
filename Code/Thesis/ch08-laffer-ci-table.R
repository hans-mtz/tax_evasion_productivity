## PRODUCT: Thesis/tables/ch08-laffer-ci.png := real mean revenue per firm-period, 95%
## hard-test CIs, all 15 tested Delta points.
## Replaces the markdown table in Thesis/chapters/08-counterfactual.qmd's Results section
## (2026-09-22 open item) with a generated PNG, same convention as every other ch08 table.
## Source: Code/Products/1300-cv-16delta-final.csv (same file ch08-headline-pct-table.R
## uses for its two headline rows) -- this table just reports all 15, wide by Delta.
source("Code/Thesis/001-setup.R")

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)

df <- read.csv(file.path(PRODUCTS_DIR, "1300-cv-16delta-final.csv")) %>%
    mutate(R_round = round(R, 4)) %>% distinct(Delta, R_round, .keep_all = TRUE) %>% select(-R_round) %>%
    mutate(TS_hard = 2 * n * Lhat, pass_hard = TS_hard <= qc)

bounds <- df %>% group_by(Delta) %>%
    summarise(lower = min(R[pass_hard]), upper = max(R[pass_hard]), .groups = "drop") %>%
    arrange(Delta)

delta_order <- c(-0.08, -0.07, -0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05)
bounds <- bounds %>% mutate(Delta = factor(Delta, levels = delta_order)) %>% arrange(Delta)
## Escape "%" for LaTeX -- these become column headers, same trap as the notes= string above
## (a raw "%" is a LaTeX comment marker and silently swallows the rest of the line). Whole
## numbers drop the decimal (14 of 15 points) so headers are short enough not to wrap onto
## two lines in a 16-column table; only +0.5% keeps one decimal.
pct <- delta_order * 100
## Floating-point trap caught directly: -0.07*100 isn't exactly -7.0 in double precision, so
## a bare `pct == round(pct)` missed it and left "-7.0%" wrapping onto two lines while every
## other whole-number Delta got the short "-N%" form. Tolerance-based check instead.
is_whole <- abs(pct - round(pct)) < 1e-8
delta_labels <- ifelse(
    is_whole,
    sprintf("%s%d\\%%", ifelse(pct > 0, "+", ""), as.integer(round(pct))),
    sprintf("%s%.1f\\%%", ifelse(pct > 0, "+", ""), pct)
)

## Wide, one column per Delta, two rows (lower/upper) -- matches the markdown table's own
## orientation exactly, just as a rendered PNG instead of a raw pipe table.
tbl <- as.data.frame(t(round(cbind(bounds$lower, bounds$upper))))
colnames(tbl) <- delta_labels
tbl <- cbind(` ` = c("Lower", "Upper"), tbl)
print(tbl)

## width=1, no caption= (project defaults; the real caption text lives in the chapter's own
## ![]{#fig-x} markdown).
## 16 columns is a lot for one page width -- explicit alignment (default alignment was
## letting the label column crowd directly into the first data column, "Lower1751" with no
## gap) and a smaller body font (still legible, gives every cell breathing room) fix it.
tt_obj <- tt(tbl, align = paste0("l", strrep("c", 15)), width = c(0.8, rep(1, 15)),
             notes = "Real mean revenue per firm-period (COP), 95\\% conservative-test (Theorem F.1) confidence sets -- tested-grid min/max of passing points at each $\\Delta$.") |>
    style_tt(fontsize = 0.8) |>
    style_tt(i = "notes", fontsize = 0.65)

render_thesis_table(tt_obj, "ch08-laffer-ci")
cat("Saved: Thesis/tables/ch08-laffer-ci.{png,pdf}\n")
