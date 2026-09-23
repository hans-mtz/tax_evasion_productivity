## PRODUCT: Thesis/tables/ch08-claims-ci.png := real mean deduction claims per firm-period,
## 95% hard-test CIs, all 9 tested Delta points (raw Claims moment, no control variate --
## see ch08-claims-ci-hard-plot.R for the full rationale for promoting this to headline).
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
    summarise(lower = min(Claims[pass_hard]), upper = max(Claims[pass_hard]), .groups = "drop") %>%
    arrange(Delta)

delta_order <- c(-0.08, -0.05, -0.04, -0.03, -0.02, 0, 0.005, 0.01, 0.02)
bounds <- bounds %>% mutate(Delta = factor(Delta, levels = delta_order)) %>% arrange(Delta)
pct <- delta_order * 100
is_whole <- abs(pct - round(pct)) < 1e-8
delta_labels <- ifelse(
    is_whole,
    sprintf("%s%d\\%%", ifelse(pct > 0, "+", ""), as.integer(round(pct))),
    sprintf("%s%.1f\\%%", ifelse(pct > 0, "+", ""), pct)
)

tbl <- as.data.frame(t(round(cbind(bounds$lower, bounds$upper))))
colnames(tbl) <- delta_labels
tbl <- cbind(` ` = c("Lower", "Upper"), tbl)
print(tbl)

tt_obj <- tt(tbl, align = paste0("l", strrep("c", 9)), width = c(0.8, rep(1, 9)),
             notes = "Real mean purchases-side deduction claims per firm-period (COP), 95\\% conservative-test (Theorem F.1) confidence sets -- tested-grid min/max of passing points at each $\\Delta$. Raw moment, no control variate.") |>
    style_tt(fontsize = 0.85) |>
    style_tt(i = "notes", fontsize = 0.65)

render_thesis_table(tt_obj, "ch08-claims-ci")
cat("Saved: Thesis/tables/ch08-claims-ci.{png,pdf}\n")
