## Bracket-finder + bisection for the HARD (absolute Theorem F.1) test,
## TS_hard=2n*Lhat vs chi2(10,.95) -- no min-subtraction needed (unlike the
## soft test), since the hard threshold is a fixed absolute bound on Lhat
## itself, independent of what else is in the grid. Uses ALL points ever
## tested at each target Delta (coarse + any existing soft-targeted fine
## points) to find the reject->pass bracket on each side, then bisects ONCE
## per side toward the hard threshold. Restricted to the requested Delta
## subset (the ones with unresolved hard bounds from the earlier gap
## analysis): -7% through +2%, including +0.5%.
## Usage: Rscript 1299-make-hard-fine-grid.R <all_points.csv> <out_fine.csv>
## 2026-09-13.

suppressMessages(library(tidyverse))
args <- commandArgs(trailingOnly = TRUE)
all_path <- args[1]
out_path <- args[2]

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)

TARGET_DELTAS <- c(-0.07, -0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.005, 0.01, 0.02)

df <- read.csv(all_path) %>%
    mutate(R_round = round(R, 4)) %>% distinct(Delta, R_round, .keep_all = TRUE) %>% select(-R_round) %>%
    mutate(TS = 2 * n * Lhat, pass = TS <= qc) %>%
    filter(Delta %in% TARGET_DELTAS) %>% arrange(Delta, R)

fine_rows <- list()
for (d in TARGET_DELTAS) {
    sub <- df %>% filter(Delta == d) %>% arrange(R)
    n_sub <- nrow(sub)
    if (n_sub < 2) { cat(sprintf("Delta=%.3f: fewer than 2 tested points, skipping\n", d)); next }

    lower_bracket <- NULL
    for (i in 1:(n_sub - 1)) {
        if (!sub$pass[i] && sub$pass[i + 1]) { lower_bracket <- c(i, i + 1); break }
    }
    upper_bracket <- NULL
    for (i in n_sub:2) {
        if (!sub$pass[i] && sub$pass[i - 1]) { upper_bracket <- c(i - 1, i); break }
    }

    interp <- function(idx_pair) {
        r_lo <- sub$R[idx_pair[1]]; r_hi <- sub$R[idx_pair[2]]
        ts_lo <- sub$TS[idx_pair[1]]; ts_hi <- sub$TS[idx_pair[2]]
        r_lo + (r_hi - r_lo) * (qc - ts_lo) / (ts_hi - ts_lo)
    }

    if (!is.null(lower_bracket)) {
        fine_rows[[length(fine_rows) + 1]] <- tibble(Delta = d, target = interp(lower_bracket), side = "lower")
    } else {
        cat(sprintf("Delta=%6.3f: lower side OPEN (smallest tested R=%.2f, TS_hard=%.2f)\n",
                    d, sub$R[1], sub$TS[1]))
    }
    if (!is.null(upper_bracket)) {
        fine_rows[[length(fine_rows) + 1]] <- tibble(Delta = d, target = interp(upper_bracket), side = "upper")
    } else {
        cat(sprintf("Delta=%6.3f: upper side OPEN (largest tested R=%.2f, TS_hard=%.2f)\n",
                    d, sub$R[n_sub], sub$TS[n_sub]))
    }
}

fine_df <- bind_rows(fine_rows)
write.csv(fine_df, out_path, row.names = FALSE)
cat(sprintf("\nSaved %d hard-test fine-grid points to %s\n", nrow(fine_df), out_path))
print(fine_df)
