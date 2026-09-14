## Reads a combined COARSE (Delta,target) grid CSV (revgrid_fixedtheta's own
## output format) for one moment type (CV-adjusted R or Loss), computes
## TS_soft = 2*n*(Lhat - min(Lhat)) [global min over the WHOLE combined grid,
## same convention as 1263-stage2-revgrid-plot.R / the project's standing
## CHT-style test], finds the pass/reject bracket on each side of every
## Delta's own coarse points, and linearly interpolates (in TS space) a
## single fine-grid candidate per bracket targeting the chi-sq crossing.
## A side with NO bracket (all 5 coarse points on that side pass) is left
## "open" -- not fabricated, flagged for the final report instead.
## Usage: Rscript 1295-make-fine-grid.R <coarse_combined.csv> <out_fine.csv>
## 2026-09-12.

suppressMessages(library(tidyverse))
args <- commandArgs(trailingOnly = TRUE)
coarse_path <- args[1]
out_path <- args[2]

n <- 32232; dg <- 10
qc <- qchisq(0.95, dg)

df <- read.csv(coarse_path)
Lmin <- min(df$Lhat)
df <- df %>% mutate(TS = 2 * n * (Lhat - Lmin), pass = TS <= qc) %>% arrange(Delta, R)

fine_rows <- list()
for (d in sort(unique(df$Delta))) {
    sub <- df %>% filter(Delta == d) %>% arrange(R)
    n_sub <- nrow(sub)

    ## lower bracket: scan upward from smallest R for a reject->pass transition
    lower_bracket <- NULL
    for (i in 1:(n_sub - 1)) {
        if (!sub$pass[i] && sub$pass[i + 1]) { lower_bracket <- c(i, i + 1); break }
    }
    ## upper bracket: scan downward from largest R for a reject->pass transition
    upper_bracket <- NULL
    for (i in n_sub:2) {
        if (!sub$pass[i] && sub$pass[i - 1]) { upper_bracket <- c(i - 1, i); break }
    }

    interp <- function(idx_pair) {
        r_lo <- sub$R[idx_pair[1]]; r_hi <- sub$R[idx_pair[2]]
        ts_lo <- sub$TS[idx_pair[1]]; ts_hi <- sub$TS[idx_pair[2]]
        ## whichever endpoint is the REJECT one (TS>qc) vs PASS (TS<=qc)
        r_lo + (r_hi - r_lo) * (qc - ts_lo) / (ts_hi - ts_lo)
    }

    if (!is.null(lower_bracket)) {
        fine_rows[[length(fine_rows) + 1]] <- tibble(Delta = d, target = interp(lower_bracket), side = "lower")
    } else {
        cat(sprintf("Delta=%.2f: lower side OPEN (smallest tested R=%.2f already passes, TS=%.2f)\n",
                    d, sub$R[1], sub$TS[1]))
    }
    if (!is.null(upper_bracket)) {
        fine_rows[[length(fine_rows) + 1]] <- tibble(Delta = d, target = interp(upper_bracket), side = "upper")
    } else {
        cat(sprintf("Delta=%.2f: upper side OPEN (largest tested R=%.2f already passes, TS=%.2f)\n",
                    d, sub$R[n_sub], sub$TS[n_sub]))
    }
}

fine_df <- bind_rows(fine_rows)
write.csv(fine_df, out_path, row.names = FALSE)
cat(sprintf("\nSaved %d fine-grid points to %s\n", nrow(fine_df), out_path))
print(fine_df)
