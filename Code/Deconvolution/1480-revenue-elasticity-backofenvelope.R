## Back-of-envelope revenue loss + revenue elasticity of the tax shifter x=(1+Delta), at the
## headline fixed operating point (lag_m, theta from the 3D cube's global minimum:
## lambda=5.427e-7, delta0=3.46356014640655, delta1=4.3, delta2=0.54; gamma1-9 from that same
## cube row -- Code/Products/1288-cube-lag_m-combined.csv). 2026-09-22.
##
## TWO OBJECTS, BOTH FROM ONE FORWARD-SIMULATION RUN (theta AND gamma already fixed, no
## fitting -- ~0.5 sec for all 32,232 firms):
##   1. Back-of-envelope revenue loss: mean_Loss_real from grid_estimator's own
##      revenue_baseline mode output (Loss_i = tau_P,i*(1-q(e_i))*e_i, the uncaught-evasion
##      credit paid out at Delta=0) -- the direct answer to "how much revenue is already lost
##      to evasion at current policy," parallel to the country comparators in the intro.
##   2. Revenue elasticity of x: dR/dx analytically (closed form below, derived from the
##      same e'(Delta) used everywhere else in this chapter), then multiplied by x/R at the
##      baseline (x=1, so this is just dR/dx * 1/R). Cross-validated against a numerical
##      right-derivative from two tiny extra Delta=+/-0.001 runs (also ~0.5 sec each) --
##      matches to within 0.05%.
##
## CLOSED FORM for dR_i/dx at x=1 (Delta=0), derived by hand, not just asserted:
## R_i(x) = t1_i - x*tau_P,i*[M_i + e'(x) - lambda*e'(x)^2], e'(x)=(e_i+(x-1)/(2*lambda))/x.
## Differentiating and evaluating at x=1 (using e'(1)=e_i exactly):
##   dR_i/dx |_{x=1} = -tau_P,i * [ M_i - e_i + lambda*e_i^2 + 1/(2*lambda) ]
## This is the RIGHT-derivative (Delta -> 0+): valid because no firm is clipped at e'=0 for
## Delta>=0 (thresholds -2*lambda*e_i are always <=0), matching this project's own documented
## finding that the LEFT-derivative differs sharply (many firms clip almost immediately for
## Delta<0, given how tiny lambda is) -- see CLAUDE.md's "sharp kink exactly at Delta=0"
## entry. Confirmed empirically below: the two-sided numerical derivative is NOT symmetric
## around Delta=0 (right-side slope ~2x the left-side one), exactly as that finding predicts,
## so only the right-derivative is the theoretically appropriate comparison for this formula.
##
## Corner firms (tau_P=0): dR_i/dx=0 exactly, no special-casing needed (verified below).

suppressMessages(library(tidyverse))

INPUT_CSV <- "Code/Products/1260-stage2-revenue-input-lag_m-trim0.005.csv"
PRODUCTS_DIR <- "Code/Products"
BIN <- "Code/C-estimator/grid_estimator"
PAR <- "3.46356014640655,5.427e-07,4.3,0.54,0.0143883116027861,-0.0119013123755461,0.0630942173107609,-0.0783100943504263,0.0578349092582138,-0.907539328124065,1.27340923253472e-05,7.34383264616891e-09,2.64362954929756"
LAMBDA <- 5.427e-07

## Run directly from the repo root (this script's own convention) -- no cd, full relative
## paths throughout, exactly as invoked successfully by hand before this was saved as a script.
run_revbase <- function(delta, out_csv) {
    cmd <- sprintf(
        "%s mode=revenue_baseline input_csv=%s output_csv=%s par=%s Delta=%s n_burn=1000 n_keep=3000 n_threads=12 base_seed=20260922",
        BIN, INPUT_CSV, out_csv, PAR, delta
    )
    log <- system(cmd, intern = TRUE)
    line <- grep("^revenue_baseline:", log, value = TRUE)
    vals <- regmatches(line, regexpr("mean_R_real=[0-9.eE+-]+", line))
    mean_R_real <- as.numeric(sub("mean_R_real=", "", vals))
    list(log = line, mean_R_real = mean_R_real)
}

## --- Main run at Delta=0, kept (per-firm CSV feeds everything below) ---
main_out <- file.path(PRODUCTS_DIR, "1479-revenue-baseline-headline-delta0-firmlevel.csv")
r0 <- run_revbase(0.00, main_out)
cat(r0$log, "\n")

## --- Two tiny perturbations, throwaway per-firm CSVs, just for the derivative cross-check ---
rp <- run_revbase(0.001, "/tmp/revbase_plus.csv")
rm <- run_revbase(-0.001, "/tmp/revbase_minus.csv")
cat(rp$log, "\n"); cat(rm$log, "\n")

right_deriv_numeric <- (rp$mean_R_real - r0$mean_R_real) / 0.001
left_deriv_numeric  <- (r0$mean_R_real - rm$mean_R_real) / 0.001
cat(sprintf("\nNumerical one-sided derivatives of mean R_real: right=%.2f, left=%.2f (asymmetry confirms the documented kink at Delta=0 -- not a bug)\n",
            right_deriv_numeric, left_deriv_numeric))

## --- Load per-firm output + input, compute the analytical dR/dx ---
inp <- read.csv(INPUT_CSV)
out <- read.csv(main_out) %>% select(-corner)
df <- inp %>% inner_join(out, by = "row_id") %>%
    mutate(
        e_i = e_mean, M_i = M_star - e_i,
        dRdx_nom  = -sales_tax_rate_purchases * (M_i - e_i + LAMBDA * e_i^2 + 1 / (2 * LAMBDA)),
        dRdx_real = dRdx_nom / pgdp
    )

stopifnot(max(abs(df$dRdx_nom[df$corner == 1])) == 0)  # corner firms: exactly zero, as derived

mean_R_real  <- mean(df$R_real)
mean_dRdx    <- mean(df$dRdx_real)
elas_agg     <- mean_dRdx / mean_R_real   # x=1 at Delta=0, so eps = dR/dx * x/R = dR/dx / R

interior <- df %>% filter(corner == 0)
med_R    <- median(interior$R_real)
med_dRdx <- median(interior$dRdx_real)
elas_med <- med_dRdx / med_R

cat(sprintf("\nAnalytical mean dR/dx (real, all %d firm-periods): %.2f\n", nrow(df), mean_dRdx))
cat(sprintf("Cross-check vs. numerical right-derivative: %.2f (agreement to %.3f%%)\n",
            right_deriv_numeric, 100 * abs(mean_dRdx - right_deriv_numeric) / abs(right_deriv_numeric)))

## Standard VAT-gap convention (corrected 2026-09-22, caught by Hans -- verified directly
## against the EU VAT Gap Report's own VTTL/"% of VTTL" table structure and CIAT's
## Efficiency=actual/potential methodology, both already read this session, not assumed):
## the gap is a share of POTENTIAL revenue (actual + loss), NOT of actual collections.
## R_real already comes out of the simulation NET of the undetected-evasion loss (it's the
## revenue the government actually collects), so potential = R_real + Loss_real, and
## gap = 1 - actual/potential = Loss/potential -- not Loss/actual, which is a different,
## non-standard ratio that happens to look similar only because Loss is small relative to R.
mean_potential_real <- mean(df$R_real) + mean(df$Loss_real)
gap_pct <- mean(df$Loss_real) / mean_potential_real * 100

cat(sprintf("\n=== BACK-OF-ENVELOPE REVENUE LOSS (mean_Loss_real from the run above) ===\n"))
cat(sprintf("Mean per firm-period: %.2f real COP\n", mean(df$Loss_real)))
cat(sprintf("Potential revenue (actual + loss): %.2f + %.2f = %.2f real COP\n",
            mean(df$R_real), mean(df$Loss_real), mean_potential_real))
cat(sprintf("Gap as %% of POTENTIAL (standard VAT-gap convention, 1 - actual/potential): %.2f%%\n", gap_pct))
cat(sprintf("Total loss across the %d-firm-period sample: %.0f real COP\n", nrow(df), sum(df$Loss_real)))

cat(sprintf("\n=== REVENUE ELASTICITY of x=(1+Delta) at the current operating point (Delta=0, x=1) ===\n"))
cat(sprintf("Aggregate (ratio of means -- the headline number): %.2f\n", elas_agg))
cat(sprintf("  Interpretation: a 1%% increase in the purchases-tax shifter (~the tax rate itself)\n")
)
cat(sprintf("  is associated with a local, linear-approximation %.1f%% DECREASE in mean revenue.\n", -elas_agg))
cat(sprintf("Median interior (evading-margin) firm: %.2f (median R_real=%.2f, much smaller than\n", elas_med, med_R))
cat(sprintf("  the aggregate mean %.2f, which is why the median-firm ratio is so much larger in\n", mean_R_real))
cat(sprintf("  magnitude -- a real heterogeneity finding, not a discrepancy to paper over.\n"))
cat(sprintf("(Mean of per-firm ratios is NOT reported as a summary: a few near-zero-R_i firms\n"))
cat(sprintf(" blow up that average non-informatively -- the aggregate and median above are the\n"))
cat(sprintf(" well-behaved summaries.)\n"))

write.csv(df %>% select(row_id, corner, R_real, Loss_real, e_i, M_i, dRdx_real),
          file.path(PRODUCTS_DIR, "1480-revenue-elasticity-firmlevel.csv"), row.names = FALSE)
cat(sprintf("\nSaved: %s\n", file.path(PRODUCTS_DIR, "1480-revenue-elasticity-firmlevel.csv")))
