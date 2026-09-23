## Export a simulated panel (1410-dgp-model.R) to the two estimator input formats (2026-09-18).
##   MSL  : Code/Products/msl/<tag>-msl-input.csv   (V,Wt,Mst,taur,beta,mu_eps,sd_eps,mu_om,sd_om,...)
##   ELVIS: Code/Products/msl/<tag>-elvis-input.csv (M_star,cal_V,tilde_cal_W,sales_tax_rate_purchases,beta,corner,row_id)
## Sample mirrors the real pipeline: unincorporated firms with a LAGGED observation (t>=2, as the lag_m
## instrument requires), interior = tau_P>0 with the top trim_top_pct by M* dropped, tau_P=0 firms as
## "corner" rows (ELVIS only; theta-free constants in MSL, so not exported there).
## beta and f_eps are the TRUE values (stage 1 taken as known, to isolate stage 2); f_omega is
## moment-deconvolved from W_tilde on ALL firms (same formula as 1401-msl-export.R).
## Usage: Rscript Code/Deconvolution/1411-sim-export.R rds=<path> tag=<name> trim_top_pct=0.005
suppressMessages(library(tidyverse))
source("Code/Deconvolution/utils-cli.R")
opt <- parse_cli_args(list(rds = "Code/Products/msl/1410-sim-base-lam1e-4-s070.rds", tag = "1411-base", trim_top_pct = 0.005))
opt$trim_top_pct <- as.numeric(opt$trim_top_pct)
log_run_header("1411-sim-export.R", opt)
sim <- readRDS(opt$rds); d <- sim$data; s <- sim$spec
d <- d %>% filter(!is.na(lag_mstar))                                   # lag available
sd_eps <- s$sd_eps; mu_eps <- -0.5 * sd_eps^2
bo <- 1 - s$beta
allw <- d                                                               # ALL firms (corp + non-corp)
mu_om <- mean(allw$Wt) - bo * mu_eps
sd_om <- sqrt(max(var(allw$Wt) - bo^2 * sd_eps^2, 0.05 * var(allw$Wt)))
cat(sprintf("f_omega (moment-deconvolved from Wt, all firms): mean %.3f sd %.3f   (true stationary: %.2f, %.2f)\n", mu_om, sd_om, s$mu_om, s$sd_om))
nc <- d %>% filter(!corp)
interior <- nc %>% filter(tauP > 0)
cut <- quantile(interior$xstar, 1 - opt$trim_top_pct); nb <- nrow(interior)
interior <- interior %>% filter(xstar <= cut)
cat(sprintf("interior firms: %d (trimmed %d with M*>%.4g); exempt tau_P=0 (corner): %d\n", nrow(interior), nb - nrow(interior), cut, sum(nc$tauP == 0)))
dir.create("Code/Products/msl", showWarnings = FALSE)
msl <- interior %>% transmute(V, Wt, Mst = xstar, taur = tauP, beta = s$beta, mu_eps = mu_eps, sd_eps = sd_eps,
                              mu_om = mu_om, sd_om = sd_om, sic_3 = sector, year = t, row_id = row_number())
write.csv(msl, sprintf("Code/Products/msl/%s-msl-input.csv", opt$tag), row.names = FALSE, quote = FALSE)
corner <- nc %>% filter(tauP == 0)
el <- bind_rows(interior %>% mutate(corner = 0L), corner %>% mutate(corner = 1L)) %>%
    transmute(M_star = xstar, cal_V = V, tilde_cal_W = Wt, sales_tax_rate_purchases = tauP, beta = s$beta, corner, row_id = row_number())
write.csv(el, sprintf("Code/Products/msl/%s-elvis-input.csv", opt$tag), row.names = FALSE, quote = FALSE)
cat(sprintf("saved MSL (%d rows) and ELVIS (%d rows: %d interior + %d corner) inputs, tag %s\n", nrow(msl), nrow(el), sum(el$corner == 0), sum(el$corner == 1), opt$tag))
