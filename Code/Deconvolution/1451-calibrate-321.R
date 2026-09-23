## SMM calibration of the evasion block (lambda, delta0, delta1, delta2, theta_psi) of the 321 DGP to the OBSERVED distribution of V (2026-09-19).
## Targets (unincorporated interior firms, tau_P>0): V quantiles (5,10,25,50,75,90,95) and mean V, weighted by 1/SE^2 (bootstrap SE). corr(V,W), corr(V,ln M*), corr(V,ln tau) are NOT
## targeted: the corps (V=-eps) already show cor(V,W)=+0.39 (model implies -0.5) and cor(V,lnM*)=+0.31, so evasion parameters cannot be what drives them; tau_P is also built from M*.
## Usage: Rscript Code/Deconvolution/1451-calibrate-321.R N=3000 psi_k=1 nstart=6
suppressMessages({library(dplyr); source("Code/Deconvolution/utils-cli.R")}); source("Code/Deconvolution/1450-dgp-321.R")
opt <- parse_cli_args(list(N = 3000, psi_k = 1, nstart = 6, seed = 7, maxit = 1500)); N <- as.numeric(opt$N); pk <- as.numeric(opt$psi_k)
spec <- make_spec_321(); sh <- draw_shocks_321(spec, N, as.numeric(opt$seed))
load("Code/Products/1200-stage2-data.RData"); dd <- subset(stage2_data, ins == "lag_m" & sic_3 == 321 & !corp & sales_tax_rate_purchases > 0)
qs <- c(.05, .1, .25, .5, .75, .9, .95)
targ <- function(V, W, M, tau) c(quantile(V, qs, names = FALSE), mean(V))
t_data <- targ(dd$cal_V, dd$tilde_cal_W, dd$M_star, dd$sales_tax_rate_purchases)
set.seed(1); bs <- replicate(300, { j <- sample(nrow(dd), replace = TRUE); targ(dd$cal_V[j], dd$tilde_cal_W[j], dd$M_star[j], dd$sales_tax_rate_purchases[j]) }); se <- apply(bs, 1, sd)
nm <- c(paste0("q", qs * 100), "meanV")
pc_cur <- NULL   # nominal price constant; recalibrated below so the simulated interior mean ln M* matches the data (V is unaffected by pc, but e/M is)
sim_t <- function(ev) { d <- build_321(spec, sh, ev, pk, pc = pc_cur); u <- d[!d$corp & d$tauP > 0, ]; list(t = targ(u$V, u$Wt, u$xstar, u$tauP), u = u, d = d) }
obj <- function(par) { ev <- c(exp(par[1]), par[2], par[3], par[4], exp(par[5])); if (any(!is.finite(ev)) || ev[1] > 0.05 || ev[1] < 1e-8) return(1e6)
    s <- tryCatch(sim_t(ev)$t, error = function(e) rep(NA, length(t_data))); if (any(!is.finite(s))) return(1e6); sum(((s - t_data) / se)^2) }
starts <- list(c(log(2e-4), 1.2, 2.78, 0.5, log(0.3)), c(log(1e-4), 0.5, 2.0, 0.3, log(0.5)), c(log(5e-4), 2.0, 3.0, 0.6, log(0.2)),
               c(log(1e-3), 1.0, 2.5, 0.4, log(0.4)), c(log(3e-5), 0.0, 1.5, 0.3, log(0.6)), c(log(2e-4), 2.5, 4.0, 0.7, log(0.25)))
best <- NULL
for (s0 in starts[seq_len(min(as.numeric(opt$nstart), length(starts)))]) {
    o <- optim(s0, obj, method = "Nelder-Mead", control = list(maxit = as.numeric(opt$maxit), reltol = 1e-9)); o <- optim(o$par, obj, method = "Nelder-Mead", control = list(maxit = as.numeric(opt$maxit), reltol = 1e-10))
    cat(sprintf("start -> obj=%.2f  lambda=%.3g d0=%.3f d1=%.3f d2=%.3f theta=%.3f\n", o$value, exp(o$par[1]), o$par[2], o$par[3], o$par[4], exp(o$par[5])))
    if (is.null(best) || o$value < best$value) best <- o
}
for (rd in 1:3) {   # alternate: fit evasion block | adjust pc so mean ln M* (interior) matches the data
    ev <- c(exp(best$par[1]), best$par[2], best$par[3], best$par[4], exp(best$par[5])); r0 <- sim_t(ev)
    if (is.null(pc_cur)) pc_cur <- r0$d$pc[1]
    pc_cur <- pc_cur + (1 - spec$beta) * (mean(log(dd$M_star)) - mean(log(r0$u$xstar)))
    o <- optim(best$par, obj, method = "Nelder-Mead", control = list(maxit = as.numeric(opt$maxit), reltol = 1e-10)); best <- o
    cat(sprintf("round %d: pc=%.4f obj=%.2f\n", rd, pc_cur, best$value))
}
ev <- c(exp(best$par[1]), best$par[2], best$par[3], best$par[4], exp(best$par[5])); r <- sim_t(ev)
cat("\n=== best fit ===\nlambda=", ev[1], " delta0=", ev[2], " delta1=", ev[3], " delta2=", ev[4], " theta_psi=", ev[5], " (psi_k=", pk, ", sd psi=", sqrt(pk) * ev[5], ")  chi2-type obj=", best$value, " on ", length(t_data), " targets\n")
tt <- data.frame(data = t_data, se = se, sim = r$t, z = (r$t - t_data) / se, row.names = nm); print(round(tt, 3))
u <- r$u; ev_ <- u[u$e > 0, ]
cat(sprintf("\nimplied (not targeted): share e>0 among interior = %.3f; E[e/M] = %.4f; E ln(1+e/M) = %.4f; median e/M | e>0 = %.4f; top1%% share of e = %.3f; max 2*lam*e = %.3f\n",
    mean(u$e > 0), mean(u$e / u$x), mean(log1p(u$e / u$x)), median(ev_$e / ev_$x), sum(sort(u$e, TRUE)[seq_len(ceiling(.01 * nrow(u)))]) / sum(u$e), max(2 * ev[1] * u$e)))
cat(sprintf("observables vs data: mean ln M* sim=%.3f data=%.3f; sd ln M* sim=%.3f data=%.3f; W mean sim=%.3f data=%.3f; W sd sim=%.3f data=%.3f\n",
    mean(log(u$xstar)), mean(log(dd$M_star)), sd(log(u$xstar)), sd(log(dd$M_star)), mean(u$Wt), mean(dd$tilde_cal_W), sd(u$Wt), sd(dd$tilde_cal_W)))
saveRDS(list(ev = ev, psi_k = pk, pc = pc_cur, spec = spec, obj = best$value, targets = data.frame(target = nm, data = t_data, se = se, sim = r$t)), sprintf("Code/Products/msl/1451-calib-321-psik%s.rds", opt$psi_k))
