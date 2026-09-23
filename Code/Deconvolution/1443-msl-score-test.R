## Robust score (LM) test of H0: theta=(lambda,d0,d1,d2)=theta_0 for the MSL likelihood (real data, lag_m, Version A, 28,892 interior firm-periods), 2026-09-19.
## No maximum needed: sigma_psi is profiled at theta_0 (restricted MLE); per-firm scores of (lambda,d0,d1,d2,ln sigma) by central differences of the C++ per-firm
## log-lik; efficient (Neyman C(alpha)) score e_i = S_theta,i - C S_sigma,i, C = OLS of S_theta on S_sigma; T = (sum e)' [ sum_clusters (sum_{i in c} e_i)(.)' ]^{-1} (sum e) ~ chi2_4,
## clustered by plant. Also the non-clustered OPG version. Sanity: at the MSL maximum T ~ 0.
suppressMessages(library(dplyr))
f <- "Code/Products/msl/1401-msl-input-lag_m-trim0.005.csv"; d <- read.csv(f)
## plant ids: replicate 1401-msl-export.R's row order (base -> interior -> trim), verify against the CSV
load("Code/Products/1200-stage2-data.RData"); b <- subset(stage2_data, ins == "lag_m")
it <- b %>% dplyr::filter(!corp, is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0)
it <- it %>% dplyr::filter(M_star <= quantile(it$M_star, 1 - 0.005, na.rm = TRUE))
stopifnot(nrow(it) == nrow(d), max(abs(it$cal_V - d$V)) < 1e-8); cl <- it$plant
cat(sprintf("n=%d firm-periods, %d plants (clusters)\n", nrow(d), length(unique(cl))))
ll_i <- function(par) { o <- tempfile(fileext = ".csv")
    system2("Code/C-estimator/msl_estimator", c(paste0("data=", f), "mode=eval", paste0("par=", paste(sprintf("%.12g", par), collapse = ",")), "threads=10", paste0("out=", o)), stdout = FALSE)
    x <- read.csv(o); unlink(o); x[[ncol(x)]] }
nll <- function(par) -sum(ll_i(par))
score_test <- function(th, sig = NULL, label) {
    if (is.null(sig)) sig <- exp(optimize(function(ls) nll(c(th, ls)), c(log(2), log(1000)), tol = 1e-4)$minimum)
    p0 <- c(th, log(sig)); h <- c(th[1] * 1e-3, 1e-3, 1e-3, 1e-3, 1e-3)
    S <- sapply(1:5, function(j) { pp <- pm <- p0; pp[j] <- pp[j] + h[j]; pm[j] <- pm[j] - h[j]; (ll_i(pp) - ll_i(pm)) / (2 * h[j]) })
    St <- S[, 1:4]; Ss <- S[, 5]; Cc <- colSums(St * Ss) / sum(Ss^2); E <- St - outer(Ss, Cc); E <- sweep(E, 2, apply(E, 2, sd), "/")   # efficient score; columns rescaled (T is invariant; avoids a singular solve when lambda is tiny)
    g <- colSums(E); Gc <- rowsum(E, cl); Vcl <- crossprod(Gc); Vop <- crossprod(E)
    Tcl <- drop(t(g) %*% solve(Vcl, g)); Top <- drop(t(g) %*% solve(Vop, g))
    cat(sprintf("%-7s theta=(%.4g, %.4f, %.4f, %.4f) sigma_psi(profiled)=%.3f | score sums = %s | T_robust(plant) = %.1f, T_OPG = %.1f vs chi2_{4,.95} = %.2f -> %s\n",
        label, th[1], th[2], th[3], th[4], sig, paste(signif(g, 3), collapse = " "), Tcl, Top, qchisq(.95, 4), ifelse(Tcl > qchisq(.95, 4), "REJECT", "fail to reject")))
    invisible(c(Tcl, Top)) }
score_test(c(2.724114e-05, 7.417881, 3.243078, 0.233465), sig = 1.694046, "MSL")
score_test(c(5.427e-7, 3.46356, 4.3, 0.54), label = "ELVIS")
score_test(c(6.847224e-4, -5.423294, 14.9643, 3.424686), label = "MSM")
