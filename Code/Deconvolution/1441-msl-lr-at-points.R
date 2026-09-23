## MSL likelihood-ratio test at candidate points (real data, lag_m, Version A normal f_omega/f_eps, 28,892 interior firms).
## H0: theta=(lambda,d0,d1,d2)=theta_0 (4 restrictions, sigma_psi profiled out) vs. the MSL maximum: LR = 2*(nll(theta_0, best sigma) - nll_hat) ~ chi2_4 under a correctly specified likelihood.
f <- "Code/Products/msl/1401-msl-input-lag_m-trim0.005.csv"
nll <- function(par) { o <- system2("Code/C-estimator/msl_estimator", c(paste0("data=", f), "mode=eval", paste0("par=", paste(sprintf("%.10g", par), collapse = ",")), "threads=3"), stdout = TRUE)
    as.numeric(sub("-logL = ", "", grep("^-logL", o, value = TRUE))) }
best <- c(2.724114e-05, 7.417881, 3.243078, 0.233465, log(1.694046)); nll_hat <- 52923.658181
cat(sprintf("MSL max: -logL = %.3f (check eval: %.3f)\n", nll_hat, nll(best)))
pts <- list(ELVIS = c(5.427e-7, 3.46356, 4.3, 0.54), MSM = c(6.847224e-4, -5.423294, 14.9643, 3.424686))
for (nm in names(pts)) { th <- pts[[nm]]; o <- optimize(function(ls) nll(c(th, ls)), c(-2, 3), tol = 1e-3)
    lr <- 2 * (o$objective - nll_hat); cat(sprintf("%-6s theta=(%.3g, %.3f, %.3f, %.3f): profiled -logL = %.3f at sigma_psi = %.3f;  LR = %.1f vs chi2_{4,.95} = %.2f -> %s\n", nm, th[1], th[2], th[3], th[4], o$objective, exp(o$minimum), lr, qchisq(.95, 4), ifelse(lr > qchisq(.95, 4), "REJECT", "fail to reject"))) }
