## PRODUCT: Code/Products/1603-np-deconv-stage2.RData (+ 1603-np-deconv-stage2-summary.csv) := deconvolution of
## u = ln(M*/M) on EXACTLY the stage-2 ELVIS sample, for all 9 interior industries (Hans, 2026-10-01).
##   Unincorporated firms = the stage-2 interior firm-periods of the industry (tau_P > 0, net share in 1501's cuts:
##   > 5%, and for 369 < 0.75, the 369 cut kept as in ELVIS; top 0.5% of interior M* trimmed as in 1532), rebuilt with
##   1532's export filters and checked against the ELVIS input (1598) by industry (n and mean V).
##   V and f_eps from the stage-2 first stage (1501: fs_net_ls) -- the same V the ELVIS moments use.
##   Estimator unchanged from 1521 (penalized B-spline logspline, same lambda/knot rule, same seed); only the sample
##   differs. The approved ch. 5 deconvolution (1521, the test's sample) is untouched.
## Diagnostic: deconvolved E[u] vs mean V on the same sample (equal up to estimation error, since E[eps] = 0).
## Run order = share of tau_P = 0 firms dropped (largest first); all run in parallel.
suppressPackageStartupMessages({ library(dplyr); library(splines); library(statmod); library(parallel) })
load("Code/Products/1532-stage2-data-final.RData")   # stage2_final, evaders
load("Code/Products/1501-fs-net.RData")              # fs_net_ls (stage-2 first stage, 369 cut applied)
fenv <- new.env(); load("Code/Products/np-deconv-funs.RData", envir = fenv)
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv

## stage-2 interior set, exactly as 1532's export("designA")
trim_top_pct <- 0.005
b <- stage2_final %>% filter(!corp) %>%
    mutate(use_c = evader, cal_V = if_else(use_c, cal_V_c, cal_V_u), tilde_cal_W = if_else(use_c, tilde_cal_W_c, tilde_cal_W_u),
           beta = if_else(use_c, beta_c, beta_u), corner = as.integer(sales_tax_rate_purchases == 0 | !use_c)) %>%
    filter(is.finite(cal_V), is.finite(tilde_cal_W), is.finite(beta), is.finite(sales_tax_rate_purchases),
           sales_tax_rate_purchases >= 0, is.finite(M_star), M_star > 0, is.finite(t1), is.finite(pgdp))
cut <- quantile(b$M_star[b$corner == 0], 1 - trim_top_pct)
b <- b %>% filter(corner == 1 | M_star <= cut)
keys <- b %>% filter(corner == 0) %>% transmute(sic_3 = as.character(sic_3), plant = as.character(plant), year = as.character(year), V_s2 = cal_V)

inp <- read.csv("Code/Products/1598-stage2-input-designA-interior-plant-k-trim0.005.csv", colClasses = c(sic_3 = "character"))
chk_in <- inp %>% group_by(sic_3) %>% summarise(n_elvis = n(), EV_elvis = mean(cal_V), .groups = "drop")
order_ind <- c("331", "369", "351", "313", "352", "342", "321", "322", "324")

run_one <- function(s) {
    set.seed(557788)
    fs <- fs_net_ls[[s]]
    k_s <- keys %>% filter(sic_3 == s)
    fs_u <- fs; fs_u$inter <- "log_mats_share_net"
    fs_u$data <- fs$data %>% mutate(plant = as.character(plant), year = as.character(year)) %>%
        inner_join(k_s %>% select(plant, year, V_s2), by = c("plant", "year"))
    stopifnot(nrow(fs_u$data) == nrow(k_s), isTRUE(all.equal(fs_u$data$cal_V, fs_u$data$V_s2, tolerance = 1e-10)))
    fs_u$data <- fs_u$data %>% select(-V_s2)
    cat(s, ": uninc", nrow(fs_u$data), "| beta", round(fs$beta, 3), "| mean V", round(mean(fs_u$data$cal_V), 4), "\n")
    fit <- fenv$estimate_np_theta(fs_u, fenv$np_pdf(fs), fenv$gl, lambda = fenv$lambda, parallel = FALSE)
    p <- fit$params; g <- seq(p$a, p$b, length.out = 20001); f <- pmax(fenv$f_e.np(g, fit$theta, p), 0)
    cdf <- cumsum(f); cdf <- cdf / max(cdf); dx <- diff(g)[1]; Eu <- sum(g * f) * dx / sum(f * dx)
    list(fit = fit, n = nrow(fs_u$data), mean_V = mean(fs_u$data$cal_V), sd_V = sd(fs_u$data$cal_V), beta = fs$beta,
         sd_eps = fs$epsilon_sigma, convergence = fit$opt$convergence,
         E_u = Eu, med_u = g[which(cdf >= 0.5)[1]], p_u05 = cdf[which(g >= 0.05)[1]],
         sd_u = sqrt(sum((g - Eu)^2 * f) * dx / sum(f * dx)), support = c(p$a, p$b))
}
res <- mclapply(order_ind, run_one, mc.cores = length(order_ind)); names(res) <- order_ind
ok <- !sapply(res, inherits, "try-error")
summ <- bind_rows(lapply(order_ind[ok], function(s) with(res[[s]], tibble(sic_3 = s, n = n, beta = beta, sd_eps = sd_eps, mean_V = mean_V, sd_V = sd_V,
                E_u = E_u, gap_Eu_minus_EV = E_u - mean_V, med_u = med_u, sd_u = sd_u, p_u_lt_005 = p_u05,
                support_hi = support[2], convergence = convergence)))) %>%
    left_join(chk_in, by = "sic_3")
print(as.data.frame(summ %>% mutate(across(where(is.numeric), ~ round(.x, 4)))))
if (any(!ok)) cat("FAILED:", order_ind[!ok], "\n")
save(res, summ, file = "Code/Products/1603-np-deconv-stage2.RData")
write.csv(summ, "Code/Products/1603-np-deconv-stage2-summary.csv", row.names = FALSE)
cat("Saved: Code/Products/1603-np-deconv-stage2.RData\n")
