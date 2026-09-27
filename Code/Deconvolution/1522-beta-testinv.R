## PRODUCT: Code/Products/1522-beta-testinv.{csv,RData} := test-inversion regions for the output elasticity of materials beta,
## every industry, on the PF sample (1501: two-tax share, codes 6-9 excluded). Decided 2026-09-26: grid beta, profile the
## alphas. For a single-instrument PF system the moments (1, Z, k, l)*eta are exactly identified in (aK, aL, g0, g1) at any
## beta, so once they are profiled they are zero at every candidate and the statistic reduces to the corporations' share
## moment alone:  g_i(beta) = s_i - ln(beta), corporations (s = net-of-tax log materials share; beta = exp(E[s | corp])
## under the measurement-error first stage, big E = 1).
## Plant-clustered, centred Omega (the convention of 1510/1512/1513); TS(beta) = n gbar^2 / Omega. Grid beta in [0.001, 1]
## step 0.001. Sharp: chi2_1 (credit for profiling aK, aL, g0, g1). Conservative: chi2_5 (no credit: 1 + 4 moments).
library(tidyverse)
load("Code/Products/1501-fs-net.RData")   # fs_net_ls

grid_b <- seq(0.001, 1, by = 0.001)
region <- \(ok) if (!any(ok)) c(NA, NA) else range(grid_b[ok])
one <- function(s) {
    fs <- fs_net_ls[[s]]; if (is.null(fs$data)) return(NULL)
    d <- fs$data %>% ungroup() %>% filter(!is.na(epsilon))            # corporations (epsilon only defined for them)
    lnD <- log(fs$beta); sh <- lnD - d$epsilon                        # epsilon = -(s - lnD)  =>  s = lnD - epsilon
    stopifnot(abs(mean(sh) - lnD) < 1e-8)                             # beta = exp(mean s), as in first_stage_panel_me
    cl <- as.character(d$plant); n <- length(sh)
    TS <- vapply(grid_b, function(b) {
        g <- sh - log(b); gb <- mean(g)
        G <- rowsum(g, cl); G <- G - as.vector(table(cl)[rownames(G)]) * gb
        n * gb^2 / (sum(G^2) / n)
    }, numeric(1))
    rs <- region(TS <= qchisq(.95, 1)); rc <- region(TS <= qchisq(.95, 5))
    tibble(sic_3 = s, beta = fs$beta, n_corp = n, corp_plants = length(unique(cl)),
           b_sh_lo = rs[1], b_sh_hi = rs[2], b_co_lo = rc[1], b_co_hi = rc[2])
}
beta_ci <- bind_rows(lapply(names(fs_net_ls), one)) %>% arrange(sic_3)
stopifnot(all(beta_ci$b_sh_lo > 0.001, beta_ci$b_co_hi < 1, na.rm = TRUE))   # regions interior to the grid
write.csv(beta_ci, "Code/Products/1522-beta-testinv.csv", row.names = FALSE)
save(beta_ci, file = "Code/Products/1522-beta-testinv.RData")
options(width = 200); print(beta_ci %>% mutate(across(where(is.numeric), \(z) round(z, 3))), n = Inf)
cat("Saved: Code/Products/1522-beta-testinv.{csv,RData}\n")
