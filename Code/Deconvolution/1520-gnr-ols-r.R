## PRODUCT: Code/Products/1520-gnr-ols.{csv,RData} := uncorrected production-function estimates, GNR (2020) Cobb-Douglas
## (measurement-error version: big E = 1) and OLS, for every industry, on the final sample: two-tax (net-of-sales-tax) log
## materials share, juridical organization codes 6-9 excluded (1501's df_net rows). Companion columns of ch. 6's table.
##
## GNR is an R port of Code/Stata/GNR_code_CD_me.do + gmm_prod_CD.ado (run by 020-loop-me.do -> Code/Products/gnr-cd-me.csv):
##   (1) nl si = ln(g0) with only a constant  => ln g0 = mean(si); mexp_eg = 1 => beta = g0 = exp(mean si); eg = -(si - ln g0)
##   (2) vg = y - eg - beta * i,  i = ln(real materials)
##   (3) GMM, exactly identified, identity weight: omega = vg - aL l - aK k; xi = residual of OLS of omega on
##       (1, omega_{-1}, omega_{-1}^2, omega_{-1}^3); moments E[l xi] = E[k xi] = 0. Lags by calendar year (tset id time; L.).
## OLS as in 300-deconv-prod.R: y ~ ln(materials) + k + l.
## VALIDATION (mode "old"): gross share, all legal forms, 931.1's df sample, five industries -> compare with gnr-cd-me.csv.
library(tidyverse); library(parallel)
threshold_cut <- 0.05

gnr_cd <- function(d, share) {
    d <- d %>% filter(is.finite(.data[[share]]), is.finite(y), is.finite(k), is.finite(l), is.finite(i)) %>%
        mutate(si = .data[[share]])
    lng0 <- mean(d$si); beta <- exp(lng0)
    d <- d %>% mutate(eg = -(si - lng0), vg = y - eg - beta * i) %>% arrange(plant, year)
    d0 <- d
    d <- d %>%
        group_by(plant) %>%
        mutate(prev = year - 1,
               vg_1 = vg[match(prev, year)], l_1 = l[match(prev, year)], k_1 = k[match(prev, year)]) %>%
        ungroup() %>% filter(is.finite(vg_1), is.finite(l_1), is.finite(k_1))
    mom <- function(p) {
        w <- d$vg - p[1] * d$l - p[2] * d$k; w1 <- d$vg_1 - p[1] * d$l_1 - p[2] * d$k_1
        xi <- resid(lm.fit(cbind(1, w1, w1^2, w1^3), w))
        c(mean(d$l * xi), mean(d$k * xi))
    }
    st <- coef(lm(vg ~ l + k, d))[c("l", "k")]
    o <- optim(st, \(p) sum(mom(p)^2), control = list(reltol = 1e-14, maxit = 5000))
    o <- optim(o$par, \(p) sum(mom(p)^2), method = "BFGS", control = list(reltol = 1e-16, maxit = 2000))
    tibble(gnr_beta = beta, gnr_aL = o$par[[1]], gnr_aK = o$par[[2]], gnr_obj = o$value, gnr_n = nrow(d),
           err_sd = sd(d$eg),
           ## firm-level log productivity, as the Stata code exports it (logomega = vg - al*l - ak*k), all rows with vg
           omega = list(d0 %>% transmute(plant, year, logomega = vg - o$par[[1]] * l - o$par[[2]] * k)))
}
ols_cd <- function(d, share) {
    d <- d %>% filter(is.finite(.data[[share]]), is.finite(y), is.finite(k), is.finite(l), is.finite(i))
    b <- coef(lm(y ~ i + k + l, d))
    tibble(ols_beta = b[["i"]], ols_aK = b[["k"]], ols_aL = b[["l"]], ols_n = nrow(d))
}

args <- commandArgs(trailingOnly = TRUE); mode <- if (length(args)) args[1] else "final"
load("Code/Products/931.1-fs-se-het.RData")   # df (raw panel)
prep <- \(x) x %>% ungroup() %>% mutate(sic_3 = as.character(sic_3), plant = as.character(plant),
                                        year = as.numeric(as.character(year)), i = log(materials))
if (mode == "old") {
    ## approved setup: gross share, all legal forms, share > 5%, no upper cut (as the Stata loop: si_level > g_cut)
    base <- prep(df) %>% mutate(share = log(nom_mats / nom_gross_output)) %>% filter(is.finite(share), share > log(threshold_cut))
    inds <- c("331", "322", "369", "313", "321")
} else {
    load("Code/Products/1501-fs-net.RData")   # df_net: two-tax sample, codes 6-9 excluded, net share rules incl. 369 cut
    base <- prep(df_net) %>% mutate(share = log_mats_share_net)
    inds <- sort(unique(base$sic_3))
}
res <- bind_rows(mclapply(inds, function(s) {
    d <- base %>% filter(sic_3 == s)
    if (nrow(d) < 50) return(tibble(sic_3 = s))
    bind_cols(tibble(sic_3 = s), gnr_cd(d, "share"), ols_cd(d, "share"))
}, mc.cores = max(1, detectCores() - 2))) %>% arrange(sic_3)
options(width = 200); print(res %>% select(-any_of("omega")) %>% mutate(across(where(is.numeric), \(z) round(z, 4))), n = Inf)
if (mode == "old") {
    st <- read.csv("Code/Products/gnr-cd-me.csv", skip = 1) %>% transmute(sic_3 = as.character(sic_3), stata_m = m, stata_k = k, stata_l = l)
    print(res %>% select(sic_3, gnr_beta, gnr_aK, gnr_aL) %>% left_join(st, by = "sic_3") %>%
          mutate(across(where(is.numeric), \(z) round(z, 4))))
} else {
    gnr_omega <- res %>% filter(!sapply(omega, is.null)) %>% select(sic_3, omega) %>% tidyr::unnest(omega)
    gnr_ols <- res %>% select(-omega)
    write.csv(gnr_ols, "Code/Products/1520-gnr-ols.csv", row.names = FALSE)
    save(gnr_ols, gnr_omega, file = "Code/Products/1520-gnr-ols.RData")
    cat("Saved: Code/Products/1520-gnr-ols.{csv,RData}\n")
}
