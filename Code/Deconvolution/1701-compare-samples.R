## PRODUCT: Code/Products/S1009/1701-compare-*.csv := current estimates (Code/Products) next to the same estimates on the
##   one-sample rule (Code/Products/S1009, run-1700-newsample.sh), for Hans to review before any text changes.
##   test (1510, net share; 1514 bootstrap), first stage and PF (1501, 1517, 1522, 1520), productivity (1523, 1524),
##   deconvolution (1521 test sample; 1603 stage-2 sample), 1983 reform (1530, ch. 7 elasticity inversion), stage-2 input.
## Usage: Rscript Code/Deconvolution/1701-compare-samples.R [old=Code/Products] [new=Code/Products/S1009]
source("Code/Deconvolution/utils-cli.R")
opt <- parse_cli_args(list(old = "Code/Products", new = "Code/Products/S1009")); log_run_header("1701-compare-samples.R", opt)
suppressPackageStartupMessages(library(tidyverse))
options(width = 220, pillar.width = 220)
O <- opt$old; N <- opt$new
ld <- function(dir, f) { e <- new.env(); load(file.path(dir, f), envir = e); e }
both <- function(f, fun) bind_rows(fun(O) %>% mutate(sample = "old"), fun(N) %>% mutate(sample = "new"))
side <- function(d, by, vals) d %>% select(all_of(c(by, "sample", vals))) %>%
    pivot_wider(names_from = sample, values_from = all_of(vals), names_glue = "{.value}_{sample}")
show <- function(title, d, file) { cat("\n==", title, "==\n"); print(as.data.frame(d %>% mutate(across(where(is.numeric), \(x) round(x, 4)))))
    write.csv(d, file.path(N, paste0("1701-compare-", file, ".csv")), row.names = FALSE) }
fmt <- \(lo, hi) ifelse(is.na(lo), "empty", sprintf("[%.3f, %.3f]", lo, hi))

## 1. Test (net share) ------------------------------------------------------------------------------------------------
t <- both("", \(d) ld(d, "1510-test-inversion.RData")$out %>% filter(share == "s_net") %>%
              transmute(sic_3, n_corp, n_uninc, mu_hat, TS0, sharp = fmt(sh_lo, sh_hi), reject = !sh_open & !is.na(sh_lo)))
show("Test for overreporting (1510, net share): mu_hat, sharp 95% region, rejects at 5%",
     side(t, "sic_3", c("n_corp", "n_uninc", "mu_hat", "sharp", "reject")), "test")
b <- both("", \(d) read.csv(file.path(d, "1514-boot-test-fixed.csv")) %>% filter(share == "s_net") %>%
              transmute(sic_3 = as.character(sic_3), mu_hat, p_one))
show("Bootstrap test (1514, net share)", side(b, "sic_3", c("mu_hat", "p_one")), "boot")

## 2. First stage and PF ------------------------------------------------------------------------------------------------
fs <- both("", \(d) { e <- ld(d, "1501-fs-net.RData"); tibble(sic_3 = names(e$fs_net_ls),
              beta = sapply(e$fs_net_ls, \(z) z$beta), sd_eps = sapply(e$fs_net_ls, \(z) z$epsilon_sigma),
              n = sapply(e$fs_net_ls, \(z) if (is.null(z$data)) NA_integer_ else nrow(z$data))) })
pf <- both("", \(d) ld(d, "1517-pf-systems-all-industries.RData")$res %>% filter(system == "lag_2_w_eps") %>%
              transmute(sic_3, aK = alpha_K, aL = alpha_L, K_sh = fmt(K_sh_lo, K_sh_hi), L_sh = fmt(L_sh_lo, L_sh_hi)))
bc <- both("", \(d) ld(d, "1522-beta-testinv.RData")$beta_ci %>% transmute(sic_3, beta_sh = fmt(b_sh_lo, b_sh_hi)))
show("First stage and PF (1501, 1517 W~_{it-2}, 1522)",
     side(fs %>% left_join(pf, by = c("sic_3", "sample")) %>% left_join(bc, by = c("sic_3", "sample")), "sic_3",
          c("n", "beta", "beta_sh", "sd_eps", "aK", "aL", "K_sh", "L_sh")), "pf")
g <- both("", \(d) ld(d, "1520-gnr-ols.RData")$gnr_ols %>% transmute(sic_3, gnr_beta, gnr_aK, gnr_aL, ols_beta))
show("GNR and OLS (1520)", side(g, "sic_3", c("gnr_beta", "gnr_aK", "gnr_aL", "ols_beta")), "gnr")

## 3. Productivity --------------------------------------------------------------------------------------------------------
om <- both("", \(d) { s <- ld(d, "1523-omega-deconv-final.RData")$omega_fin_stats_df
              tibble(key = rownames(s), mean = s$mean, sd = s$sd) })
show("Productivity deconvolution (1523)", side(om, "key", c("mean", "sd")), "omega")
ps <- both("", \(d) read.csv(file.path(d, "1524-omega-persistence-final.csv")) %>% transmute(sic_3 = as.character(sic_3), method, gamma1, n))
show("Productivity persistence (1524)", side(ps, c("sic_3", "method"), c("gamma1", "n")), "persistence")

## 4. Deconvolution of overreporting --------------------------------------------------------------------------------------
dc <- both("", \(d) { s <- ld(d, "1521-np-deconv-selected.RData")$deconv_sel_stats
              tibble(sic_3 = sub(" .*", "", rownames(s)), E_u = s$mean, sd_u = s$sd) })
show("Deconvolution on the test sample (1521, selected industries)", side(dc, "sic_3", c("E_u", "sd_u")), "deconv-test")
d2 <- both("", \(d) read.csv(file.path(d, "1603-np-deconv-stage2-summary-macbook.csv")) %>%
              transmute(sic_3 = as.character(sic_3), n, mean_V, E_u, med_u, x_mean = NA_real_))
show("Deconvolution on the stage-2 sample (1603; ch. 5)", side(d2 %>% select(-x_mean), "sic_3", c("n", "mean_V", "E_u", "med_u")), "deconv-stage2")

## 5. 1983 reform -----------------------------------------------------------------------------------------------------------
dd <- both("", \(d) read.csv(file.path(d, "1530-fiscal-did-compare.csv")) %>% transmute(series, year, net))
show("1983 reform, event-study paths (1530, net share)", side(dd, c("series", "year"), "net"), "did")
el <- both("", \(d) read.csv(file.path(d, "ch07-overreporting-elasticity-inversion.csv")) %>%
              transmute(post = as.character(post), n, E_mid, ci = sprintf("[%.2f, %.2f]", ci_lo, ci_hi)))
show("Midpoint elasticity of overreporting (appendix H)", side(el, "post", c("n", "E_mid", "ci")), "elasticity")

## 6. Stage-2 input ----------------------------------------------------------------------------------------------------------
s2 <- both("", \(d) { f <- file.path(d, paste0("1598-stage2-input-designA-interior-plant-k-", if (d == N) "trim0" else "trim0.005", ".csv"))
              read.csv(f, colClasses = c(sic_3 = "character")) %>% group_by(sic_3) %>%
                  summarise(n = n(), mean_V = mean(cal_V), mean_Mbar = mean(Mbar), beta = first(beta), sig2eps = first(sig2eps), .groups = "drop") })
show("Stage-2 interior input (1598)", side(s2, "sic_3", c("n", "mean_V", "mean_Mbar", "beta", "sig2eps")), "stage2-input")
cat("\nSaved: ", file.path(N, "1701-compare-*.csv"), "\n")
