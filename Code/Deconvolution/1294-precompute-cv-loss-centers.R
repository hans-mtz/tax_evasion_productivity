## Precompute step for the CV-adjusted-R and Loss (Delta,target) grids.
## For each Delta in {-0.05,...,0.05} (step 0.01), forward-simulate (no
## optimization, moment-set-A-only seed gamma -- the same one used for every
## prior fixed-theta run) to get:
##  - R_center, R_se_naive (sizing SE for the CV-adjusted moment)
##  - Loss_center, Loss_se_naive (sizing SE for the Loss moment)
## Also (re-)estimates cv_beta/cv_mu_c ONCE, pooled across all 11 Deltas'
## forward-sim draws (more robust than the single-Delta-0 estimate used in
## the exploratory check), then re-derives R_se_naive as the CV-adjusted
## residual SD/sqrt(n) at each Delta using this pooled beta.
## Writes: Code/Products/1294-cv-loss-grid-design.csv (one row per Delta,
## all quantities needed to build both coarse grids) and prints cv_beta/
## cv_mu_c for the shell scripts to consume.
## 2026-09-12.

suppressMessages(library(tidyverse))

DELTAS <- seq(-0.05, 0.05, by = 0.01)
THETA <- "3.46356,5.427e-07,4.3,0.54"
GAMMA9 <- "0.0346259131274546,-0.0314423872203614,0.213345547568563,-0.269758367960626,0.0579244974907063,-1.13805034160568,1.45248121626455e-05,1.42024529746241e-08,4.37374834787322"
INPUT <- "Code/Products/1260-stage2-revenue-input-lag_m-trim0.005.csv"

load("Code/Products/1200-stage2-data.RData")
load("Code/Products/colombia_data.RData")
tax_panel <- colombia_data_frame %>% transmute(plant = as.character(plant), year = year, t1 = sales_tax_sales, pgdp = p_gdp)
base <- stage2_data %>% dplyr::filter(ins == "lag_m", !corp) %>% mutate(plant = as.character(plant))
interior <- base %>% dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0) %>% mutate(corner = 0L)
Mstar_cutoff <- quantile(interior$M_star, 1 - 0.005, na.rm = TRUE)
interior <- interior %>% dplyr::filter(M_star <= Mstar_cutoff)
corner_obs <- base %>% dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases == 0) %>% mutate(corner = 1L)
run_sample <- bind_rows(interior, corner_obs) %>% left_join(tax_panel, by = c("plant", "year")) %>%
    mutate(row_id = dplyr::row_number()) %>% select(row_id, t1, pgdp)

all_firms <- list()
summary_rows <- list()

for (d in DELTAS) {
    tag <- gsub("-", "m", gsub("\\.", "", sprintf("%.2f", d)))
    outcsv <- sprintf("/tmp/precompute_revbase_d%s.csv", tag)
    cmd <- sprintf(
        "cd 'Code/C-estimator' && ./grid_estimator mode=revenue_baseline input_csv=../Products/1260-stage2-revenue-input-lag_m-trim0.005.csv output_csv=%s par=%s,%s Delta=%.2f n_burn=1000 n_keep=3000 n_threads=12 base_seed=20260829",
        outcsv, THETA, GAMMA9, d
    )
    system(cmd, ignore.stdout = TRUE)
    rb <- read.csv(outcsv) %>% inner_join(run_sample, by = "row_id")
    rb$Delta <- d
    all_firms[[tag]] <- rb
    cat(sprintf("Delta=%.2f: mean(R_real)=%.3f mean(Loss_real)=%.4f done\n", d, mean(rb$R_real), mean(rb$Loss_real)))
}

pooled <- bind_rows(all_firms)
fit_cv <- lm(R_real ~ I(t1 / pgdp), data = pooled)
cv_beta <- unname(coef(fit_cv)[2])
cv_mu_c <- mean(pooled$t1 / pooled$pgdp)
cat(sprintf("\nPOOLED cv_beta=%.6f cv_mu_c=%.6f (R^2=%.5f)\n", cv_beta, cv_mu_c, summary(fit_cv)$r.squared))

design <- map_dfr(names(all_firms), function(tag) {
    rb <- all_firms[[tag]]
    n <- nrow(rb)
    resid_cv <- rb$R_real - cv_beta * (rb$t1 / rb$pgdp)
    tibble(
        Delta = rb$Delta[1], n = n,
        R_center = mean(rb$R_real),
        R_cv_center = mean(resid_cv) + cv_beta * cv_mu_c,   # should equal R_center exactly
        R_cv_se_naive = sd(resid_cv) / sqrt(n),
        Loss_center = mean(rb$Loss_real),
        Loss_se_naive = sd(rb$Loss_real) / sqrt(n),
        Loss_frac_zero = mean(rb$Loss_real == 0)
    )
})

write.csv(design, "Code/Products/1294-cv-loss-grid-design.csv", row.names = FALSE)
cat("\nSaved: Code/Products/1294-cv-loss-grid-design.csv\n")
print(design)

cat(sprintf("\nCV_BETA=%.6f\nCV_MU_C=%.6f\n", cv_beta, cv_mu_c))
