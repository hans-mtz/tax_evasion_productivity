## PRODUCT: Code/Products/ch05-overreporting-medians.csv := summaries across industries of the deconvolved mean
## overreporting ratio x = e/M (ch. 5 text, abstracts): unweighted median, firm-weighted median and firm-weighted mean,
## for (i) the five largest industries where overreporting is detected (313, 321, 322, 342, 369; ch. 5 rule) and
## (ii) four of the five largest industries by output (313, 321, 351, 352; 311 is exempt; ch. 4 ranking).
## Also writes Code/Products/ch05-overreporting-by-industry.csv (per-industry E[u], E[x], quantiles, firm counts).
## Reads:  Code/Products/1603-np-deconv-stage2-macbook.RData (penalized B-spline deconvolution of u = ln(M*/M) on the
##         stage-2 sample, f_eps from corporations, net log materials share; the fits behind ch05-overreporting-ratio.R)
##         Code/Products/1598-stage2-input-designA-interior-plant-k-trim0.005.csv (firm-years and plants by industry).
## Method: per industry, f_u on a 4001-point grid normalized to integrate to 1; E[x] = int (e^u - 1) f_u du (same as
##         ch05-overreporting-ratio.R). Weighted median = the smallest industry mean whose cumulative weight share
##         reaches 1/2 (industries sorted by their mean). Weights: firm-years (distinct plants reported alongside).
## Added 2026-10-04 (Hans) for reproducibility of the numbers in the research log of that date.
source("Code/Thesis/001-setup.R")
fenv <- new.env(); load(file.path(PRODUCTS_DIR, "np-deconv-funs.RData"), envir = fenv)   # load, never source() 030
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv
f_e.np <- fenv$f_e.np
load(file.path(PRODUCTS_DIR, "1603-np-deconv-stage2-macbook.RData"))   # res, summ

fits <- lapply(res, `[[`, "fit")
by_ind <- imap_dfr(fits, function(d, nm) {
    p <- d$params; u <- seq(p$a, p$b, length.out = 4001)
    fu <- vapply(u, function(z) f_e.np(z, d$theta, p), numeric(1)); du <- diff(u)[1]; fu <- fu / sum(fu * du)
    x <- exp(u) - 1; q <- function(pr) x[which.max(cumsum(fu * du) >= pr)]
    tibble(sic_3 = str_extract(nm, "\\d{3}"), Eu = sum(u * fu * du), Ex = sum(x * fu * du),
           p10 = q(0.10), p50 = q(0.50), p90 = q(0.90), p99 = q(0.99))
})
inp <- read.csv(file.path(PRODUCTS_DIR, "1598-stage2-input-designA-interior-plant-k-trim0.005.csv"),
                colClasses = c(sic_3 = "character"))
cnt <- inp %>% group_by(sic_3) %>% summarise(firm_years = n(), plants = n_distinct(plant_id), EV = mean(cal_V), .groups = "drop")
by_ind <- by_ind %>% left_join(cnt, by = "sic_3") %>% arrange(sic_3)
stopifnot(all(!is.na(by_ind$firm_years)))
print(by_ind)
write.csv(by_ind, file.path(PRODUCTS_DIR, "ch05-overreporting-by-industry.csv"), row.names = FALSE)

wmed <- function(v, w) { o <- order(v); v[o][which(cumsum(w[o]) / sum(w) >= 0.5)[1]] }
sets <- list("five largest evaders" = c("313", "321", "322", "342", "369"),
             "four of the five largest" = c("313", "321", "351", "352"))
out <- imap_dfr(sets, function(s, nm) {
    d <- by_ind %>% filter(sic_3 %in% s); stopifnot(nrow(d) == length(s))
    tibble(set = nm, industries = paste(s, collapse = " "), median = median(d$Ex),
           wmedian_firm_years = wmed(d$Ex, d$firm_years), wmedian_plants = wmed(d$Ex, d$plants),
           wmean_firm_years = weighted.mean(d$Ex, d$firm_years), mean = mean(d$Ex))
})
print(out)
write.csv(out, file.path(PRODUCTS_DIR, "ch05-overreporting-medians.csv"), row.names = FALSE)
cat("Saved: Code/Products/ch05-overreporting-{medians,by-industry}.csv\n")
