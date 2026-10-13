## PRODUCT: Code/Products/1525-pf-regions-extended.csv := the 95% test-inversion regions of 1517 with every open end closed:
## where a region projection reaches the edge of 1517's [0,1]^2 grid (0 or 1 not rejected), the grid is extended beyond it in
## strips of 0.25 (same 0.005 step, same statistic J_at from 1512) until the region stops at an interior point, cap [-2, 3].
## Purpose (Hans 2026-10-13): a table of estimates should show finite bounds even when they fall outside the parameter space
## (e.g. a negative alpha_K); the region containing 0 already says the coefficient is not different from zero. The test tables
## (ch. 4) keep the open-end convention. Default: the five industries of ch. 6, instrument W~_{it-2}, sharp test (chi2_2).
## Projections combine 1517's [0,1]^2 projection with the accepted points of the strips, so 1517's grid is not recomputed.
## Usage: Rscript Code/Deconvolution/1525-pf-regions-extended.R [inds=321,322,324,342,369] [system=lag_2_w_eps] [test=sharp]
library(tidyverse); library(parallel)
source("Code/Deconvolution/utils-cli.R")
args <- parse_cli_args(defaults = list(inds = "321,322,324,342,369", system = "lag_2_w_eps", test = "sharp"))
inds <- strsplit(args$inds, ",")[[1]]; system <- args$system; test <- args$test
stopifnot(system != "joint", test %in% c("sharp", "cons"))   # single-instrument statistic only (1512's J_at)

load("Code/Products/1501-fs-net.RData"); fs_all_ls <- fs_net_ls
grab <- function(file, from, to) { s <- readLines(file); s[grep(from, s):(grep(to, s) - 1)] }
eval(parse(text = grab("Code/Deconvolution/1512-pf-testinv-regions.R", "^prep <- function", "^one_ind <- function")))
load("Code/Products/1517-pf-systems-all-industries.RData")   # res: base regions on [0,1]^2
crit <- qchisq(.95, if (test == "sharp") 2 else 4)
sfx <- if (test == "sharp") "sh" else "co"
step <- 0.005; width <- 0.25; cap <- c(-2, 3)
sq <- \(a, b) if (a > b) numeric(0) else round(seq(a, b, by = step), 3)
ncore <- max(1, detectCores() - 2)

one <- function(s) {
    P <- prep(s); b <- res %>% filter(sic_3 == s, system == !!system)
    K <- c(b[[paste0("K_", sfx, "_lo")]], b[[paste0("K_", sfx, "_hi")]]); L <- c(b[[paste0("L_", sfx, "_lo")]], b[[paste0("L_", sfx, "_hi")]])
    if (anyNA(c(K, L))) return(tibble(sic_3 = s, K_lo = NA, K_hi = NA, L_lo = NA, L_hi = NA, Klim_lo = 0, Klim_hi = 1, Llim_lo = 0, Llim_hi = 1, n_new = 0))
    lim <- c(Klo = 0, Khi = 1, Llo = 0, Lhi = 1); n_new <- 0
    acc <- function(g) { J <- unlist(mclapply(seq_len(nrow(g)), \(i) J_at(P, system, g$aK[i], g$aL[i]), mc.cores = ncore))
                         n_new <<- n_new + nrow(g); g[!is.na(J) & J <= crit, , drop = FALSE] }
    upd <- function(a) if (nrow(a)) { K <<- range(c(K, a$aK)); L <<- range(c(L, a$aL)) }
    repeat {
        open <- c(Klo = K[1] <= lim[["Klo"]], Khi = K[2] >= lim[["Khi"]], Llo = L[1] <= lim[["Llo"]], Lhi = L[2] >= lim[["Lhi"]])
        open <- open & c(lim[["Klo"]] > cap[1], lim[["Khi"]] < cap[2], lim[["Llo"]] > cap[1], lim[["Lhi"]] < cap[2])
        if (!any(open)) break
        if (open["Klo"]) { new <- max(cap[1], lim[["Klo"]] - width); upd(acc(expand.grid(aK = sq(new, lim[["Klo"]] - step), aL = sq(lim[["Llo"]], lim[["Lhi"]])))); lim["Klo"] <- new }
        if (open["Khi"]) { new <- min(cap[2], lim[["Khi"]] + width); upd(acc(expand.grid(aK = sq(lim[["Khi"]] + step, new), aL = sq(lim[["Llo"]], lim[["Lhi"]])))); lim["Khi"] <- new }
        if (open["Llo"]) { new <- max(cap[1], lim[["Llo"]] - width); upd(acc(expand.grid(aK = sq(lim[["Klo"]], lim[["Khi"]]), aL = sq(new, lim[["Llo"]] - step)))); lim["Llo"] <- new }
        if (open["Lhi"]) { new <- min(cap[2], lim[["Lhi"]] + width); upd(acc(expand.grid(aK = sq(lim[["Klo"]], lim[["Khi"]]), aL = sq(lim[["Lhi"]] + step, new)))); lim["Lhi"] <- new }
    }
    still <- c(K[1] <= lim[["Klo"]], K[2] >= lim[["Khi"]], L[1] <= lim[["Llo"]], L[2] >= lim[["Lhi"]])
    if (any(still)) warning(s, ": region still open at the cap ", paste(names(lim)[still], collapse = ","))
    tibble(sic_3 = s, K_lo = K[1], K_hi = K[2], L_lo = L[1], L_hi = L[2],
           Klim_lo = lim[["Klo"]], Klim_hi = lim[["Khi"]], Llim_lo = lim[["Llo"]], Llim_hi = lim[["Lhi"]], n_new = n_new)
}
out <- bind_rows(lapply(inds, one)) %>% mutate(system = system, test = test, crit = crit, .after = sic_3)
print(out, width = Inf)
f <- "Code/Products/1525-pf-regions-extended.csv"
old <- if (file.exists(f)) read.csv(f, colClasses = c(sic_3 = "character")) %>% anti_join(out, by = c("sic_3", "system", "test")) else NULL
write.csv(bind_rows(old, out) %>% arrange(system, test, sic_3), f, row.names = FALSE)
cat("Saved:", f, "\n")
