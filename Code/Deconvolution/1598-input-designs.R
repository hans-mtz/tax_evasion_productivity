## PRODUCT: Code/Products/1598-stage2-input-designA-interior-plant-k-trim0.005.csv := the 1592 interior input plus
##   k        log capital (stage2_final$k), via the same verified row_id map as 1592;
##   audit_g  1 if the firm-period is in the top 10% of capital within its industry (interior firms, all years
##            pooled), else 0 -- the group G for the audit-probability moment (design ii, 2026-09-30);
##   audit_gv same with V (the observed overreporting proxy) instead of capital -- robustness check (Hans, 2026-10-01);
##   umed     the deconvolved median of u for the firm's industry (1521; 7 industries), -1 where none (351, 352)
##            -- targets for the median robustness rows (design i).
suppressPackageStartupMessages(library(dplyr))
load("Code/Products/1532-stage2-data-final.RData")
trim_top_pct <- 0.005
b <- stage2_final %>% filter(!corp) %>%
    mutate(use_c = evader,
           cal_V = if_else(use_c, cal_V_c, cal_V_u), tilde_cal_W = if_else(use_c, tilde_cal_W_c, tilde_cal_W_u),
           beta = if_else(use_c, beta_c, beta_u),
           corner = as.integer(sales_tax_rate_purchases == 0 | !use_c)) %>%
    filter(is.finite(cal_V), is.finite(tilde_cal_W), is.finite(beta), is.finite(sales_tax_rate_purchases),
           sales_tax_rate_purchases >= 0, is.finite(M_star), M_star > 0, is.finite(t1), is.finite(pgdp))
cut <- quantile(b$M_star[b$corner == 0], 1 - trim_top_pct)
b <- b %>% filter(corner == 1 | M_star <= cut) %>% mutate(row_id = row_number())
map <- b %>% transmute(row_id, k_chk = k, M_star_chk = M_star, cal_V_chk = cal_V)
inp <- read.csv("Code/Products/1592-stage2-input-designA-interior-plant-trim0.005.csv", colClasses = c(sic_3 = "character"))
out <- inp %>% left_join(map, by = "row_id")
stopifnot(isTRUE(all.equal(out$M_star, out$M_star_chk, tolerance = 1e-12)), isTRUE(all.equal(out$cal_V, out$cal_V_chk, tolerance = 1e-12)),
          all(is.finite(out$k_chk)))
out <- out %>% mutate(k = k_chk) %>% select(-ends_with("_chk")) %>%
    group_by(sic_3) %>% mutate(audit_g = as.integer(k >= quantile(k, 0.9)),             # headline group: capital
                               audit_gv = as.integer(cal_V >= quantile(cal_V, 0.9))) %>% ungroup()   # robustness: V
## deconvolved medians from 1521
fenv <- new.env(); load("Code/Products/np-deconv-funs.RData", envir = fenv)
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv
load("Code/Products/1521-np-deconv-selected.RData")
med <- sapply(names(deconv_sel_list), function(nm) { x <- deconv_sel_list[[nm]]; p <- x$params
    g <- seq(p$a, p$b, length.out = 20001); f <- pmax(fenv$f_e.np(g, x$theta, p), 0); cdf <- cumsum(f); cdf <- cdf / max(cdf); g[which(cdf >= 0.5)[1]] })
medtab <- data.frame(sic_3 = sub(" .*", "", names(med)), umed = unname(med))
print(medtab)
out <- out %>% left_join(medtab, by = "sic_3") %>% mutate(umed = ifelse(is.na(umed), -1, umed))
cat(sprintf("rows %d | audit_g share %.3f | audit_gv share %.3f | overlap %d | firms with a median target %d\n", nrow(out), mean(out$audit_g),
            mean(out$audit_gv), sum(out$audit_g & out$audit_gv), sum(out$umed >= 0)))
print(out %>% group_by(sic_3) %>% summarise(n = n(), audit_g = sum(audit_g), umed = first(umed)) %>% as.data.frame())
f <- "Code/Products/1598-stage2-input-designA-interior-plant-k-trim0.005.csv"
write.csv(out, f, row.names = FALSE, quote = FALSE); cat("Saved:", f, "\n")
