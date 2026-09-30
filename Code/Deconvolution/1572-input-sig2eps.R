## PRODUCT: Code/Products/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv := the design-A input with the tax column
## (1546) plus sig2eps = variance of the corporations' first-stage eps in the firm's industry (1532 stage2_final,
## eps_c for corp == TRUE). Target for the eps-variance row E[eps^2 - sig2eps_j] = 0 (EPSVAR build), ALL firms, like
## the other eps rows (Hans, 2026-09-29): for corner firms it tests the same restriction (their eps is from the
## unincorporated sample, the target from corporations), so it is not mechanical.
## Maintained assumption: eps (output measurement error) has the same distribution for corporations and unincorporated
## firms (the deconvolution's assumption).
library(tidyverse)
load("Code/Products/1532-stage2-data-final.RData")
v <- stage2_final %>% filter(corp, is.finite(eps_c)) %>% group_by(sic_3) %>% summarise(sig2eps = var(eps_c), n_corp = n(), .groups = "drop")
inp <- read.csv("Code/Products/1546-stage2-input-designA-tau-trim0.005.csv", colClasses = c(sic_3 = "character"))
out <- inp %>% left_join(v %>% select(sic_3, sig2eps), by = "sic_3") %>% left_join(v %>% select(sic_3, n_corp), by = "sic_3")
stopifnot(all(is.finite(out$sig2eps)))
print(as.data.frame(out %>% group_by(sic_3) %>% summarise(firms = n(), interior = sum(corner == 0), sig2eps = round(first(sig2eps), 4), n_corp = first(n_corp))))
out <- out %>% select(-n_corp)
write.csv(out, "Code/Products/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv", row.names = FALSE, quote = FALSE)
cat("Saved: Code/Products/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv\n")
