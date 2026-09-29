## PRODUCT: Code/Products/1546-stage2-input-designA-tau-trim0.005.csv := the design-A stage-2 input (1532) plus the
## instrument column ltau_bar for the new moment row psi * ltau_bar (TAU_ROW build of grid_estimator; Research-log
## 2026-09-28): the cost shock psi is independent of the benefit shifter tau.
##   headline (Hans, 2026-09-28): ltau_bar = ln(tau_P), the firm's OWN purchases tax rate -- valid under psi _|_ tau, and
##     exactly the shifter inside h = ln tau_P + ln B(x). No leave-one-out needed. (The only concern would be fake
##     invoices carrying a different rate than real purchases, which would make tau_P move with e.)
##   robustness file: ltau_bar = ln of the industry-year mean purchases tax rate (all firms incl. corporations).
library(tidyverse)
inp <- read.csv("Code/Products/1532-stage2-input-designA-trim0.005.csv", colClasses = c(sic_3 = "character"))
load("Code/Products/1532-stage2-data-final.RData")   # stage2_final, for the industry-year means
jt <- stage2_final %>% filter(is.finite(sales_tax_rate_purchases)) %>% group_by(sic_3, year) %>%
    summarise(tau_jt = mean(sales_tax_rate_purchases), .groups = "drop")

own <- inp %>% mutate(ltau_bar = ifelse(corner == 0, log(sales_tax_rate_purchases), 0))
ind <- inp %>% left_join(jt, by = c("sic_3", "year")) %>% mutate(ltau_bar = ifelse(corner == 0, log(tau_jt), 0)) %>% select(-tau_jt)
for (x in list(own, ind)) stopifnot(nrow(x) == nrow(inp), all(is.finite(x$ltau_bar[x$corner == 0])))
i <- own$corner == 0
cat(sprintf("interior %d | var ln tau_P (own) %.3f | var ln industry-year mean %.3f | cor %.3f\n",
            sum(i), var(own$ltau_bar[i]), var(ind$ltau_bar[i]), cor(own$ltau_bar[i], ind$ltau_bar[i])))
write.csv(own, "Code/Products/1546-stage2-input-designA-tau-trim0.005.csv", row.names = FALSE, quote = FALSE)
write.csv(ind, "Code/Products/1546-stage2-input-designA-taujt-trim0.005.csv", row.names = FALSE, quote = FALSE)
cat("Saved: Code/Products/1546-stage2-input-designA-{tau,taujt}-trim0.005.csv\n")
