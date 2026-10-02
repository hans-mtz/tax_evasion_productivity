## PRODUCT: Code/Products/1618-stage2-input-{designA-interior-plant-k,designA-interior-plant-k-umed}-pshare{05,10}-trim0.005.csv
##   := the design i (1598) and design iib (1604) stage-2 inputs plus column `pshare` = the deconvolved share of
##   overreporters of the firm's industry, P(u >= c), from the stage-2 deconvolution 1603 (c = 0.05 and 0.10), for the
##   IND5P share rows (grid_estimator_ind5p, share_u = c). Hans, 2026-10-02.
suppressPackageStartupMessages(library(dplyr))
fenv <- new.env(); load("Code/Products/np-deconv-funs.RData", envir = fenv)
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv
load("Code/Products/1603-np-deconv-stage2-macbook.RData")   # res
ps <- bind_rows(lapply(names(res), function(s) {
    p <- res[[s]]$fit$params; g <- seq(p$a, p$b, length.out = 20001)
    f <- pmax(fenv$f_e.np(g, res[[s]]$fit$theta, p), 0); f <- f / sum(f)
    tibble(sic_3 = s, p05 = sum(f[g >= 0.05]), p10 = sum(f[g >= 0.10]))
}))
print(as.data.frame(ps %>% mutate(across(where(is.numeric), ~ round(.x, 4)))))
for (src in c("1598-stage2-input-designA-interior-plant-k-trim0.005.csv", "1604-stage2-input-designA-interior-plant-k-umed-trim0.005.csv")) {
    d <- read.csv(file.path("Code/Products", src), colClasses = c(sic_3 = "character"))
    stopifnot(all(d$sic_3 %in% ps$sic_3))
    for (c in c("05", "10")) {
        out <- d %>% left_join(ps %>% select(sic_3, pshare = !!paste0("p", c)), by = "sic_3")
        stopifnot(!anyNA(out$pshare))
        f <- file.path("Code/Products", sub("-trim0.005.csv", paste0("-pshare", c, "-trim0.005.csv"), sub("^1598|^1604", "1618", src)))
        write.csv(out, f, row.names = FALSE, quote = FALSE); cat("Saved:", f, "\n")
    }
}
