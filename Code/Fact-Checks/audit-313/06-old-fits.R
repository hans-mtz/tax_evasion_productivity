suppressMessages(library(tidyverse))
setwd("/Users/hans/Github/Tax_Evasion_Productivity")
fenv <- new.env(); load("Code/Products/np-deconv-funs.RData", envir = fenv)
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv
summ_fit <- function(d) { p <- d$params; u <- seq(p$a, p$b, length.out = 4001)
  fu <- vapply(u, function(z) fenv$f_e.np(z, d$theta, p), numeric(1)); du <- diff(u)[1]; fu <- fu / sum(fu * du)
  Eu <- sum(u * fu * du); c(a = p$a, b = p$b, Eu = Eu, sdu = sqrt(sum((u-Eu)^2*fu*du)), Ex = sum((exp(u) - 1) * fu * du), exp_Eu_m1 = exp(Eu) - 1) }
e1 <- new.env(); load("Code/Products/np_deconv_unincorp.RData", envir = e1)
e2 <- new.env(); load("Code/Products/bs_mle_data.RData", envir = e2)
e3 <- new.env(); load("Code/Products/1603-np-deconv-stage2-macbook.RData", envir = e3)
e4 <- new.env(); load("Code/Products/1521-np-deconv-selected.RData", envir = e4); cat("1521 objects:", ls(e4), "\n")
out <- rbind(
  old_pooled_np_291 = summ_fit(e2$full_np_deconv_list[["313 log_mats_share"]]),
  old_uninc_292     = summ_fit(e1$unincorp_np_deconv_list[["313 log_mats_share"]]),
  current_1603      = summ_fit(e3$res[["313"]]$fit))
print(round(out, 4))
cat("old fs_list 313 data rows:", nrow(fenv$fs_list[["313 log_mats_share"]]$data), "\n")
