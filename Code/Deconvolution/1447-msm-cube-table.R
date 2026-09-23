## Table: naive-MSM J at the 7-point cubes around (a) the MSM optimizer solution and (b) the ELVIS point (lag_m, delta0 profiled).
library(tidyverse); library(tinytable); source("Code/Deconvolution/050-render-tbls.R")
lab <- c(center = "Center", lambda_up = "$\\lambda\\times2$", lambda_dn = "$\\lambda\\div2$", d1_up = "$\\delta_1+1$", d1_dn = "$\\delta_1-1$", d2_up = "$\\delta_2+0.2$", d2_dn = "$\\delta_2-0.2$")
rd <- function(f, g) read.csv(f) |> mutate(grid = g)
d <- bind_rows(rd("Code/Products/1445-msm-cube-lag_m.csv", "Around the MSM optimizer solution"),
               rd("Code/Products/1445-msm-cube-elvis-lag_m.csv", "Around the ELVIS point"))
sci <- function(x) sprintf("$%.2f\\times10^{%d}$", x / 10^floor(log10(x)), floor(log10(x)))
tbl <- d |> transmute(grid, Point = lab[point], lam = sci(lambda), d0 = sprintf("%.2f", delta0), d1 = sprintf("%.2f", delta1),
                      d2 = sprintf("%.2f", delta2), J = format(round(J), big.mark = ",", trim = TRUE),
                      dec = ifelse(reject, "Reject", "\\textbf{Pass}"))
print(tbl)
idx <- list("Around the MSM optimizer solution" = 1, "Around the ELVIS point" = 8)   # one insertion row per group
tt_obj <- tt(select(tbl, -grid), align = "lcccccc",
             notes = "Real data (lag\\_m, trim 0.5\\%). Naive MSM (Tim Conley's variant): $\\varepsilon$ drawn from the corporate pool, $S=250$, same draws at all points; $\\delta_0$ profiled at each point. $J=n\\hat L_n$ compared with $\\chi^2_{5,.95}=11.07$.")
colnames(tt_obj) <- c("Point", "$\\lambda$", "$\\delta_0$", "$\\delta_1$", "$\\delta_2$", "$J$", "Test (95\\%)")
tt_obj <- tt_obj |> group_tt(i = idx) |> style_tt(i = "notes", fontsize = 0.7)
render_png_tt_tbl(tt_obj, "1447-msm-cube-table"); cat("Saved: Paper/tbls/1447-msm-cube-table.png\n")
