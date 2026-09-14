## Stage-2 ELVIS, n_keep=3000, independent same-point seeding (2026-09-09):
## (1) 2D plot, Lhat vs lambda (the 16-point lambda grid); (2) 3D plot,
## Lhat vs (delta1,delta2) (the 49-point delta grid). Same style as
## Paper/images/1212-stage2-A-lhat-lambda.png and 1241-stage2-deltagrid-
## lag_m-3d.png.

library(tidyverse)

wong_blue <- "#0072B2"; wong_red <- "#D55E00"

## %% 2D: Lhat vs lambda ------------------------------------------------
lam_df <- read.csv("Code/Products/1251-lambdagrid-lag_m-nkeep3000-combined.csv")

p1 <- ggplot(lam_df, aes(x = lambda, y = Lhat)) +
    geom_line(color = "grey50", linewidth = 0.6) +
    geom_point(aes(color = pass, shape = pass), size = 3) +
    scale_color_manual(values = c("TRUE" = wong_red, "FALSE" = wong_blue),
                        labels = c("TRUE" = "Fail to reject (95% CI)", "FALSE" = "Rejected")) +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 4),
                        labels = c("TRUE" = "Fail to reject (95% CI)", "FALSE" = "Rejected")) +
    scale_x_log10(labels = scales::label_scientific()) +
    labs(x = expression(lambda~"(log scale)"), y = expression(hat(L)[n]~"(CUE objective)"),
         color = NULL, shape = NULL,
         title = "Stage-2 ELVIS: L_n(lambda), lag_m, moment set A (9 rows)",
         subtitle = "n_burn=1000, n_keep=3000, independent same-point seeding, delta1/delta2 free. trim=0.5%.") +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linetype = "solid"),
        plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Times", size = 9, hjust = 0.5),
        legend.position = "bottom"
    )
ggsave("Paper/images/1252-stage2-lag_m-lhat-lambda-nkeep3000.png", p1, width = 9, height = 6.5, dpi = 300)
cat("Saved: Paper/images/1252-stage2-lag_m-lhat-lambda-nkeep3000.png\n")

## %% 3D: Lhat vs (delta1,delta2) ----------------------------------------
delta_df <- read.csv("Code/Products/1250-deltagrid-lag_m-nkeep3000-combined.csv")

d1s <- sort(unique(delta_df$delta1)); d2s <- sort(unique(delta_df$delta2))
Z <- matrix(NA, length(d1s), length(d2s))
for (i in seq_along(d1s)) for (j in seq_along(d2s)) {
    v <- delta_df$Lhat[delta_df$delta1 == d1s[i] & delta_df$delta2 == d2s[j]]
    Z[i, j] <- if (length(v) == 1) v else NA
}
qc <- qchisq(0.95, 9); n <- 32232
thr <- min(delta_df$Lhat) + qc / (2 * n)

png("Paper/images/1252-stage2-deltagrid-lag_m-3d-nkeep3000.png", width = 2400, height = 1800, res = 220)
par(family = "Times")
res <- persp(d1s, d2s, Z, theta = -35, phi = 22, expand = 0.6, col = "lightgrey",
             border = "grey30", ticktype = "detailed", shade = 0.4,
             xlab = "delta1", ylab = "delta2", zlab = "L_n_hat",
             main = "Stage-2 ELVIS: L_n(delta1,delta2), lag_m, moment set A (9 rows), n_keep=3000",
             sub = sprintf("Threshold: Lhat_min+chi2_9,.95/(2n)=%.5g. Red=inside 95%% CI, blue=outside. All 49 pts converged.", thr))
pts <- trans3d(delta_df$delta1, delta_df$delta2, delta_df$Lhat, res)
points(pts, pch = 19, cex = 1.1, col = ifelse(delta_df$Lhat <= thr, wong_red, wong_blue))
legend("topright", legend = c("Inside 95% CI (fail to reject)", "Outside (rejected)"),
       col = c(wong_red, wong_blue), pch = 19, bty = "n", cex = 0.9)
dev.off()
cat("Saved: Paper/images/1252-stage2-deltagrid-lag_m-3d-nkeep3000.png\n")
