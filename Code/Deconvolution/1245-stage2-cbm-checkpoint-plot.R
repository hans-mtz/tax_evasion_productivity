## FHJ2008-style batch-means checkpoint plot: MCSE and batch size vs n,
## for the 3 diagnostic points, growing-by-10% schedule (2026-09-08).
## p(n) and epsilon are NOT applied here -- this shows the raw ingredients
## (t_{a-1,.975}, MCSE, half-width) only; the actual stopping decision needs
## those two chosen deliberately, not baked into this figure.

library(tidyverse)

points <- list(
    list(label = "(2,0.2)",  file = "Code/Products/1243-psitraj-d1_2-d2_0.2.csv"),
    list(label = "(6,0.3)",  file = "Code/Products/1243-psitraj-d1_6-d2_0.3.csv"),
    list(label = "(1,0.4)",  file = "Code/Products/1243-psitraj-d1_1-d2_0.4.csv")
)
N_INTERIOR <- 28892

checkpoints <- c(1000)
while (tail(checkpoints, 1) < 5000) checkpoints <- c(checkpoints, min(5000, round(tail(checkpoints, 1) * 1.1)))
checkpoints <- unique(checkpoints)

cbm_stats <- function(Z_sum, n_firms, n) {
    Zr <- Z_sum[1:n] / n_firms
    b <- floor(sqrt(n)); a <- floor(n / b)
    used <- a * b
    Zr <- Zr[1:used]
    batch_id <- rep(1:a, each = b)
    batch_means <- tapply(Zr, batch_id, mean)
    gbar <- mean(Zr)
    sigma2 <- b / (a - 1) * sum((batch_means - gbar)^2)
    mcse <- sqrt(sigma2 / used)
    tcrit <- qt(0.975, df = a - 1)
    halfwidth <- tcrit * mcse
    data.frame(n = n, a = a, b = b, tcrit = tcrit, mean = gbar, sigma2 = sigma2, mcse = mcse,
               halfwidth = halfwidth, rel_halfwidth_pct = 100 * abs(halfwidth / gbar))
}

all_res <- bind_rows(lapply(points, function(p) {
    df <- read.csv(p$file)
    res <- do.call(rbind, lapply(checkpoints, function(n) cbm_stats(df$Z_pooled_sum, N_INTERIOR, n)))
    res$point <- p$label
    res
}))

write.csv(all_res, "Code/Products/1245-stage2-cbm-checkpoint-table.csv", row.names = FALSE)
cat("Saved: Code/Products/1245-stage2-cbm-checkpoint-table.csv\n")

wong_cb_palette <- c("#E69F00", "#56B4E9", "#009E73")

p1 <- ggplot(all_res, aes(x = n, y = mcse, color = point)) +
    geom_line(linewidth = 0.7) + geom_point(size = 2) +
    scale_color_manual(values = wong_cb_palette) +
    labs(x = "n (kept draws, growing 10%/checkpoint)", y = "MCSE (batch means)", color = "(δ1,δ2) point",
         title = "Batch-means MCSE of pooled ψ vs. chain length",
         subtitle = "a=b=⌊√n⌋ (FHJ2008's own 'convenient' default). No ε/p(n) applied -- raw MCSE only.") +
    theme_minimal() +
    theme(plot.title = element_text(family = "Times", size = 13, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(family = "Times", size = 9, hjust = 0.5), legend.position = "bottom")
ggsave("Paper/images/1245-stage2-cbm-mcse-vs-n.png", p1, width = 9, height = 6, dpi = 300)

p2 <- ggplot(all_res, aes(x = n, y = b)) +
    geom_line(color = "grey40", linewidth = 0.7) + geom_point(color = "grey20", size = 2) +
    labs(x = "n (kept draws)", y = "batch size b = ⌊√n⌋",
         title = "CBM batch size at each checkpoint (same for all 3 points, depends only on n)") +
    theme_minimal() +
    theme(plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5))
ggsave("Paper/images/1245-stage2-cbm-batchsize-vs-n.png", p2, width = 8, height = 5, dpi = 300)

cat("Saved: Paper/images/1245-stage2-cbm-mcse-vs-n.png, 1245-stage2-cbm-batchsize-vs-n.png\n")
print(all_res %>% select(point, n, a, b, tcrit, mean, sigma2, mcse, halfwidth, rel_halfwidth_pct), digits = 5)
