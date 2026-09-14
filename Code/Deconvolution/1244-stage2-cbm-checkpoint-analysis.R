## FHJ2008-style batch-means checkpoint analysis, retroactive on the existing
## n_keep=5000 psi trajectories (2026-09-08). No new MCMC -- takes the SAME
## trajectory already computed for 3 points ((2,0.2),(6,0.3),(1,0.4), each at
## its own converged n_keep=5000 fit) and asks: had we grown the chain 10% at
## a time from 1000 to 5000, checking the fixed-width criterion (FHJ2008 eq.
## 7) at each step with CBM batches (a=b=floor(sqrt(n)), their own
## "convenient choice" default), where would the rule have told us to stop?

points <- list(
    list(label = "(2,0.2)",  file = "Code/Products/1243-psitraj-d1_2-d2_0.2.csv"),
    list(label = "(6,0.3)",  file = "Code/Products/1243-psitraj-d1_6-d2_0.3.csv"),
    list(label = "(1,0.4)",  file = "Code/Products/1243-psitraj-d1_1-d2_0.4.csv")
)
N_INTERIOR <- 28892

## Growing-by-10% checkpoint schedule, capped at 5000 (FHJ2008's own toy
## example grows the chain 10% at a time between checks).
checkpoints <- c(1000)
while (tail(checkpoints, 1) < 5000) checkpoints <- c(checkpoints, min(5000, round(tail(checkpoints, 1) * 1.1)))
checkpoints <- unique(checkpoints)

cbm_stats <- function(Z_sum, n_firms, n) {
    ## Z_sum[1:n] = cross-firm SUM of psi at each step; pooled scalar per step
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
    data.frame(n = n, a = a, b = b, mean = gbar, sigma2 = sigma2, mcse = mcse,
               halfwidth = halfwidth, rel_halfwidth_pct = 100 * abs(halfwidth / gbar))
}

for (p in points) {
    df <- read.csv(p$file)
    cat("====", p$label, "====\n")
    res <- do.call(rbind, lapply(checkpoints, function(n) cbm_stats(df$Z_pooled_sum, N_INTERIOR, n)))
    print(res, digits = 5, row.names = FALSE)
    cat("\n")
}
