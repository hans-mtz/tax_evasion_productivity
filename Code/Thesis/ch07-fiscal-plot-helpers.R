## Shared helper for the ch. 7 (1983 reform) figures, ch07-fiscal-{all-unincorp,liable-vs-exempt,llc,prt}-plot.R.
## Built 2026-09-24 to replace the copied slide PNGs (921-2-*.png from 921.2-het-slides.R: 720x480, no
## resolution tag). Same models as the ch. 7 tables (ch07-fiscal-table-helpers.R), so figures and tables
## cannot disagree. Two panels: levels mu_t and differences Delta mu_t relative to 1983 (1983 = 0, the
## reference). Points with 95% CIs from the models' own two-way (plant, year) clustered SEs.
source("Code/Thesis/ch07-fiscal-table-helpers.R")   # 001-setup, fixest, YEARS

## One group's year path from a model: tibble(year, b, se); missing years (1983 in the diff models) -> b = 0, se = NA
path_df <- function(model, prefix) {
    ct <- coeftable(model)
    tibble(year = YEARS) %>% mutate(
        b  = vapply(year, \(y) { nm <- paste0(prefix, y); if (nm %in% rownames(ct)) ct[nm, 1] else 0 }, numeric(1)),
        se = vapply(year, \(y) { nm <- paste0(prefix, y); if (nm %in% rownames(ct)) ct[nm, 2] else NA_real_ }, numeric(1)))
}

## series: named list, group label -> list(level = list(model, prefix), diff = list(model, prefix))
fiscal_plot <- function(series, slug, colours = NULL) {
    d <- imap_dfr(series, function(s, g) bind_rows(
        path_df(s$level$model, s$level$prefix) %>% mutate(panel = "Levels"),
        path_df(s$diff$model,  s$diff$prefix)  %>% mutate(panel = "Difference relative to 1983")
    ) %>% mutate(group = g)) %>%
        mutate(panel = factor(panel, c("Levels", "Difference relative to 1983")),
               group = factor(group, names(series)),
               lo = b - 1.96 * se, hi = b + 1.96 * se, year = 1900 + year)
    dodge <- position_dodge(width = if (length(series) > 1) 0.5 else 0)
    p <- ggplot(d, aes(x = year, y = b, colour = group)) +
        geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
        geom_vline(xintercept = 1983, linetype = "dashed", colour = "grey60") +
        geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, position = dodge, na.rm = TRUE) +
        geom_point(size = 1.8, position = dodge) +
        facet_wrap(~panel, nrow = 1) +
        scale_x_continuous(breaks = 1900 + YEARS, labels = YEARS) +
        labs(x = "Year", y = "Coefficient and 95% CI", colour = NULL) +
        theme_thesis() +
        theme(panel.grid.major.x = element_blank())
    if (!is.null(colours)) p <- p + scale_colour_manual(values = colours)
    if (length(series) == 1) p <- p + theme(legend.position = "none")
    save_thesis_plot(p, slug, width = 8, height = 4)
    cat("Saved: Thesis/figures/", slug, ".{png,pdf}\n", sep = "")
}
