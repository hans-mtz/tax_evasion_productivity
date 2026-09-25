## Firm size by juridical organization (@fig-size-by-jo): densities of log
## capital (k), log labour in employee-years (l) and log gross output (y) for
## proprietorships, LLCs and corporations. Partnerships are left out, as in the
## juridical-organization comparison of @sec-fiscal. Same sample filter as
## ch03-summary-stats-table.R. Also prints medians in levels, for the text.
## Status 2026-09-24: saved for reference; not yet referenced in any chapter.

source("Code/Thesis/001-setup.R")
load(file.path(PRODUCTS_DIR, "colombia_data.RData"))

jo_levels <- c("Proprietorship", "Ltd. Co.", "Corporation")

base <- colombia_data_frame %>%
    filter(
        is.finite(y),
        is.finite(k),
        is.finite(l),
        is.finite(m),
        JO_class %in% jo_levels
    ) %>%
    mutate(JO_class = factor(JO_class, levels = jo_levels))

size_long <- base %>%
    select(JO_class, k, l, y) %>%
    pivot_longer(c(k, l, y), names_to = "var", values_to = "val") %>%
    mutate(var = factor(
        var,
        levels = c("k", "l", "y"),
        labels = c("Capital, log k", "Labour, log l", "Gross output, log y")
    ))

p <- ggplot(size_long, aes(val, colour = JO_class, fill = JO_class)) +
    geom_density(alpha = 0.12, linewidth = 0.7) +
    facet_wrap(~var, scales = "free", ncol = 3) +
    labs(x = NULL, y = "Density", colour = NULL, fill = NULL) +
    theme_thesis() +
    theme(legend.position = "bottom")

ggsave(
    file.path(FIGURES_DIR, "ch03-size-by-jo.png"), p,
    width = THESIS_WIDTH, height = 3.2, dpi = THESIS_DPI
)

base %>%
    group_by(JO_class) %>%
    summarise(
        N      = n(),
        plants = n_distinct(plant),
        med_K  = median(exp(k)),
        med_L  = median(exp(l)),
        med_Y  = median(exp(y))
    ) %>%
    as.data.frame() %>%
    print(digits = 4)
