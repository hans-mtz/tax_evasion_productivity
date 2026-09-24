## PRODUCT: Thesis/tables/ch05-overreporting-ratio.png  := Table 5.x, overreporting ratio x = e/M by industry
##          Thesis/figures/ch05-overreporting-ratio.png := Figure 5.x, deconvolved density of x by industry
## Reads:   Code/Products/np_deconv_unincorp.RData (unincorp_np_deconv_list: penalized B-spline (logspline)
##          deconvolution of u = ln(M*/M) = ln(1 + e/M) on UNINCORPORATED firms only, i.e. the same firms as the
##          preferred test in ch. 4; produced by 292-np-deconv-unincorp.R). 291-bs-deconv.R's pooled fit
##          (corporations included, diluting f_u toward 0) is NOT used here.
## Method:  density transformation of the deconvolved f_u (Hogg et al. 2019, Thm 1.7.1; slides
##          700-deconvolving-evasion.qmd, "Getting density of the ratio"):
##              x = exp(u) - 1,   f_x(y) = f_u(ln(1+y)) / (1+y),   y >= 0.
##          No re-estimation: moments/quantiles of x are integrals against the already-fitted f_u.
##          Check: E[u] from the grid reproduces unincorp_np_stats_df$mean (printed below).
source("Code/Thesis/001-setup.R")
## f_e.np() and helpers: LOAD the saved functions, never source() 030-np-deconv-funs.R here --
## that script ends with save(list=ls()) and would overwrite Code/Products/np-deconv-funs.RData.
fenv <- new.env(); load(file.path(PRODUCTS_DIR, "np-deconv-funs.RData"), envir = fenv)
for (fn in ls(fenv)) if (is.function(fenv[[fn]])) environment(fenv[[fn]]) <- fenv   # helpers (s, C_recursive, ...) resolve in fenv
f_e.np <- fenv$f_e.np

load(file.path(PRODUCTS_DIR, "np_deconv_unincorp.RData"))   # unincorp_np_deconv_list, unincorp_np_stats_df

five <- c("331", "322", "369", "313", "321")
ind_names <- c("331" = "Wood products", "322" = "Wearing apparel", "369" = "Non-metallic minerals",
               "313" = "Beverages", "321" = "Textiles")

## f_u on a fine grid of u, normalized to integrate to 1 on the grid
grid_fu <- function(d, n = 4001) {
    p <- d$params
    u <- seq(p$a, p$b, length.out = n)
    fu <- vapply(u, function(z) f_e.np(z, d$theta, p), numeric(1))
    du <- diff(u)[1]
    tibble(u = u, fu = fu / sum(fu * du), du = du)
}

dens <- imap_dfr(unincorp_np_deconv_list, function(d, nm) {
    grid_fu(d) %>% mutate(sic_3 = str_extract(nm, "\\d{3}"))
}) %>%
    mutate(x = exp(u) - 1,
           fx = fu / (1 + x))          # f_x(y) = f_u(ln(1+y)) / (1+y)

qx <- function(x, fu, du, pr) x[which.max(cumsum(fu * du) >= pr)]

stats <- dens %>% group_by(sic_3) %>%
    summarise(
        Eu   = sum(u * fu * du),
        Ex   = sum(x * fu * du),
        SDx  = sqrt(sum((x - Ex)^2 * fu * du)),
        p10  = qx(x, fu, du, 0.10),
        p50  = qx(x, fu, du, 0.50),
        p90  = qx(x, fu, du, 0.90),
        .groups = "drop"
    ) %>%
    mutate(sic_3 = factor(sic_3, five)) %>% arrange(sic_3)
print(stats)
print(unincorp_np_stats_df)   # E[u] check

pct <- \(v) sprintf("%.1f\\%%", 100 * v)
tbl <- stats %>% transmute(
    Industry = paste0(as.character(sic_3), " ", ind_names[as.character(sic_3)]),
    Mean = pct(Ex), SD = pct(SDx), P10 = pct(p10), Median = pct(p50), P90 = pct(p90)
)

tt_obj <- tt(tbl, align = "lccccc", width = c(3, 1, 1, 1, 1, 1),
             notes = "Overreporting ratio $x=e/M$: overreported materials as a share of true materials. Moments and quantiles of the density $f_x(y)=f_u(\\ln(1+y))/(1+y)$, obtained by transforming the deconvolved density of $u=\\ln(1+e/M)$ (penalized B-spline deconvolution, unincorporated firms).") %>%
    style_tt(i = "notes", fontsize = 0.65)
render_thesis_table(tt_obj, "ch05-overreporting-ratio")

## Figure: f_x by industry. x-axis cut at 60%: all five densities are ~0 beyond it, except
## Beverages (313), whose long, very thin right tail (P99 far out, skewness 7.3 in u) would
## otherwise stretch the axis and flatten the other four curves.
x_max <- 0.6

plt <- dens %>% filter(x <= x_max) %>%
    mutate(Industry = factor(paste0(sic_3, " ", ind_names[sic_3]),
                             paste0(five, " ", ind_names[five]))) %>%
    ggplot(aes(x = x, y = fx, colour = Industry)) +
    geom_line(linewidth = 0.7) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "Overreporting ratio, e/M (share of true materials)", y = "Density", colour = NULL) +
    theme_thesis() +
    guides(colour = guide_legend(nrow = 2))
save_thesis_plot(plt, "ch05-overreporting-ratio")
