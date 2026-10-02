## UPDATED 2026-10-01: reads 1603-np-deconv-stage2-macbook.RData -- the deconvolution on EXACTLY the stage-2 (ELVIS)
## sample, all 9 interior industries (313, 321, 322, 324, 331, 342, 351, 352, 369): unincorporated firms with tau_P > 0,
## net share in (5%, and for 369 75%), top 0.5% of M* trimmed; V and f_eps from the stage-2 first stage (1501).
## Table shows E[u] next to E[V] on the same sample (equal under the model); figure is small multiples (9 industries).
## (2026-09-26 version: 1521, the test's sample, 7 industries selected by the 99% test rule.)
## PRODUCT: Thesis/{tables,figures}/appE-overreporting-ratio-all.png := same for all nine interior industries (appendix)
## PRODUCT: Thesis/tables/ch05-overreporting-ratio.png  := Table 5.x (5 industries), overreporting ratio x = e/M by industry
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

load(file.path(PRODUCTS_DIR, "1603-np-deconv-stage2-macbook.RData"))   # res (per industry: fit, mean_V, ...), summ
unincorp_np_deconv_list <- setNames(lapply(res, `[[`, "fit"), paste(names(res), "log_mats_share_net"))
unincorp_np_stats_df <- summ
ev_tab <- summ %>% transmute(sic_3, EV = mean_V, n)

## 2026-10-01 rule (Hans): chapter text = the 5 largest industries (output share) where overreporting is detected at 1%
## (313 7.7%, 321 7.2%, 369 3.0%, 342 2.3%, 322 2.1%); the appendix E asset (appE-overreporting-ratio-all) carries all nine interior industries.
nine <- c("313", "321", "322", "324", "331", "342", "351", "352", "369")
five <- c("313", "321", "322", "342", "369")
ind_names <- c("313" = "Beverages", "321" = "Textiles", "322" = "Wearing apparel", "324" = "Footwear",
               "331" = "Wood products", "342" = "Printing and publishing", "351" = "Industrial chemicals",
               "352" = "Other chemicals", "369" = "Non-metallic minerals")

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
    left_join(ev_tab, by = "sic_3") %>%
    mutate(sic_3 = factor(sic_3, nine)) %>% arrange(sic_3)
print(stats)
print(unincorp_np_stats_df)   # E[u] check

pct <- \(v) sprintf("%.1f\\%%", 100 * v)
make_assets <- function(sel, slug, ncol, height) {
tbl <- stats %>% filter(sic_3 %in% sel) %>% transmute(
    Industry = paste0(as.character(sic_3), " ", ind_names[as.character(sic_3)]),
    Mean = pct(Ex), SD = pct(SDx), P10 = pct(p10), Median = pct(p50), P90 = pct(p90),
    `$E[u]$` = sprintf("%.3f", Eu), `$E[\\mathcal V]$` = sprintf("%.3f", EV)
)

tt_obj <- tt(tbl, align = "lccccccc", width = c(3.2, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8),
             notes = paste0("Overreporting ratio $x=e/M$: overreported materials as a share of true materials. Moments and quantiles of the density $f_x(y)=f_u(\\ln(1+y))/(1+y)$, obtained by transforming the deconvolved density of $u=\\ln(1+e/M)$ (penalized B-spline deconvolution, log materials share net of sales taxes). Sample: unincorporated firms that pay sales tax on purchases, the sample of the structural estimation in Chapter 8. $E[u]$: mean of the deconvolved $u$; under the model it equals $E[\\mathcal V]$ on the same sample."
    , if (identical(sel, five)) " Industries: the five largest, by share of output, where overreporting is detected at the 1\\% level (all nine in Appendix E)." else " Industries: the nine where overreporting is detected, whose unincorporated firms enter the structural estimation.")) %>%
    style_tt(i = "notes", fontsize = 0.8)
render_thesis_table(tt_obj, slug)

## Figure: f_x by industry. x-axis cut at 60%: all five densities are ~0 beyond it, except
## Beverages (313), whose long, very thin right tail (P99 far out, skewness 7.3 in u) would
## otherwise stretch the axis and flatten the other four curves.
x_max <- 0.6

plt <- dens %>% filter(x <= x_max, sic_3 %in% sel) %>%
    mutate(Industry = factor(paste0(sic_3, " ", ind_names[sic_3]),
                             paste0(sel, " ", ind_names[sel]))) %>%
    ggplot(aes(x = x, y = fx)) +
    geom_line(linewidth = 0.6, colour = THESIS_COLS[1]) +
    facet_wrap(~Industry, ncol = ncol, scales = "free_y", labeller = label_wrap_gen(width = 16)) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "Overreporting ratio, e/M (share of true materials)", y = "Density") +
    theme_thesis()
save_thesis_plot(plt, slug, height = height)
}
make_assets(five, "ch05-overreporting-ratio", ncol = 3, height = THESIS_HEIGHT)
make_assets(nine, "appE-overreporting-ratio-all", ncol = 3, height = 6)
