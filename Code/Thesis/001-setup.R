## Shared setup for all Code/Thesis scripts: paths, table style, ggplot theme.
## Sourced by every chNN-*.R script and by 000-build-all.R. Run from the repo
## root (Tax_Evasion_Productivity/), same convention as Code/Deconvolution/*.R.

library(tidyverse)
library(tinytable)

THESIS_DIR   <- "Thesis"
FIGURES_DIR  <- file.path(THESIS_DIR, "figures")
TABLES_DIR   <- file.path(THESIS_DIR, "tables")
PRODUCTS_DIR <- "Code/Products"

dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)

## Table rendering (tinytable -> pdflatex -> PNG, both kept). Reuses
## Code/Deconvolution/050-render-tbls.R, extended there (not here) with a
## `linewidth_pt` argument and a DPI-tagging fix.
source("Code/Deconvolution/050-render-tbls.R")

## The book's real \textwidth for the scrreport/DIV=11/letter setup in
## Thesis/_quarto.yml, computed once via a throwaway
## `\typeout{\the\textwidth}` standalone compile (2026-09-22) -- NOT
## hand-guessed. Recompute if the document class/paper size/DIV ever
## changes. Every thesis table is built against THIS value so that
## tt(..., width = <fraction>) means "that fraction of the actual book
## page", not the standalone-class default (~345pt/4.79in) -- fixes tables
## rendering at inconsistent apparent sizes (2026-09-22).
THESIS_TEXTWIDTH_PT <- 446.76

## JMP/paper.qmd's own real \textwidth (documentclass article, 12pt,
## margin=1in), computed the same throwaway-compile way, 2026-09-22:
## 469.76pt -- only ~5% wider than the book's. Every table/figure asset is
## still pinned to THESIS_TEXTWIDTH_PT (one shared PNG, symlinked into both
## JMP/tables|figures and used here -- "they can be the same" per chat) --
## at width=1 this leaves a small (~5%) margin of extra whitespace on the
## JMP page, never an overflow, so a single pin is safe for both documents.
## Recorded here only so the 5% gap is a documented, deliberate choice, not
## a forgotten discrepancy -- do not add a second JMP-specific pin unless a
## real visual problem shows up.
JMP_TEXTWIDTH_PT <- 469.76

## Wrapper fixing the output dir to Thesis/tables/ and pinning the render's
## \linewidth to the book's real text width, so every ch0N script just
## calls render_thesis_table(tt_obj, "ch0N-slug") and, inside that script's
## own tt(...) call, sets `width = <fraction of 1>` deliberately (project
## default: width = 1, full text width, decided in chat 2026-09-22).
render_thesis_table <- function(tt_obj, slug) {
    render_png_tt_tbl(tt_obj, slug, out_dir = TABLES_DIR, linewidth_pt = THESIS_TEXTWIDTH_PT)
    invisible(file.path(TABLES_DIR, paste0(slug, ".png")))
}

## Shared ggplot theme for thesis figures (fixed page-width sizing; distinct
## from the slide-oriented theming in the Quarto-Slides scripts).
THESIS_DPI    <- 300
THESIS_WIDTH  <- 6.5   # in, matches a standard book page text width
THESIS_HEIGHT <- 4.5

theme_thesis <- function(base_size = 11) {
    theme_minimal(base_size = base_size) +
        theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )
}

save_thesis_plot <- function(plot, slug, width = THESIS_WIDTH, height = THESIS_HEIGHT) {
    for (ext in c("png", "pdf")) {
        ggsave(
            file.path(FIGURES_DIR, paste0(slug, ".", ext)),
            plot = plot, width = width, height = height, dpi = THESIS_DPI
        )
    }
    invisible(file.path(FIGURES_DIR, paste0(slug, ".png")))
}

## Base-graphics counterpart to save_thesis_plot(), for the handful of
## figures built with persp()/trans3d() instead of ggplot2 (no 3D plotting
## package is installed in this project -- see 1289-cube-3dplot.R's own
## header comment). `plot_fn` is a zero-arg function that draws the whole
## plot via base graphics calls (persp(), points(), text(), ...); it is run
## once per device so both formats exist, matching every ggplot2 figure.
save_thesis_base_plot <- function(plot_fn, slug, width = THESIS_WIDTH, height = THESIS_HEIGHT) {
    pdf(file.path(FIGURES_DIR, paste0(slug, ".pdf")), width = width, height = height)
    plot_fn()
    dev.off()
    png_path <- file.path(FIGURES_DIR, paste0(slug, ".png"))
    png(png_path, width = width, height = height, units = "in", res = THESIS_DPI)
    plot_fn()
    dev.off()
    ## Found 2026-09-22 (first real use of this function, building ch08's base-graphics
    ## figures): base R's png(..., res=) sets the RASTERIZATION resolution but, unlike
    ## ggsave(), does not reliably write it into the PNG's own pHYs chunk -- `identify`
    ## showed units=Undefined at a nominal 72x72 despite res=300, the exact same class of
    ## bug the table pipeline's `-units PixelsPerInch` fix (050-render-tbls.R,
    ## 2026-09-22 earlier the same day) already diagnosed and fixed for magick's PDF->PNG
    ## conversion. Same fix here: re-stamp the density directly with magick after the file
    ## exists, so Quarto's \pandocbounded reads the real physical size instead of guessing
    ## 72dpi and rendering at some other, inconsistent apparent size.
    system2("magick", c(shQuote(png_path), "-density", THESIS_DPI, "-units", "PixelsPerInch", shQuote(png_path)))
    invisible(png_path)
}
