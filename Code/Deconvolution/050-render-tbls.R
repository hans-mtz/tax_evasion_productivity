## Libraries and packages
library(fixest)
library(tinytable)

## %% Render PNG table from dplyr table


render_png_tt_tbl <- function(tt_tbl_in, file_name, out_dir = "Paper/tbls", linewidth_pt = NULL){
    tmp_tex <- tempfile(fileext = ".tex", tmpdir = out_dir)
    on.exit(unlink(tmp_tex), add = TRUE)
    print(tmp_tex)

    tt_tbl_in |>
    save_tt(
            output = tmp_tex,
            overwrite = TRUE
        )

    tex_lines <- readLines(tmp_tex)
    ## tinytable wraps the talltblr in \begin{table}...\end{table} whenever
    ## notes= or caption= is used. The standalone class captures document
    ## content in a restricted-horizontal box to compute its tight bounding
    ## box, and floats (table/table*) are not allowed inside that box --
    ## this throws "Not allowed in LR mode" and silently yields a 0pt-page
    ## PDF (caught 2026-09-17 on a machine with a newer tabularray where
    ## note{}/caption's internal implementation started hitting this; older
    ## tabularray versions may not trigger it, which is why this went
    ## unnoticed on some machines). There's nothing else on a standalone
    ## page to float around, so the wrapper is safe to drop outright --
    ## \centering and the table content itself are unaffected.
    tex_lines <- tex_lines[!grepl("^\\\\(begin|end)\\{table\\*?\\}\\s*$", tex_lines)]

    preamble <- "
\\documentclass{standalone}
\\usepackage{xcolor}
\\usepackage{tabularray}
\\UseTblrLibrary{booktabs}
\\IfFileExists{tblrlibrotating.sty}{\\UseTblrLibrary{rotating}}{}
\\UseTblrLibrary{siunitx}
\\usepackage{float}
\\usepackage{graphicx}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}
"
    ## Optional: pin \linewidth/\textwidth to a caller-given value (in pt)
    ## BEFORE the table is typeset, so tt(..., width=<fraction>) maps onto a
    ## real, known physical size instead of the standalone/article default
    ## (~345pt/4.79in) -- lets a fraction mean "that fraction of the actual
    ## book's text width", not some arbitrary standalone default. Code/Thesis
    ## callers pass the book's real \textwidth (446.76pt/6.18in for the
    ## scrreport/DIV=11/letter setup, computed once via a throwaway
    ## `\typeout{\the\textwidth}` compile -- not hardcoded here). NULL
    ## (default) preserves old behavior exactly, so existing Paper/ callers
    ## that never pass this are unaffected.
    linewidth_cmd <- if (!is.null(linewidth_pt)) {
        sprintf("\\setlength{\\linewidth}{%gpt}\\setlength{\\textwidth}{%gpt}", linewidth_pt, linewidth_pt)
    } else {
        ""
    }

    cat(
        preamble,
        "\\begin{document}",
        linewidth_cmd,
        tex_lines,
        "\\end{document}",
        file = tmp_tex,
        sep = "\n"
    )
    tmp_sh <- tempfile(fileext = ".sh")
    on.exit(unlink(tmp_sh), add = TRUE)
    cat(
        "#!/bin/zsh",
        "export PATH=\"/usr/local/bin:$PATH\"",
        "export PATH=\"/Library/TeX/texbin:$PATH\"",
        "export PATH=\"/opt/homebrew/bin:$PATH\"",
        "echo $PATH",
        paste0("pdflatex -synctex=1 -interaction=nonstopmode -file-line-error -recorder -output-directory=", out_dir, " '", tmp_tex,"'"),
        paste("mv", gsub("\\.tex", ".pdf", tmp_tex), paste0(out_dir,"/", file_name, ".pdf")),
        ## -units PixelsPerInch: without it, ImageMagick's -density sets the
        ## RASTERIZATION resolution but doesn't reliably tag the output
        ## PNG's pHYs chunk, so pdflatex (assuming 72dpi on an untagged PNG)
        ## reads a 300dpi image as ~4.17x too large, and Quarto's
        ## \pandocbounded then clamps it down to fill the full page width
        ## regardless of the table's real content size -- this was the
        ## actual cause of "tables render in all sorts of sizes" (diagnosed
        ## 2026-09-22, Thesis/ book PDF).
        paste("magick -density 300 -units PixelsPerInch", paste0(out_dir,"/", file_name, ".pdf"),paste0(out_dir,"/", file_name, ".png")),
        paste0("rm -f ", out_dir, "/*.aux ", out_dir, "/*.fls ", out_dir, "/*.synctex.gz ", out_dir, "/*.log"),
        sep = "\n",
        file = tmp_sh
    )
    system(paste("chmod +x", tmp_sh), intern = TRUE)
    system2(tmp_sh, wait = TRUE)

}



## %% Render PNG tables from Fixest Regression etables

render_png_etbl <- function(tbl_in, dict=dict, file_name, out_dir = "Paper/tbls", linewidth_pt = NULL){
    tmp_tex <- tempfile(fileext = ".tex", tmpdir = out_dir)
    on.exit(unlink(tmp_tex), add = TRUE)
    print(tmp_tex)

    etable(tbl_in, dict = dict, file = tmp_tex)
    tex_lines <- readLines(tmp_tex)

    preamble <- "
\\documentclass{standalone}
\\usepackage{xcolor}
\\usepackage{tabularray}
\\UseTblrLibrary{booktabs}
\\IfFileExists{tblrlibrotating.sty}{\\UseTblrLibrary{rotating}}{}
\\UseTblrLibrary{siunitx}
\\usepackage{float}
\\usepackage{graphicx}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}
"
    ## Same rationale as render_png_tt_tbl() above.
    linewidth_cmd <- if (!is.null(linewidth_pt)) {
        sprintf("\\setlength{\\linewidth}{%gpt}\\setlength{\\textwidth}{%gpt}", linewidth_pt, linewidth_pt)
    } else {
        ""
    }

    cat(
        preamble,
        "\\begin{document}",
        linewidth_cmd,
        tex_lines,
        "\\end{document}",
        file = tmp_tex,
        sep = "\n"
    )
    tmp_sh <- tempfile(fileext = ".sh")
    on.exit(unlink(tmp_sh), add = TRUE)
    cat(
        "#!/bin/zsh",
        "export PATH=\"/usr/local/bin:$PATH\"",
        "export PATH=\"/Library/TeX/texbin:$PATH\"",
        "export PATH=\"/opt/homebrew/bin:$PATH\"",
        "echo $PATH",
        paste0("pdflatex -synctex=1 -interaction=nonstopmode -file-line-error -recorder -output-directory=", out_dir, " '", tmp_tex,"'"),
        paste("mv", gsub("\\.tex", ".pdf", tmp_tex), paste0(out_dir,"/", file_name, ".pdf")),
        paste("magick -density 300 -units PixelsPerInch", paste0(out_dir,"/", file_name, ".pdf"),paste0(out_dir,"/", file_name, ".png")),
        paste0("rm -f ", out_dir, "/*.aux ", out_dir, "/*.fls ", out_dir, "/*.synctex.gz ", out_dir, "/*.log"),
        sep = "\n",
        file = tmp_sh
    )
    system(paste("chmod +x", tmp_sh), intern = TRUE)
    system2(tmp_sh, wait = TRUE)

}
