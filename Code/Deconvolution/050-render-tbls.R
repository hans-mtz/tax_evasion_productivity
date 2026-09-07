## Libraries and packages
library(fixest)
library(tinytable)

## %% Render PNG table from dplyr table


render_png_tt_tbl <- function(tt_tbl_in, file_name, out_dir = "Paper/tbls"){
    tmp_tex <- tempfile(fileext = ".tex", tmpdir = out_dir)
    on.exit(unlink(tmp_tex), add = TRUE)
    print(tmp_tex)

    tt_tbl_in |>
    save_tt(
            output = tmp_tex,
            overwrite = TRUE
        )

    tex_lines <- readLines(tmp_tex)

    preamble <- "
\\documentclass{standalone}
\\usepackage{xcolor}
\\usepackage{tabularray}
\\UseTblrLibrary{booktabs}
\\UseTblrLibrary{rotating}
\\UseTblrLibrary{siunitx}
\\usepackage{float}
\\usepackage{graphicx}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\UseTblrLibrary{siunitx}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}
"
    cat(
        preamble,
        "\\begin{document}",
        tex_lines,
        "\\end{document}",
        file = tmp_tex,
        sep = "\n"
    )
    tmp_sh <- tempfile(fileext = ".sh")
    on.exit(unlink(tmp_sh), add = TRUE)
    cat(
        "#!/bin/zsh",
        "export PATH=\"/Library/TeX/texbin:$PATH\"",
        "export PATH=\"/usr/local/bin:$PATH\"",
        "echo $PATH",
        paste0("'/Library/TeX/texbin/pdflatex' -synctex=1 -interaction=nonstopmode -file-line-error -recorder -output-directory=", out_dir, " '", tmp_tex,"'"),
        paste("mv", gsub("\\.tex", ".pdf", tmp_tex), paste0(out_dir,"/", file_name, ".pdf")),
        paste("'/usr/local/bin/magick' -density 300", paste0(out_dir,"/", file_name, ".pdf"),paste0(out_dir,"/", file_name, ".png")),
        paste0("rm -f ", out_dir, "/*.aux ", out_dir, "/*.fls ", out_dir, "/*.synctex.gz"),
        sep = "\n",
        file = tmp_sh
    )
    system(paste("chmod +x", tmp_sh), intern = TRUE)
    system2(tmp_sh, wait = TRUE)

}



## %% Render PNG tables from Fixest Regression etables

render_png_etbl <- function(tbl_in, dict=dict, file_name, out_dir = "Paper/tbls"){
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
\\UseTblrLibrary{rotating}
\\UseTblrLibrary{siunitx}
\\usepackage{float}
\\usepackage{graphicx}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\UseTblrLibrary{siunitx}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}
"
    cat(
        preamble,
        "\\begin{document}",
        tex_lines,
        "\\end{document}",
        file = tmp_tex,
        sep = "\n"
    )
    tmp_sh <- tempfile(fileext = ".sh")
    on.exit(unlink(tmp_sh), add = TRUE)
    cat(
        "#!/bin/zsh",
        "export PATH=\"/Library/TeX/texbin:$PATH\"",
        "export PATH=\"/usr/local/bin:$PATH\"",
        "echo $PATH",
        paste0("'/Library/TeX/texbin/pdflatex' -synctex=1 -interaction=nonstopmode -file-line-error -recorder -output-directory=", out_dir, " '", tmp_tex,"'"),
        paste("mv", gsub("\\.tex", ".pdf", tmp_tex), paste0(out_dir,"/", file_name, ".pdf")),
        paste("'/usr/local/bin/magick' -density 300", paste0(out_dir,"/", file_name, ".pdf"),paste0(out_dir,"/", file_name, ".png")),
        paste0("rm -f ", out_dir, "/*.aux ", out_dir, "/*.fls ", out_dir, "/*.synctex.gz"),
        sep = "\n",
        file = tmp_sh
    )
    system(paste("chmod +x", tmp_sh), intern = TRUE)
    system2(tmp_sh, wait = TRUE)

}

