## Latex Standalone Tables as Figures/TikZ Figures

To include the latex TikZ figures/tables as figures in your slides, do the following:

1. Create the figure/table in PDF using a tex file (with TikZ code). Once the TEX file is ready you can run from terminal

```bash 
pdflatex file.tex
```
or

```bash 
lualatex file.tex
```

1. Once the PDF is ready, use `imagemagick` to convert it to PNG. In the terminal, run 

```bash
convert -density 600 file.pdf file.png
```

Note: `-density 600` can be adjusted to improve image quality. It stands for dots per inch (dpi).