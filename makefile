# Usually, only these lines need changing
QPAPFILE = Tax-Prod
QSLIFILE = Tax-Prod
RDIR = ./Code/Colombia
RDIR2 = ./Code/Deconvolution

QPAPDIR = ./Paper
QSLIDIR = ./Quarto-Slides
# list all R files
RFILES := $(wildcard $(RDIR)/*.R)
EXCLUDE := $(wildcard $(RDIR)/_*.R)
RFILES2 := $(wildcard $(RDIR2)/*.R)
EXCLUDE2 := $(wildcard $(RDIR2)/_*.R)
QFILES := $(wildcard $(QPAPDIR)/*.qmd)
QEXCLUDE := $(wildcard $(QPAPDIR)/_*.qmd)
RDEP := $(wildcard $(RDIR)/*beta_diff.R)
# excluding files that start with "_" during development
RFILES := $(filter-out $(EXCLUDE),$(RFILES))
RFILES2 := $(filter-out $(EXCLUDE2),$(RFILES2))
QFILES := $(filter-out $(QEXCLUDE),$(QPAPFILES))
QFILES := $(filter-out $(QPAPFILE).qmd,$(QPAPFILES))
RIND := $(filter-out $(RDEP),$(RFILES))


# Indicator files to show R file has run
OUT_FILES := $(RFILES:.R=.Rout)
OUT_FILES_DEC := $(RFILES2:.R=.Rout)
RDEPOUT := $(RIND:.R=.Rout)
# Targets
## Default target
main: $(RDIR)/main.Rout #$(filter-out $(RDIR)/main.Rout, $(OUT_FILES))

## Make all
# all: main paper slides
docs: jmp thesis

## Run R files
R: $(OUT_FILES) $(OUT_FILES_DEC)

R-code: $(OUT_FILES)

## Make paper
# paper: #$(QPAPDIR)/$(QPAPFILE).pdf
# 	quarto render $(QPAPDIR)/$(QPAPFILE).qmd 
# #	quarto render $(QPAPDIR)/$(QPAPFILE).qmd --to pdf -M include-in-header:packages.tex
# #	open -a Preview $(QPAPDIR)/$(QPAPFILE).pdf
# pdf:
# 	quarto render $(QPAPDIR)/$(QPAPFILE).qmd --to pdf -M include-in-header:_extensions/quarto-journals/jasa/packages.tex

# html:
# 	quarto render $(QPAPDIR)/$(QPAPFILE).qmd --to html
## Make slides
slides: #$(QSlIFILE).html
	quarto render $(QSLIDIR)/$(QSLIFILE).qmd
#	open -a Safari $(QSLIDIR)/$(QSLIFILE).html

jmp:
	quarto render JMP/paper.qmd

thesis:
	quarto render Thesis/index.qmd

# Rules
$(RDIR)/%.Rout: $(RDIR)/%.R 
	R CMD BATCH --no-save --no-restore-data $< $@

$(RDIR2)/%.Rout: $(RDIR2)/%.R 
	R CMD BATCH --no-save --no-restore-data $< $@

# # Compile main tex file and show errors
$(QPAPDIR)/$(QPAPFILE).pdf: $(QPAPDIR)/$(QPAPFILE).qmd #$(QFILES) #$(OUT_FILES) #$(CROP_FILES)
	quarto preview $<

# # Compile main tex file and show errors
# $(QSLIFILE).html: $(QSLIFILE).qmd $(OUT_FILES) #$(CROP_FILES)
#     quarto preview "$(QSLIDIR)/$(QSLIFILE).qmd"

# Dependencies
# May need to add something here if some R files depend on others.
$(RDIR)/main.Rout: $(RFILES)

# $(RDIR)/30_beta_diff.Rout $(RDIR)/40_beta_diff_yoy.Rout $(RDIR)/45_beta_diff.Rout :$(RDIR)/20_functions.R $(RDIR)/15_global_vars.R $(RDIR)/00_reading_data.R

# $(RDIR)/60_plot_discontinuity.Rout $(RDIR)/50_beta_diff.Rout $(RDIR)/30_beta_diff.Rout:$(RDIR)/20_functions.R $(RDIR)/15_global_vars.R $(RDIR)/00_reading_data.R

# $(RDEPOUT) : $(RIND)


# Clean up stray files
clean:
	rm -fv $(OUT_FILES) 
	rm -fv *.Rout *.RData
	rm -fv *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.out *.bcf *blx.bib *.run.xml
	rm -fv *.fdb_latexmk *.fls *.nav *.snm 
#	rm -fv $(TEXFILE).pdf
clean-out:
	rm -fv $(OUT_FILES) *.Rout

clean-tex:
	rm -fv Paper/*.aux Paper/*.log Paper/*.toc Paper/*.blg Paper/*.bbl Paper/*.synctex.gz
	rm -fv Paper/*.fdb_latexmk Paper/*.fls 
	rm -fv Quarto-Slides/*.aux Quarto-Slides/*.log Quarto-Slides/*.toc Quarto-Slides/*.blg Quarto-Slides/*.bbl Quarto-Slides/*.synctex.gz
	rm -fv Quarto-Slides/*.fdb_latexmk Quarto-Slides/*.fls Quarto-Slides/*.nav Quarto-Slides/*.snm Quarto-Slides/*.vrb
	rm -fv Paper/tbls/*.aux Paper/tbls/*.fls Paper/tbls/*.log Paper/tbls/*.synctex.gz Paper/tbls/*.fdb_latexmk 
.PHONY: all docs clean paper slides clean-tex clean-out jmp thesis 