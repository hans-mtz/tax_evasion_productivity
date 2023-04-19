# Usually, only these lines need changing
QPAPFILE = Tax-Prod
QSLIFILE = JMP-update
RDIR = ./Code/Colombia
QPAPDIR = ./Paper
QSLIDIR = ./Quarto-Slides

# list all R files
RFILES := $(wildcard $(RDIR)/*.R)
EXCLUDE := $(wildcard $(RDIR)/_*.R)
QFILES := $(wildcard $(QPAPDIR)/*.qmd)
QEXCLUDE := $(wildcard $(QPAPDIR)/_*.qmd)
# excluding files that start with "_" during development
RFILES := $(filter-out $(EXCLUDE),$(RFILES))
QFILES := $(filter-out $(QEXCLUDE),$(QPAPFILES))
QFILES := $(filter-out $(QPAPFILE).qmd,$(QPAPFILES))
# Indicator files to show R file has run
OUT_FILES := $(RFILES:.R=.Rout)

# Default target
all: $(OUT_FILES)

# May need to add something here if some R files depend on others.

# RUN EVERY R FILE
$(RDIR)/%.Rout: $(RDIR)/%.R #$(RDIR)/functions.R
	R CMD BATCH --no-save --no-restore-data $< $@

# # Compile main tex file and show errors
$(QPAPDIR)/$(QPAPFILE).pdf: $(QPAPDIR)/$(QPAPFILE).qmd $(QFILES) #$(OUT_FILES) #$(CROP_FILES)
	quarto preview $<


# # Compile main tex file and show errors
# $(QSLIFILE).html: $(QSLIFILE).qmd $(OUT_FILES) #$(CROP_FILES)
#     quarto preview "$(QSLIDIR)/$(QSLIFILE).qmd"

# Dependencies
main.Rout: $(filter-out main.Rout, $(OUT_FILES))

# Run R files
R: $(OUT_FILES)

# View main tex file
paper: #$(QPAPDIR)/$(QPAPFILE).pdf
	quarto render $(QPAPDIR)/$(QPAPFILE).qmd
#	open -a Preview $(QPAPDIR)/$(QPAPFILE).pdf

# View main tex file
slides: #$(QSlIFILE).html
	quarto render $(QSLIDIR)/$(QSLIFILE).qmd
#	open -a Safari $(QSLIDIR)/$(QSLIFILE).html

# Clean up stray files
clean:
	rm -fv $(OUT_FILES) 
	rm -fv *.Rout *.RData
	rm -fv *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.out *.bcf *blx.bib *.run.xml
	rm -fv *.fdb_latexmk *.fls
#	rm -fv $(TEXFILE).pdf

.PHONY: all clean paper slides