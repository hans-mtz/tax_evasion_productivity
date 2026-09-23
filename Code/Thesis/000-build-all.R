## Rebuilds every figure/table asset the Thesis book uses, then checks the
## manifest: any qmd figure/table reference with no file on disk, and any
## Thesis/figures|tables file that no chapter references. Run from the repo
## root: Rscript Code/Thesis/000-build-all.R

source("Code/Thesis/001-setup.R")

scripts <- sort(setdiff(
    list.files("Code/Thesis", pattern = "^ch[0-9]+-.*\\.R$", full.names = TRUE),
    character(0)
))

cat("Running", length(scripts), "Thesis asset scripts...\n")
for (s in scripts) {
    cat("--", s, "\n")
    source(s, chdir = FALSE)
}

## --- Manifest check ---------------------------------------------------
manifest_path <- "Code/Thesis/manifest.csv"
if (file.exists(manifest_path)) {
    manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
    missing_on_disk <- manifest$asset[!file.exists(file.path(THESIS_DIR, manifest$asset))]
    if (length(missing_on_disk) > 0) {
        cat("\nMISSING (in manifest, not on disk):\n"); print(missing_on_disk)
    }
} else {
    cat("\nNo manifest.csv found; skipping missing-file check.\n")
}

on_disk <- c(
    file.path("figures", list.files(FIGURES_DIR, pattern = "\\.png$")),
    file.path("tables",  list.files(TABLES_DIR,  pattern = "\\.png$"))
)
if (file.exists(manifest_path)) {
    manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
    orphans <- setdiff(on_disk, manifest$asset)
    if (length(orphans) > 0) {
        cat("\nORPHANS (on disk, not in manifest -- check if any chapter still uses them):\n")
        print(orphans)
    }
}

cat("\nDone.\n")
