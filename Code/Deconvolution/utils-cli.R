## CLI helper for Rscript / R CMD BATCH driver scripts ----------------------
## Convention (2026-08-28): a driver script declares a `defaults` list, calls
## parse_cli_args(defaults) to get args overridden from the command line
## (key=value, comma-separated for vector-valued defaults), and calls
## log_run_header() once at the top so every .Rout/.log carries a header
## recording exactly what was run -- source()-ing interactively with no args
## reproduces `defaults` unchanged, so nothing breaks for interactive use.
##
## Usage from the shell (R CMD BATCH's classic args idiom, same parsing as
## Rscript's commandArgs()):
##   R CMD BATCH "--args ins=lag_m n_burn=100 n_keep=1000 grid_probs=0.9,0.75,0.5" \
##       Code/Deconvolution/1210-stage2-elvis-driver.R \
##       Code/Products/logs/1210-lag_m-drop-nburn100.Rout
## or with Rscript (stdout/stderr redirected by the shell instead of R CMD
## BATCH's automatic .Rout):
##   Rscript Code/Deconvolution/1210-stage2-elvis-driver.R ins=lag_m n_burn=100 \
##       > Code/Products/logs/1210-lag_m-drop-nburn100.log 2>&1

parse_cli_args <- function(defaults) {
    raw <- commandArgs(trailingOnly = TRUE)
    opt <- defaults
    for (a in raw) {
        kv <- strsplit(a, "=", fixed = TRUE)[[1]]
        if (length(kv) != 2) stop(sprintf("Malformed arg '%s' (expected key=value)", a))
        key <- kv[1]; val <- kv[2]
        if (!key %in% names(defaults)) {
            stop(sprintf("Unknown arg '%s'; valid keys: %s", key, paste(names(defaults), collapse = ", ")))
        }
        target <- defaults[[key]]
        pieces <- strsplit(val, ",", fixed = TRUE)[[1]]
        opt[[key]] <- if (is.numeric(target)) as.numeric(pieces)
                      else if (is.logical(target)) as.logical(pieces)
                      else pieces
    }
    opt
}

log_run_header <- function(script_name, opt) {
    git_sha <- tryCatch(
        system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE),
        error = function(e) NA_character_
    )
    if (length(git_sha) == 0) git_sha <- NA_character_
    cat(sprintf("==== %s ====\n", script_name))
    cat(sprintf("time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    cat(sprintf("git:  %s\n", ifelse(is.na(git_sha), "unknown", git_sha)))
    cat("args:\n")
    for (nm in names(opt)) cat(sprintf("  %-12s = %s\n", nm, paste(opt[[nm]], collapse = ",")))
    cat("====================================\n")
}
