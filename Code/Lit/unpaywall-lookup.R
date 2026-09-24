## Unpaywall open-access lookup (and optional PDF download) ------------------
## Queries Unpaywall via the roadoi R wrapper for a set of DOIs, prints whether
## each is open access and its best PDF link, and optionally downloads the PDFs
## into Lit-Papers/.
##
## PRIVACY: Unpaywall requires an email. It is read ONLY from the environment
## variable UNPAYWALL_EMAIL, set in the user-level ~/.Renviron (outside this
## repo), never hard-coded, never passed as a CLI arg, never printed or logged.
## Error messages are scrubbed of it before printing. Setup, once:
##   usethis::edit_r_environ()          # opens ~/.Renviron
##   UNPAYWALL_EMAIL=<academic email>   # add this line, save, restart R
##
## Usage:
##   Rscript Code/Lit/unpaywall-lookup.R dois=10.1086/381476,10.1016/j.jpubeco.2008.09.004
##   Rscript Code/Lit/unpaywall-lookup.R dois=10.1086/381476 download=TRUE

source("Code/Deconvolution/utils-cli.R")

defaults <- list(
    dois     = "10.1086/381476",
    download = FALSE,
    out_dir  = "Lit-Papers"
)
opt <- parse_cli_args(defaults)

email <- Sys.getenv("UNPAYWALL_EMAIL")
if (!nzchar(email)) {
    stop("UNPAYWALL_EMAIL is not set. Add it to ~/.Renviron (see header), then restart R.")
}
scrub <- function(x) gsub(email, "<email>", x, fixed = TRUE)

if (!requireNamespace("roadoi", quietly = TRUE)) stop("Install roadoi: install.packages('roadoi')")

res <- tryCatch(
    roadoi::oadoi_fetch(dois = opt$dois, email = email, .progress = "none"),
    error = function(e) stop(scrub(conditionMessage(e)), call. = FALSE)
)
rm(email)

best_pdf <- vapply(seq_len(nrow(res)), function(i) {
    loc <- res$best_oa_location[[i]]
    if (is.null(loc) || nrow(loc) == 0 || is.null(loc$url_for_pdf)) NA_character_
    else as.character(loc$url_for_pdf[1])
}, character(1))

out <- data.frame(doi = res$doi, is_oa = res$is_oa, pdf = best_pdf,
                  title = substr(res$title, 1, 60))
print(out, right = FALSE)

if (isTRUE(opt$download)) {
    for (i in which(!is.na(out$pdf))) {
        dest <- file.path(opt$out_dir, paste0(gsub("[^A-Za-z0-9.-]", "_", out$doi[i]), ".pdf"))
        ok <- tryCatch({
            utils::download.file(out$pdf[i], dest, mode = "wb", quiet = TRUE)
            TRUE
        }, error = function(e) FALSE)
        cat(sprintf("%s -> %s\n", out$doi[i], if (ok) dest else "download failed"))
    }
    if (!any(!is.na(out$pdf))) cat("No open-access PDFs to download.\n")
}
