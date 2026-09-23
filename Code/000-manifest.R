## Static dependency scan of Code/**/*.R and Paper|Quarto-Slides|Thesis/**/*.qmd (2026-09-22).
## Best-effort regex scan (NOT a full parser) of load/save/readRDS/saveRDS/read.csv/write.csv/ggsave/
## render_png_tt_tbl/render_png_etbl/source calls in R files, and markdown image refs `![](path)` in
## qmd files. Builds a producer/consumer graph and flags:
##   (a) BROKEN LOADS: a script loads/reads a file with no producing script found AND the file isn't on
##       disk -- the exact bug class hit twice this session (1475 loading an RData 1474 never saved;
##       930.1-fs-se-het.R depending on wip_df, silently inherited from 915.1-size.RData, not declared in
##       itself). Catches this as "no rule to make target" would in a real Makefile, without needing one.
##   (b) UNREFERENCED OUTPUTS: a script writes a file that no other script loads and no qmd references --
##       informational (many are fine: final deliverables, superseded runs, diagnostics).
##   (c) For every Code/Thesis/*.R script: its transitive upstream (which Code/Deconvolution/Colombia
##       scripts it descends from, via the Products files it loads) -- the port-traceability ask.
## Re-run any time: Rscript Code/000-manifest.R. Writes Code/MANIFEST.md.
options(warn = -1)
## Scope (2026-09-22, user): Code/Ecuador and Code/Spain are unrelated projects living in the same Code/ folder --
## excluded entirely, not just from the issues report. Files whose basename starts with "_" are this project's own
## convention for known-deprecated/unused scripts -- also excluded entirely. Active code is Deconvolution, Rcpp,
## C-estimator, Thesis; Colombia and Stata are kept in scope (Colombia data wrangling is live; some Stata files may
## still be used) -- note .do files aren't scanned by this tool yet (R-only regexes), only listed if referenced.
r_files <- list.files("Code", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
r_files <- r_files[!grepl("^Code/(Ecuador|Spain)/", r_files) & !grepl("^_", basename(r_files))]
qmd_files <- list.files(c("Paper", "Quarto-Slides", "Thesis"), pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)

find_captures <- function(lines, pattern) {
    out <- character(0)
    for (ln in lines) {
        mm <- gregexpr(pattern, ln, perl = TRUE)[[1]]
        if (mm[1] == -1) next
        st <- attr(mm, "capture.start"); ln_ <- attr(mm, "capture.length")
        if (is.null(st)) next
        for (i in seq_len(nrow(st))) if (st[i, 1] > 0) out <- c(out, substr(ln, st[i, 1], st[i, 1] + ln_[i, 1] - 1))
    }
    out
}
norm <- function(x) trimws(gsub("\\\\", "/", x))

pat <- list(
    load    = '(?i)\\bload\\(\\s*(?:file\\s*=\\s*)?["\']([^"\']+\\.[Rr][Dd]ata)["\']',
    save    = '(?i)\\bsave\\([^)]*?file\\s*=\\s*["\']([^"\']+\\.[Rr][Dd]ata)["\']',
    readcsv = '(?i)read\\.csv\\(\\s*["\']([^"\']+\\.csv)["\']',
    writecsv= '(?i)write\\.csv\\([^)]*?["\']([^"\']+\\.csv)["\']',
    readrds = '(?i)readRDS\\(\\s*["\']([^"\']+\\.rds)["\']',
    saverds = '(?i)saveRDS\\([^)]*?["\']([^"\']+\\.rds)["\']',
    ggsave  = '(?i)ggsave\\(\\s*["\']([^"\']+\\.(?:png|pdf))["\']',
    ggsave2 = '(?i)ggsave\\([^)]*?filename\\s*=\\s*["\']([^"\']+)["\']',
    source  = '(?i)\\b(?:sys\\.)?source\\(\\s*["\']([^"\']+\\.R)["\']',
    tblpng  = 'render_png_tt_tbl\\(\\s*[^,]+,\\s*["\']([^"\']+)["\']',
    tblpng_od = 'render_png_tt_tbl\\([^)]*out_dir\\s*=\\s*["\']([^"\']+)["\']',
    tblpng2 = 'render_png_etbl\\(\\s*[^,]+,[^,]+,\\s*["\']([^"\']+)["\']'
)

produces <- list(); consumes <- list()
add <- function(l, script, target) c(l, list(data.frame(script = script, target = norm(target), stringsAsFactors = FALSE)))

for (f in r_files) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    lines <- lines[!grepl("^\\s*#", lines)]              # drop full-line comments (avoids scanning dead/commented-out load() calls as live)
    lines <- sub("(?<!['\"])#.*$", "", lines, perl = TRUE) # strip trailing comments not inside a quoted string (best-effort)
    txt <- paste(lines, collapse = " ")   # also scan joined text for calls split across lines
    all_lines <- c(lines, txt)
    for (t in find_captures(all_lines, pat$load))    consumes <- add(consumes, f, t)
    for (t in find_captures(all_lines, pat$save))    produces <- add(produces, f, t)
    for (t in find_captures(all_lines, pat$readcsv)) consumes <- add(consumes, f, t)
    for (t in find_captures(all_lines, pat$writecsv))produces <- add(produces, f, t)
    for (t in find_captures(all_lines, pat$readrds)) consumes <- add(consumes, f, t)
    for (t in find_captures(all_lines, pat$saverds)) produces <- add(produces, f, t)
    for (t in find_captures(all_lines, pat$ggsave))  produces <- add(produces, f, t)
    for (t in find_captures(all_lines, pat$ggsave2)) produces <- add(produces, f, t)
    for (t in find_captures(all_lines, pat$source))  consumes <- add(consumes, f, t)
    ## table-png helper: reconstruct <out_dir>/<name>.png (+ .pdf), default out_dir="Paper/tbls"
    names_ <- find_captures(all_lines, pat$tblpng); names2_ <- find_captures(all_lines, pat$tblpng2)
    ods <- find_captures(all_lines, pat$tblpng_od)
    if (length(names_) || length(names2_)) {
        od <- if (length(ods)) ods[1] else "Paper/tbls"
        for (nm in c(names_, names2_)) { produces <- add(produces, f, file.path(od, paste0(nm, ".png")))
                                          produces <- add(produces, f, file.path(od, paste0(nm, ".pdf"))) }
    }
}
produces <- if (length(produces)) do.call(rbind, produces) else data.frame(script = character(0), target = character(0))
consumes <- if (length(consumes)) do.call(rbind, consumes) else data.frame(script = character(0), target = character(0))

qmd_refs <- list()
img_pat <- '!\\[[^\\]]*\\]\\(([^)]+\\.(?:png|pdf|jpg|jpeg))\\)'
for (f in qmd_files) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    for (t in find_captures(lines, img_pat)) {
        p <- norm(t)
        if (!grepl("^(Paper|Code|Thesis|Quarto-Slides|/)", p)) {
            p <- normalizePath(file.path(dirname(f), p), mustWork = FALSE)
            p <- sub(paste0("^", normalizePath("."), "/"), "", p)
        }
        qmd_refs <- add(qmd_refs, f, p)
    }
}
qmd_refs <- if (length(qmd_refs)) do.call(rbind, qmd_refs) else data.frame(script = character(0), target = character(0))

## %% Issue (a): broken loads/reads/sources -------------------------------------------------------
has_producer <- consumes$target %in% produces$target | file.exists(consumes$target) |
    (grepl("\\.R$", consumes$target) & consumes$target %in% r_files)
broken <- consumes[!has_producer, ]
broken <- broken[!duplicated(broken), ]

## %% Issue (b): unreferenced outputs --------------------------------------------------------------
is_consumed <- produces$target %in% consumes$target | produces$target %in% qmd_refs$target
orphans <- produces[!is_consumed, ]
orphans <- orphans[!duplicated(orphans), ]

## %% Traceability: for every Code/Thesis/*.R script, its transitive upstream ------------------------
thesis_scripts <- r_files[grepl("^Code/Thesis/", r_files)]
upstream_of <- function(script, depth = 6) {
    seen_scripts <- character(0); frontier <- script; chain <- list()
    for (d in seq_len(depth)) {
        if (length(frontier) == 0) break
        my_targets <- consumes$target[consumes$script %in% frontier]
        my_targets <- unique(my_targets)
        prod_scripts <- unique(produces$script[produces$target %in% my_targets])
        src_scripts  <- unique(my_targets[grepl("\\.R$", my_targets) & my_targets %in% r_files])
        next_frontier <- setdiff(union(prod_scripts, src_scripts), seen_scripts)
        if (length(next_frontier) == 0) break
        chain[[d]] <- next_frontier
        seen_scripts <- union(seen_scripts, next_frontier); frontier <- next_frontier
    }
    unlist(chain)
}

## %% Write MANIFEST.md ------------------------------------------------------------------------------
L <- c(
"# Code manifest (auto-generated)",
"",
sprintf("Regenerate: `Rscript Code/000-manifest.R`. Scanned %d `.R` files under `Code/` and %d `.qmd` files under `Paper/`, `Quarto-Slides/`, `Thesis/`.", length(r_files), length(qmd_files)),
"",
"Best-effort regex scan of `load`/`save`/`readRDS`/`saveRDS`/`read.csv`/`write.csv`/`ggsave`/`render_png_tt_tbl`/`render_png_etbl`/`source` calls in R, and markdown image refs in qmd. Not a full parser: misses dynamically-built paths (`sprintf`-constructed filenames), positional (non-`file=`) `save()` calls, and non-image asset refs. Treat gaps below as \"not caught by the scan\", not \"doesn't exist\".",
"",
"## Issues",
"",
sprintf("### Broken loads/reads/sources (%d)", nrow(broken)),
"A script loads/reads/sources a file with no producing script found in this scan, AND the file is not currently on disk. Each of these either needs its producing script re-run, or is stale/dead and safe to ignore once checked.",
"",
"| Script | Missing target |",
"|---|---|",
if (nrow(broken)) sprintf("| `%s` | `%s` |", broken$script, broken$target) else "| (none found) | |",
"",
sprintf("### Unreferenced outputs (%d)", nrow(orphans)),
"A script writes a file that no other scanned script loads/reads, and no qmd references. Informational -- most are final deliverables (consumed by a qmd not caught by this scan's image-ref pattern), superseded intermediate runs, or one-off diagnostics. Not automatically dead.",
"",
"<details><summary>expand</summary>",
"",
"| Script | Output |",
"|---|---|",
if (nrow(orphans)) sprintf("| `%s` | `%s` |", orphans$script, orphans$target) else "| (none found) | |",
"",
"</details>",
"",
"## Code/Thesis: traceability to original scripts",
"",
"For each `Code/Thesis/*.R` (the new, curated pipeline), its transitive upstream in the legacy `Code/Deconvolution`/`Code/Colombia` tree -- i.e. which original scripts its own inputs were produced by.",
""
)
for (s in thesis_scripts) {
    up <- upstream_of(s)
    L <- c(L, sprintf("### `%s`", s),
           sprintf("- Direct inputs: %s", if (nrow(consumes[consumes$script == s, ])) paste0("`", consumes$target[consumes$script == s], "`", collapse = ", ") else "(none found)"),
           sprintf("- Produces: %s", if (nrow(produces[produces$script == s, ])) paste0("`", produces$target[produces$script == s], "`", collapse = ", ") else "(none found)"),
           sprintf("- Transitive upstream (legacy scripts this descends from): %s", if (length(up)) paste0("`", up, "`", collapse = ", ") else "(none found)"),
           "")
}

L <- c(L, "## Full producer/consumer graph", "", "One row per scanned `load`/`save`/`read.csv`/`write.csv`/`readRDS`/`saveRDS`/`ggsave`/table-png/`source` call. `role` is `produces` (writes/creates) or `consumes` (reads/depends on).", "",
       "<details><summary>expand (~%d rows)</summary>" |> sprintf(nrow(produces) + nrow(consumes)), "",
       "| Script | Role | Target |", "|---|---|---|",
       sprintf("| `%s` | produces | `%s` |", produces$script, produces$target),
       sprintf("| `%s` | consumes | `%s` |", consumes$script, consumes$target),
       "", "</details>")

writeLines(L, "Code/MANIFEST.md")
cat(sprintf("Saved: Code/MANIFEST.md (%d broken loads, %d unreferenced outputs, %d Thesis scripts traced)\n",
            nrow(broken), nrow(orphans), length(thesis_scripts)))
