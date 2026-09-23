# Thesis draft — working plan

Shared tracker (Hans + Claude). Update status in place; put dated notes in the log at the bottom.
**Goal, revised 2026-09-22: the Fri 2026-09-25 deadline is for the Job Market Paper (`JMP/`), not this thesis.** The thesis book stays in progress here and gets submitted next year (summer term) -- content written for these chapters now is shared with the JMP (see §9), so thesis progress and JMP progress are the same work, but JMP is the priority ordering when time is tight this week. Content over format either way.
**Roles:** Hans writes the prose. Claude scaffolds, ports, standardizes figures/tables/bib, renders, and edits/coaches.

Status legend: `[ ]` todo · `[~]` in progress · `[x]` done · `[-]` dropped

---

## 1. Decisions (settled 2026-09-21)

- Format: Quarto **book** project in `Thesis/`. No university template for now (sups care about content only).
- **Tables: tinytable + etable ONLY** (no kableExtra/`kbl`). Data tables -> `tt()`; fixest regressions -> `etable()`. Both render via `Code/Deconvolution/050-render-tbls.R`'s pdflatex->PNG pipeline (unchanged, reused as-is).
- **Figures: ggplot2 for everything 2D.** Base graphics (`persp()`/`trans3d()`) only for the one 3D grid plot (`ch08-elvis-cube`) -- no 3D package is installed project-wide, so this isn't worth introducing. Both get PNG+PDF via `Code/Thesis/001-setup.R`'s `save_thesis_plot()` / `save_thesis_base_plot()`.
- **LaTeX-special characters in ANY tinytable cell/column-name must be escaped**: literal `%` and `&` both corrupt the rendered table silently (comment-out-rest-of-line for `%`, extra-column-split for `&`) -- confirmed hard the hard way on `ch03-summary-stats` 2026-09-22 (see `feedback_tables_r_png.md`, now doubly confirmed). Always escape as `\%` / `\&` before passing to `tt()`.
- **`group_tt()` shifts row indices.** Any `style_tt(i=...)` call targeting a group-header row must use POST-`group_tt()`-insertion row numbers, not the row numbers computed on the ungrouped table -- caught same session, bold landed on the wrong row.
- **Literal `_` in a tinytable column name/cell is read as a LaTeX math-mode subscript trigger and garbles** (e.g. raw column name `n_sic` rendered as broken `n` + subscript). Rename to plain labels (`setNames()`/`colnames<-`) before `tt()`, same fix as the `%`/`&` traps above -- caught a 3rd/4th time in `ch03-top-industries-table.R`/`ch03-corps-by-industry-table.R`.
- **PNG asset sizing in the Quarto/LaTeX pipeline — the "magick/pandoc issue," root cause, fix, and how to tell if you've hit it again.** One underlying bug shows up in two places (tables and base-graphics figures); if a rendered PNG asset looks the wrong size in the book or JMP PDF — too big, too small, or inconsistent between assets that should match — **check this bullet first**, don't re-diagnose from scratch.

  **Root cause (both cases): a PNG file whose resolution isn't tagged, or is tagged in a unit pdflatex doesn't expect, gets read as 72dpi by default.** A 300dpi image read as 72dpi is interpreted as ~4x too large in physical size; Quarto's `\pandocbounded` then clamps it back down to the full page width regardless of the asset's real content size. This is the actual mechanism behind "tables/figures render in all sorts of sizes" — not randomness. **Diagnose directly with `identify -format "%wx%h  %xx%y ppi  units=%U\n" path/to/file.png`** (ImageMagick) — a broken asset shows `units=Undefined`; a correctly-tagged one shows `units=PixelsPerCentimeter` (ImageMagick's own preferred unit once explicitly told which one to tag) at ~118.11 ppcm (=300 ppi).

  **Table fix (2026-09-22):** `magick -density 300 file.pdf file.png` (`050-render-tbls.R`, inside `render_png_tt_tbl()`/`render_png_etbl()`) sets the *rasterization* resolution but doesn't reliably *tag* the output file's own resolution metadata. Fixed by adding `-units PixelsPerInch` to that same `magick` call. **Belt-and-braces control, also added:** both functions take an optional `linewidth_pt` that pins `\linewidth`/`\textwidth` inside the table's own standalone-class render to the BOOK's real text width (446.76pt/6.18in for `scrreport`/DIV=11/letter, computed once via a throwaway `\typeout{\the\textwidth}` compile -- recompute if the class/paper/DIV ever changes), so `tt(..., width=<fraction>)` means a deliberate fraction of the actual book page, not the standalone/`article` default (~345pt/4.79in). `Code/Thesis/001-setup.R`'s `render_thesis_table()` wraps this with `linewidth_pt = THESIS_TEXTWIDTH_PT` automatically. **Project default: every thesis table gets `width = 1`** (full text width) unless a script overrides it.

  **Figure fix, a second instance of the same bug, found 2026-09-22 while building ch. 8's assets:** `save_thesis_base_plot()` (`Code/Thesis/001-setup.R`, the base-graphics counterpart to `save_thesis_plot()`) used base R's `png(..., res=300)`, which — unlike `ggsave()` — does *not* reliably write that resolution into the file's own pHYs chunk either. Same symptom, same diagnosis via `identify`, same fix: re-stamp the file with `magick -density <dpi> -units PixelsPerInch <path> <path>` immediately after `dev.off()`. Now baked into `save_thesis_base_plot()` itself — any new base-graphics figure gets this for free, nothing to remember per-script. `ggplot2` figures saved via `save_thesis_plot()`/`ggsave()` were never affected — only the base-graphics path had this bug.

  **A related but genuinely different bug, don't conflate the two:** forcing a figure's *device canvas* down to a small size (e.g. `THESIS_WIDTH=6.5in`) while the plot's title/legend/axis text keep their original absolute sizes causes real content overlap (a title running off the page, a legend sitting on top of data) — this is a **layout** problem, has nothing to do with DPI tagging, and `identify` won't show anything wrong (the file's metadata is fine; the content itself is broken). Caught directly on ch. 8's figures by actually looking at the rendered PNGs, not just checking metadata. **Fix: render every figure at its own native designed size** (whatever width/height its original `ggsave()`/`png()` call already used — several of ch. 8's are 10-12in wide) and let Quarto/LaTeX's `\includegraphics` do the width-fit as a single proportional raster scale, which preserves every relative text/legend/data position exactly as designed. Don't shrink the R device canvas to try to pre-fit the page — that's LaTeX's job, not the plotting script's.

  **JMP vs. Thesis text width — a non-issue, deliberately not double-engineered:** JMP's own real `\textwidth` (`article`/12pt/1in margins) is 469.76pt, only ~5% wider than the book's 446.76pt (`JMP_TEXTWIDTH_PT` in `001-setup.R`, computed the same throwaway-compile way). Since figures/tables are shared (symlinked) between both documents, every asset stays pinned to `THESIS_TEXTWIDTH_PT` for both — the ~5% gap just leaves a little extra margin in the JMP, never an overflow. Don't add a second, JMP-specific pin unless a real visual problem actually shows up.
- **`width=1` on a table with one long-text column + several short numeric columns wraps the text column badly** (equal 1/ncol split forces it as narrow as the numeric columns -- one case wrapped to 7 lines). Fix: `tt()`'s `width` also accepts a per-column numeric vector (proportional, auto-normalized -- see `?tinytable::tt`), e.g. `width = c(4, 1, 1, 1, ...)` to weight the text column ~4x a numeric one. Applied in `ch03-top-industries-table.R`, `ch03-corps-by-industry-table.R`, `ch03-summary-stats-table.R`.
- **No `caption=` in any `tt()` call** (decided in chat 2026-09-22) -- Quarto's own `![Caption](path){#tbl-slug}` markdown is the single source of the caption/numbering now; a caption baked into the R-rendered PNG double-captioned every table page. Real explanatory text that isn't a title goes in `notes=` instead (rendered as a footnote line under the table).
- **Spelling (2026-09-23): full Canadian** (Canadian institution): labour, endeavour, behaviour, colour(ed), modelling/modelled, labelled, grey, centre, analogue, defence; keep *-ize* (Canadian standard). Applies to prose, captions, and asset labels in `Code/Thesis/` (not LaTeX commands such as `\centering` or `colorlinks`). Shared legacy scripts in `Code/Deconvolution/` that also feed the slides are left unchanged.
- **Test naming (2026-09-23): "conservative test" ($TS_{\text{cons}}$), never "hard test"**, in prose, captions, and asset labels (ch. 8, appendix A, `Code/Thesis/ch08-*.R`). "Soft test" unchanged. Asset *file names* (`ch08-*-hard.png`) and internal R variables (`TS_hard`) were left as they are. `Code/Deconvolution/1300-*` still says HARD because it also feeds the slides; `ch08-laffer-ci-hard-plot.R` overrides the title.
- Add a **Setting and data** chapter (from `80-colombia`, `90-colombia-data`).
- **Related literature stays last** (chapter 9); move forward later if needed.
- **Heterogeneity / size (`961-evasion-het`) dropped** for now.
- Counterfactual chapter **includes the stage-2 (ELVIS) estimation**; ELVIS/MSL derivations go to appendices.
- Model chapter is built **from the slides** (`Quarto-Slides/sections/200-model.qmd`, `600-opt-tax.qmd`), not the older Paper model.
- Not every existing qmd gets reused. Anything not mapped below is dropped by default.
- `Paper/sections/56-id-evasion.qmd` is supervisor-approved: **copy, never edit in place**.

## 2. Structure

```
Thesis/
  PLAN.md                 # this file
  _quarto.yml  index.qmd  # book config, abstract
  chapters/   01-intro  02-model  03-setting-data  04-testing  05-deconvolution
              06-pf-productivity  07-fiscal-policy  08-counterfactual  09-literature
  appendices/ A-*.qmd
  figures/  tables/       # generated PNGs only
  biblio/references.bib   # single merged file
Code/Thesis/              # one script per figure/table + 000-build-all.R
```

## 3. Chapter map and status

| # | Chapter | Sources (copy from) | Status |
|---|---|---|---|
| 1 | Introduction | `Paper/sections/010-intro`, abstract in `Paper/Tax-Prod.qmd`, `001-TODO` | `[ ]` mostly new writing; abstract stale (no counterfactual) |
| 2 | Model | Slides `200-model`, `600-opt-tax` (model/FOC parts); `Paper/sections/9999-tax-wedge` | `[ ]` |
| 3 | Setting and data | `80-colombia`, `90-colombia-data`; Colombian tax-system notes from `30-lit-rev` | `[~]` outline written (`chapters/03-setting-data.qmd`); all 4 tables ported + rendered |
| 4 | Testing for evasion | `200-deconv` §Testing (`#sec-tax-ev-test`), `11-the-story`, `56-id-evasion` "Testing for Tax Evasion" | `[ ]` |
| 5 | Deconvolving tax evasion | `56-id-evasion` (Identifying Tax Evasion, non-parametric), `120-implementation`, `200-deconv` (moments, parametric MLE), `930-eps-density`; Slides `700-deconvolving-evasion` | `[ ]` split `200-deconv` between ch. 4 and 5 |
| 6 | PF parameters and productivity | `56-id-evasion` (PF, productivity, two flexible inputs, translog), `250-pf` | `[~]` table+figure built (joint efficient-GMM, both instruments, test-inversion; supersedes `250-pf.qmd`'s 4-column table); prose still TODO |
| 7 | Fiscal policy evaluation | Slides `300-fiscal-reform` (most up to date); Paper `965-DiD`, `966-DiD-txrt`, `98-fiscal-ref-col`, `961-over-time` | `[~]` outline file `chapters/07-fiscal-policy.qmd` written (from slides); Hans merging Paper sections + new findings |
| 8 | Counterfactual question | Slides `630-counterfactual-question`, `600-opt-tax`, `620-ELVIS`, `650-stage2-prelim-results`, `660-cv-diagnostic` | `[~]` outline file `chapters/08-counterfactual.qmd` written (main points + key values); Hans writing prose |
| 9 | Related literature | `30-related_lit` (skeleton), `30-lit-rev` (notes) | `[ ]` |
| A-C | Appendices (stubs `A-elvis`, `B-control-variate`, `C-msl` created) | `950-384-369`, `980-non-random-q`, `910-GNR-rep`, `920-gnr-inter`, `9999-elvis`, `9999-control-variate`, `9999-msl-implementation`, `9999-density-trans` | `[ ]` |

### Unassigned / dropped by default (confirm or rescue)
`961-evasion-het` (dropped), `960-who-evades`, `970-david-tbl`, `110-revenue`, `125-prices`, `150-intermediates`,
`210-exp-swapping`, `300-misallocation`, `35-setting`, `55-id-strat`, `65-prelim`, `91-colombia-empiric`,
`92-col-corp-tech`, `95-colombia-empiric2`, `45.1-model-old`, `046-revised-model`, all `.Rmd` legacy files, `00/01/000-notes`.
Slides not used: `400-tax-evasion-het`, `500-geo`, `500-opt-tax-claude`, `550-tau`.

## 4. Figures and tables convention

- qmd files contain **no computation and no `library()`**: only `![caption](figures/chNN-slug.png){#fig-slug}` or a `::: {#tbl-slug}` wrapper around the table PNG.
- Every asset has **exactly one script**: `Code/Thesis/chNN-<slug>.R`, reading `Code/Products/*.RData`, writing to `Thesis/figures/` or `Thesis/tables/`.
- Naming: `chNN-slug.png` (chapter prefix = where it is used; slug = cross-ref label).
- Shared style: one `theme_thesis()` (ggplot) and one tinytable style, sourced by every script. Tables go tinytable -> pdflatex -> PNG (as in `050-render-tbls.R`), escape `\%`.
- `Code/Thesis/000-build-all.R` regenerates everything; a manifest (chapter -> assets) catches orphans and missing files.
- Known cost: old DiD figures (`915-*`, `917-*`, `921-*`, `922-*`) need their producing code located or ported.

Asset inventory (fill as chapters are ported):

| Asset | Chapter | Script | Status |
|---|---|---|---|
| `figures/ch07-fiscal-all-unincorp.png` | 7 | src `921.2-het-slides.R` -> `921-2-all-inds.png` | `[~]` copied, script pending |
| `figures/ch07-fiscal-liable-vs-exempt.png` | 7 | `921-2-joint.png` | `[~]` copied |
| `figures/ch07-fiscal-llc.png` / `ch07-fiscal-prt.png` | 7 | `921-2-joint-3.png` / `921-2-joint-2.png` | `[~]` copied |
| `figures/ch08-elvis-cube.png` | 8 | `Code/Thesis/ch08-elvis-cube-plot.R` | `[x]` done 2026-09-22 |
| `figures/ch08-lambda-grid.png` | 8 | `Code/Thesis/ch08-lambda-grid-plot.R` | `[x]` done 2026-09-22 |
| `figures/ch08-delta-grid.png` | 8 | `Code/Thesis/ch08-delta-grid-plot.R` | `[x]` done 2026-09-22 |
| `figures/ch08-trim-cutoff.png` | 8 | `Code/Thesis/ch08-trim-cutoff-plot.R` | `[x]` done 2026-09-22 |
| `figures/ch08-laffer-ci-hard.png` | 8 | `Code/Thesis/ch08-laffer-ci-hard-plot.R` | `[x]` done 2026-09-22 |
| `figures/ch08-theory-vs-cv.png` | 8 | `Code/Thesis/ch08-theory-vs-cv-plot.R` | `[x]` done 2026-09-22 (`ch08-theory-raw.png` from `1308-theory-raw-plot.R` still not called, out of scope for this pass) |
| `tables/ch08-headline-estimates.png` | 8 | `Code/Thesis/ch08-headline-estimates-table.R` | `[x]` done 2026-09-22 |
| `tables/ch08-detection-prob.png` | 8 | `Code/Thesis/ch08-detection-prob-table.R` | `[x]` done 2026-09-22 |
| `tables/ch08-headline-pct.png` | 8 | `Code/Thesis/ch08-headline-pct-table.R` | `[x]` done 2026-09-22 |
| `tables/ch08-laffer-ci.png` | 8 | `Code/Thesis/ch08-laffer-ci-table.R` | `[x]` done 2026-09-22 |
| `tables/ch06-pf-comparison.png` | 6 | `Code/Thesis/ch06-pf-comparison.R` | `[x]` joint efficient GMM (m\*\_{it-1}+W~\_{it-2}, beta fixed) vs. GNR/OLS; industry 313's chi2_3 region is empty, cell reports chi2_5 with `*` (Research-log 2026-09-21) |
| `figures/ch06-pf-testinv-regions.png` | 6 | `Code/Thesis/ch06-pf-testinv-regions.R` | `[x]` companion to the table: both tests, all 5 industries |

| `tables/ch03-summary-stats.png` | 3 | `ch03-summary-stats-table.R` | `[x]` done -- rebuilt from scratch, no `modelsummary` (see §5) |
| `tables/ch03-jo-summary.png` | 3 | `ch03-summary-stats-table.R` (same script, 2nd table) | `[x]` done |
| `tables/ch03-top-industries.png` | 3 | `ch03-top-industries-table.R` | `[x]` done (kbl -> tt()) |
| `tables/ch03-corps-by-industry.png` | 3 | `ch03-corps-by-industry-table.R` | `[x]` done (kbl -> tt()) |
| `tables/ch06-pf-comparison.png` | 6 | `ch06-pf-comparison.R` | `[x]` done -- Hans's own script, aligned to shared `001-setup.R` helpers |
| `figures/ch06-pf-testinv-regions.png` | 6 | `ch06-pf-testinv-regions.R` | `[x]` done -- same alignment, now also writes PDF (previously PNG-only) |

Next step for remaining `[~]` rows (ch. 7, 8): move each producing script to `Code/Thesis/chNN-<slug>.R` writing straight into `Thesis/figures|tables`.

## 5. Bibliography

- [x] Merged 23 files into `Thesis/biblio/references.bib`: 104 unique keys, existing keys kept. Where a key appeared in several files, the longest variant was kept; `@workingpaper`/`@working_paper` normalized to `@techreport` (Carrillo2022 had two variants, one dropped).
- [ ] Review possible duplicates under different keys: `BUEHN2016`/`Buehn2016`, `Hu2021`/`Hu2022`, `Nola2018`/`Paulus2015` (same title or DOI).
- [ ] Review keys whose content differed across files: `McLure1989`, `Kang2021`, `Asatryan2016` (also has a `@workingpaper` variant), `Hu2022`, `Paulus2015`.
- [ ] Check every cited `@key` resolves; list unused entries.
- Style: currently `agsm.bst`; no requirement, keep for now.
- `modelsummary` is listed in `renv.lock` but NOT actually installed in the renv library (`renv::status()` flags the project out of sync) -- blocked `ch03-summary-stats-table.R` until rebuilt without it (see §3 log). Not fixed at the renv level; only worked around for this one table. Revisit if another chapter's table wants `datasummary_skim`-style output.

## 6. Schedule

| Day | Target | Status |
|---|---|---|
| Mon 09-21 | Plan; scaffold book; bib merge; book renders (HTML checked, PDF not yet); ch. 7 and 8 outline files | `[x]` PDF render still to check |
| Tue 09-22 | Ch. 2-4 (model, setting/data, testing) incl. figure/table scripts | `[ ]` |
| Wed 09-23 | Ch. 5-6 (deconvolution, PF/productivity) | `[ ]` |
| Thu 09-24 | Ch. 7-8 (fiscal policy, counterfactual) | `[ ]` |
| Fri 09-25 | Intro, literature, appendices; full render; fix cross-refs and missing assets | `[ ]` |

## 7. Open questions

- Which of `965-DiD` / `966-DiD-txrt` / `98-fiscal-ref-col` / `961-over-time` stay in ch. 7?
- How is `200-deconv` split between ch. 4 (testing) and ch. 5 (deconvolution)?
- Stage-1 net-of-tax re-estimation (`log_mats_share_net`) is pending a supervisor talk: does the draft describe stage 1 as approved, with the two-tax-rate extension as future work?
- **2026-09-22, must not get lost before submitting anywhere:** the ELVIS/counterfactual results (ch. 8) inherit $\hat\beta$ from the single-tax first stage -- state this explicitly to supervisors as a limitation, don't let it read as already-resolved. See the flagged bullet in `chapters/08-counterfactual.qmd`'s Scope and limits section. ELVIS is NOT being re-run this week; that's future work.
- Abstract needs rewriting to include the counterfactual and stage-2 results.

## 7a. DEFERRED — post-deadline: M/M* notation flip

**Decided 2026-09-22, explicitly postponed until after the Friday draft deadline — do not start before then.** Every other lit. convention stars the *unobserved* variable; this project currently has it backwards ($M$ = true/unobserved, $M^*$ = observed, e.g. $M^*=M+e$). Flip to: $M^*$ = true/unobserved materials, $M$ = observed. Scope agreed so far (session cut short before full sign-off — re-confirm when resumed):
- **In scope:** `Quarto-Slides/` (all .qmd, essentially every section touches this) and `Thesis/chapters/` + `Thesis/appendices/`.
- **Out of scope, explicit:** `Paper/` folder — leave entirely as-is.
- **Still open, ask again before starting:** whether `CLAUDE.md` also gets updated to match (it documents the old convention throughout and is otherwise "current state only" — leaving it stale would contradict the slides/thesis it's meant to describe); whether code/data/column names (`lag_m` instrument codename, R/C++/Stata variables, CSV headers, `.RData` objects) stay untouched (this was going to be confirmed as "yes, prose-only change" but never got a final yes/no).
- **Mechanics, worked out, still valid when resumed:** this is a symmetric token swap ($M\leftrightarrow M^*$, and correspondingly $m\leftrightarrow m^*$ for logs) applied file-by-file, not a blind global regex — every derived quantity flips consistently as a side effect (e.g. $u=\ln(M^*/M)\to\ln(M/M^*)$, $e=M^*-M\to M-M^*$, ELVIS support bounds, the `lag_m` instrument's math symbol $m^*_{it-1}\to m_{it-1}$), but needs care around bare "M"/"m" that isn't this variable (matrices, MSL/MSM abbreviations, unrelated words). Large surface area (10+ slide files with interdependent derivations) — budget real time, re-render both the slide deck and the book afterward to confirm nothing broke.

## 7b. DEFERRED — post-deadline: switch to the two-tax model

**Decided 2026-09-23 (Hans): the Friday draft tells the single-tax story everywhere, ch. 8 included**, because every current estimate (stage 1, PF step, ELVIS $\hat\theta$, counterfactual) uses the single-tax first stage. Text has to match the numbers. Don't re-propose two-tax edits before Friday.
- **Now:** ch. 8's two-tax material (two-tax revenue equation, two-tax profit function, net-of-tax limitation bullets) is kept but gated `::: {.content-visible when-meta="draft"}`. Visible text uses $\tau$, not $\tau_P$.
- **After Friday (the better version):** two-tax model; first stage re-estimated net-of-tax (`log_mats_share_net`); PF step as joint efficient GMM with both instruments ($m^*_{it-1}$, $\tilde{\mathcal W}_{it-2}$); re-run ELVIS and the counterfactual on it; switch the document's story to two-tax and un-gate the ch. 8 blocks.

## 9. JMP (`JMP/`) — the Friday deliverable

**One prose source, two documents.** `JMP/paper.qmd` is a standalone Quarto `default`-type project that `{{< include >}}`s the exact same `Thesis/chapters/*.qmd` and `Thesis/appendices/*.qmd` files this book uses -- no forked/duplicate chapter text. `JMP/figures`, `JMP/tables`, `JMP/biblio` are symlinks to the `Thesis/` versions, so `Code/Thesis/*.R` output shows up in both renders automatically.

- **Root-relative image paths, required for this to work:** every `![...](...)` in `Thesis/chapters/*.qmd` and `appendices/*.qmd` uses `/figures/...` / `/tables/...` (project-root-relative), not `../figures/...`. Quarto resolves `{{< include >}}`d relative paths against the *including* document, not the included file, so a `../`-relative path breaks the moment the same chapter is included from a project at a different depth (verified: the book resolves `/figures/x.png` to `../figures/x.png` in `_book/chapters/`; the JMP resolves the identical source to `./figures/x.png` from `JMP/`). Keep using root-relative paths in any new chapter content.
- **Trimming content out of the JMP only, without forking the file:** wrap the passage in the shared chapter file with `::: {.content-hidden when-meta="jmp"} ... :::`. `JMP/paper.qmd` sets `jmp: true` in its YAML; `Thesis/_quarto.yml` does not set `jmp`, so the same text still renders in the book. Reverse (JMP-only tighter restatement) uses `.content-visible when-meta="jmp"`.
- **Format, standard econ-JMP look, not the thesis's `scrreport`:** plain `article`, 12pt, 1in margins, `mathptmx` (Times), `linestretch: 1.15`, `toc: false`, no separate title page -- title/author/abstract/JEL/keywords all land on page 1, straight into "1 Introduction" beneath, verified in the first real render (25pp with all chapters still at outline/stub length). JEL codes + keywords block injected via `include-before-body` (plain quarto `keywords:`/custom `jel:` YAML fields aren't typeset by the default pdf template on their own). Appendices via `\appendix` + an unnumbered "Online Appendix" heading -- letters (A/B/C) confirmed correct in the rendered PDF.
- **Abstract and Introduction are the two deliberate exceptions to "no duplication," both by design (2026-09-22):** `Thesis/index.qmd`/`abstract:` YAML field, as before -- structurally different Quarto mechanisms, can't share one source. **Introduction:** `JMP/sections/01-intro.qmd` (local to `JMP/`, NOT `Thesis/chapters/01-intro.qmd`) -- a JMP intro and a thesis intro are different animals (space-constrained hook vs. thesis framing), confirmed explicitly in chat after the first scaffold accidentally shared them. Source: `Paper/sections/010-intro.qmd`'s visible prose (the `.content-hidden unless-meta="draft"` blocks in that file are NOT copied -- see below), copied over and mechanically proofread (spelling/spacing/punctuation only, no rewording). Both still carry the same stale placeholders (`XXXX`, `[what factor]`, etc.) -- fill with real numbers before this goes anywhere.
- **`Paper/sections/010-intro.qmd` has Tim Conley's feedback hidden inline** (`.content-hidden unless-meta="draft"` blocks: a per-paragraph purpose label on most paragraphs, one specific inline note on the revenue-losses paragraph, and a dated "Notes, Meeting with Tim Sep 2025" block at the end covering paragraph ordering and a possible structural call -- "focus on tax evasion, leave identification in the Appendix"). **Not incorporated into `JMP/sections/01-intro.qmd` -- deliberately left for Hans to decide**, per his own instruction (2026-09-22): suggest in bullets what incorporating it would look like, don't write it into the prose. Given as chat bullets, not filed anywhere in `Thesis/`/`JMP/` yet -- if it doesn't get acted on before this file is next touched, re-surface it rather than assume it's resolved.
- **New shared chapter added:** `Thesis/chapters/10-conclusion.qmd` (stub) -- the thesis chapter list had no conclusion; both documents need one. Added to `Thesis/_quarto.yml`'s chapter list and to `JMP/paper.qmd`'s include sequence, same position (end of body, before appendices).
- **Not yet decided:** whether "Related Literature" (ch. 9) should move earlier for the JMP (many JMPs fold it into/near the intro rather than placing it last, which is more of a thesis convention) -- currently both documents use the same order (…counterfactual → literature → conclusion → appendix). Revisit once ch. 9 has real content; reordering the JMP's own include sequence in `paper.qmd` doesn't require touching the shared chapter files.
- **Still placeholder in `JMP/paper.qmd`'s YAML, fill in before submitting anywhere:** `thanks:` (seminar/committee acknowledgements), JEL codes (guessed at H26/D22/L60/O47, confirm), and the abstract itself.
- Build: `cd JMP && quarto render paper.qmd --to pdf` -> `JMP/paper.pdf` (gitignored, like `Thesis/_book/`, along with `paper.tex`/`.quarto/` -- `keep-tex: true` is on for inspecting the compiled LaTeX when debugging layout, not for tracking).

## 8. Log

- 2026-09-21: plan agreed; decisions above; tracker created.
- 2026-09-21: scaffold created (`_quarto.yml`, `index.qmd`, 9 chapter files, 3 appendix stubs); bib merged; ch. 7 and 8 written as outlines with key values and live figure/table calls; HTML render clean, all cross-refs resolve. Stubs carry the labels other chapters cite (`sec-model`, `sec-setting`, `sec-deconvolution`, `sec-pf`, `sec-app-*`).
- 2026-09-22: `Code/Thesis/` scaffolded (`001-setup.R`, `000-build-all.R`, `manifest.csv`). Ch. 3 (setting and data) built end to end: outline written, all 4 tables ported and rendering clean. Found Hans's own `ch06-pf-comparison.R`/`ch06-pf-testinv-regions.R` already in `Code/Thesis/` (new joint-GMM PF results, industries 331/322/369/313/321) and aligned them to the shared setup. Standardized on tinytable+etable (tables) / ggplot2+base-persp (figures) per chat. Rebuilt `ch03-summary-stats-table.R` from scratch without `modelsummary` (not installed); split its one table into two (numeric skim + J.Org. composition) per Hans's call in chat. Two real bugs caught and fixed in that process: unescaped `%`/`&` in tinytable cells/labels silently corrupts the render, and `style_tt(i=...)` after `group_tt()` needs post-insertion row indices -- both now documented in §1 as standing conventions. Full book HTML render clean throughout.
- 2026-09-22: full PDF render checked for the first time (`Thesis/_book/Tax-Evasion-and-Productivity.pdf`, 38pp) -- found and fixed the actual cause of inconsistent table sizing (untagged PNG resolution -> pdflatex assumes 72dpi -> Quarto's `\pandocbounded` clamps everything to full page width regardless of content) by adding `-units PixelsPerInch` to `050-render-tbls.R`'s `magick` call. Added deliberate size control on top: `render_png_tt_tbl()`/`render_png_etbl()` now take `linewidth_pt`, pinning each table's `\linewidth` to the book's real text width (446.76pt/6.18in) so `tt(width=<fraction>)` is meaningful; project default `width=1` for every table. Caught the equal-column-split side effect (a long-text column wrapping 7 lines next to short numeric ones) and fixed with `width=`'s per-column-vector form. Dropped `caption=` from every `tt()` call per Hans's call (Quarto's own `![cap](path){#tbl-x}` is now the single caption source; real notes moved to `notes=`), and moved captions into the ch. 3 qmd accordingly. Also caught a 3rd/4th unescaped-`%` instance and a new bug (literal `_` in a raw column name reads as a LaTeX subscript trigger and garbles) while fixing ch03-top-industries/corps-by-industry. All fixes documented as standing conventions in §1. Full PDF re-render clean.
- 2026-09-22: M/M* notation-flip request raised (project convention has the star backwards vs. the literature -- want $M^*$=true/unobserved, $M$=observed). Scoped (Quarto-Slides/ + Thesis/ in, Paper/ out, CLAUDE.md and code/data names still open) then explicitly deferred to after the Friday deadline -- see §7a. No files touched for this item.
- 2026-09-22: **priority flip -- Fri 2026-09-25 is the JMP deadline, not the thesis** (thesis submits next summer). Scaffolded `JMP/` as described in §9: standalone `default`-type Quarto project, `paper.qmd` includes the same `Thesis/chapters/*.qmd`/`appendices/*.qmd` files (no fork), `figures`/`tables`/`biblio` symlinked to `Thesis/`. Rewrote every chapter's image path from `../figures|tables/` to `/figures|tables/` (root-relative) so the same file resolves correctly from both projects -- verified with a minimal two-project mock before touching real files, then confirmed on the real book (`quarto render --to html`, clean, images present in `_book/`) and the real JMP (`quarto render paper.qmd --to pdf`, clean, 25pp at current stub length). Added `Thesis/chapters/10-conclusion.qmd` (new stub, neither document had one) to both `_quarto.yml` and `paper.qmd`. First real PDF confirms the target layout: no title page, title/author/abstract/JEL/keywords/Introduction all flow from page 1, appendix letters (A/B/C) correct. Still open: abstract rewrite (shared stale TODO text), `thanks`/JEL placeholders in `JMP/paper.qmd` YAML, and whether ch. 9 (literature) should move earlier in the JMP's own include order.
- 2026-09-22 (later): **ch. 8's remaining `[~]` tables and figures moved to `[x]` done** -- all 3 tables and 6 figures now have real `Code/Thesis/ch08-*.R` scripts. Hit the magick/pandoc PNG-sizing bug again (this time in `save_thesis_base_plot()` itself, previously undiscovered) plus a related-but-different device-canvas-vs-native-size layout bug -- **both now written up as a standing, consolidated reference in §1** ("PNG asset sizing in the Quarto/LaTeX pipeline") rather than left as a one-off fix buried in this log entry; check there first if a rendered asset ever looks the wrong size again. Also fixed a small cosmetic glyph bug (a base-R title with a literal `"β̂"` string mis-rendered; switched to `bquote(hat(beta))` plotmath). Full book and JMP PDF re-renders both clean, no broken glyphs anywhere in either document.
