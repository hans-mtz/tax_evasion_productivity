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
- **Notation (2026-09-23):** (i) densities are always $f_X$ for the density of variable $X$ (conditional $f_{X|Z}$; a reference/dominating density $f^0_{X|Z}$), never $F_X$, $\mu$, $\rho$ or $\pi$ copied from another paper's notation. Ch. 8's ELVIS passage was converted from Schennach's $\mu$, $\rho(M|Z)$, and the data distribution. (ii) Population means/expectations are Greek coefficients with hats for estimates ($\mu_t\equiv E[u\mid D^N=1,t]$, estimate $\hat\mu_t$), never a bar, which reads as a sample mean.
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
| 2 | Model | Slides `200-model`, `600-opt-tax` (model/FOC parts); `Paper/sections/9999-tax-wedge` | `[x]` done 2026-10-07 (prose, proof-read; appendix D aligned) |
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

JMP to supervisors **Tue 2026-10-13** (set 2026-10-06; the Sept 21–25 schedule is in git history).

| Day | Target | Status |
|---|---|---|
| Tue 10-06 – Wed 10-07 | Model chapter (`02-model.qmd`): Setup → Corporations from bullets to prose (draft-prose, two proof-reads, Hans approves); decide the δ_C and old SOC draft blocks | `[x]` done 2026-10-07 |
| Wed 10-07 – Thu 10-08 | Ch. 8 text on the M-responding headline: claims arcs (1655), loss shares (1653), revenue figure/table and break-even (1651/1652), mean q (1657); replace "input decisions fixed" passages | `[ ]` |
| Fri 10-09 | Numbers propagated: JMP abstract (full pass + "median overreporting", "after the raise"), intro para. 3, conclusion (1.9 → 1.5 percent), thesis abstract | `[ ]` |
| Sat 10-10 – Sun 10-11 | Conclusion prose (Hans); `thanks:`, JEL codes | `[ ]` |
| Mon 10-12 | Full JMP render, leak check, cross-refs, layout | `[ ]` |
| Tue 10-13 | Send to supervisors | `[ ]` |

Post-deadline (not before Oct 13): κ sensitivity, D robustness, 1% level grid, M/M* flip (§7a), net-share denominator (§9a).
Post-deadline, ch. 8 counterfactual (2026-10-07): claims levels with true materials responding (replace the 1622 claims figure/table, now draft-only); claims elasticities with true materials fixed (robustness); entry of corner firms in the 19 non-rejecting industries via design `allcorr` (stage-2 re-estimation; the increase-side response is a lower bound without it); share of firms that stop overreporting on a cut (new auxiliary target, engine review sequence).
Ch. 8 rewrite tracker: the TRACKER comment below the outline in `Thesis/chapters/08-counterfactual.qmd`.

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

## 9a. After the supervisor meeting (2026-09-25) — NEW DEADLINE Oct 13

Supervisors need one week for letters (due to the department Oct 20), so the JMP goes to them by **Tue 2026-10-13**. ELVIS approved for the counterfactual; $\hat\lambda$ not credible (scale problem). Plan, in order:
- [x] (3) External evidence (2026-09-25): Ecuador, Mexico, IRS, Spain, Perry and Cárdenas; anchors only, not a calibration (log).
- [x] Detection function chosen (2026-09-26): $q=\lambda_1(1-e^{-e/\bar M_{j,t-1}})$, lagged industry mean, level estimated, support $e<\bar M$; power form as alternative (`Paper/sections/9999-detection-q.qmd`).
- [~] Re-run ELVIS + counterfactual on the final specification — plan and tracker in §9b below.
- [x] **Sample definition (decided 2026-09-26, option b; APPLIED 2026-09-26 to the test `1510`/`1514`, first stage `1501`, PF `1502`/`1511`–`1517`, stage-2 data `1503`, deconvolution `1521`, GNR/OLS `1520`, and ch. 7 via `1530-fiscal-did.R` on the net share, replacing `921.1-DD.R`; ch. 3 tables via `Code/Thesis/ch03-sample.R`, which also drops 353):** every step codes corporations as `juridical_organization == 3` and "unincorporated" as everything else, which wrongly includes code 6 (stock partnerships, *comandita por acciones*, taxed as corporations; 1.1% of the working sample) and codes 7–9 ("Others": cooperatives, state enterprises; 1.3%). Drop codes 6–9 from the unincorporated group in the re-run: first stage (`1501`), PF (`1502`), stage-2 data (`1503`), and the ch. 4/5/7 inputs (`021-deconv-funs.R`, `921.1-DD.R`). Unincorporated = LLCs (1) + proprietorships (0) + partnerships (2, 4, 5). Ch. 3 text says codes 6–9 are excluded.
- [ ] Update ch. 8, intro, abstract, conclusion with the new numbers.
- [x] First stages re-checked under the two-tax model (2026-09-26, details in the research log): test by inversion (ch. 4, sharp; appendix F conservative + fixed bootstrap), PF headline $\tilde{\mathcal W}_{it-2}$ with points at the minimum of the region statistic (ch. 6, seven industries; appendix E, 28), $\beta$ regions, GNR/OLS via the R port, deconvolution of the seven industries selected ex ante (ch. 5). Text updated in ch. 4–6, appendices, intros, abstracts, conclusion outline; ch. 6 region figure dropped.
- [x] Productivity rerun on the $\tilde{\mathcal W}_{it-2}$ estimates (2026-09-26): 90--10 about 40% lower (holds), half-life 1.6 to 3.1 times longer; table W~ vs GNR only.
- [x] Ch. 7 DiD with codes 6--9 dropped and the net-of-tax share (2026-09-27, `1530`; wedge in appendix G).
- [x] Chs. 2, 4, 5 on the two-tax model (2026-09-28): ch. 2 presents the two-tax model (single-tax model in appendix D, @sec-app-single-tax); ch. 4 defines $s_{it}$ as the net share; ch. 5 uses $\mathcal V$ from ch. 4.
- [ ] DEFERRED post-deadline (Hans, 2026-09-28): net-share denominator $(1-\tau_S)GO$ instead of $GO-t_1$ (code `1501`). Checked 2026-09-28 without re-estimating: $\Delta\ln\hat\beta$ 0.005--0.013 (tobacco 0.05), test $\mu$ moves $<0.005$ except 313 (+0.018), 314 ($-0.036$); rerun of every first stage needed (log 2026-09-28).
- [x] Ch. 4 wording on 311/312 (Hans, 2026-09-28; done same day): exempt industries did get refunds on purchase taxes, but their $\tau_P$ is near zero (median 0.2% in 311, 0.6% in 312, vs 9.2% liable), so the credit rewarded overreporting little; reword "gained nothing from the credit" around the near-zero rate (log 2026-09-28).
- [ ] Still open: 313 benchmark (49% overreporting, flagged). 369 deconvolution level gap. PF lags that span a missing year (~1.8%).
Details: `Research-log/log.md`, 2026-09-25.

## 9b. Stage-2 re-estimation plan (agreed 2026-09-28)

Reasoning and numbers: `Research-log/log.md`, 2026-09-28 ("Stage-2 re-estimation plan"). One change per step, simplest first, each step compared with the previous one on a 1-D $\lambda$ grid ($\delta$'s free) so we know what drives what. Confirm seeding/grid/`n_burn`/`n_keep` with Hans before each launch. Keep the 0.5% trim.

**Decisions:**
- **Which firms are corrected (design A):** evasion is corrected, and firms enter the evasion FOC, only in the **9 industries where the headline test rejects** (sharp, 5%): 313, 321, 322, 324, 331, 342, 351, 352, 369. In the other 19 (inconclusive, empty region 323/356/384, untested) all firms are **corner firms** ($M=M^*$, $\varepsilon$/$\omega$ rows only), with the **uncorrected first stage** ($\beta$ pooled over all firms = GNR's first stage with $\mathcal E=1$, verified in `1520`) and the **same linear PF step** ($\tilde{\mathcal W}_{it-2}$ instrument). Keeps the counterfactual's all-industry scope; in the 19 the claims response is mechanical only (a lower bound; no extensive margin — say so in ch. 8). Why not all firms: industries with $E[\mathcal V_j]<0$ force $E[\varepsilon]>0$ there and bias the evaders' $u$ down through the pooled $\varepsilon$ row, and spill into $\omega$ and the $\delta$'s.
- **Corner definition otherwise unchanged ($\tau_P=0$).** Not adding $\tau_S=0$: non-collecting firms got refunds of tax paid on purchases (Hans) — **source needed for ch. 3**.
- **1981 $\bar M$:** leave-one-out same-year industry mean (flagged); robustness: drop 1981.
- **Price / benefit shifter:** year intercepts $\delta_{0,t}$ (no industry materials deflator exists: `materials = nom_mats/p_gdp` exactly). No industry×year intercepts (would leave $\lambda_1$ only the within-cell firm variation in $\tau_P$, 38% of the variance and the most mechanically suspect). Additive industry intercepts only if year intercepts leave a problem.
- **Appendix E:** corrected (corporations' $\beta$) and uncorrected (pooled $\beta$, same linear PF step) for all 28 industries, flag for which one stage 2 uses; GNR/OLS stay as benchmarks.
- **Ch. 5 deconvolution rule (proposed, Hans):** 1% rule restricted to the **top 5 by output share**: 313 (7.7%), 321 (7.2%), 369 (3.0%), 342 (2.3%), 322 (2.1%); drops 324 (1.1%), 331 (0.5%). Keep 313 (flagged) — dropping it after seeing it is ex-post selection. *To confirm.*

**Preparation:**
- [x] P1 `1531-fs-pf-pooled.R`: pooled (uncorrected) first stage + linear PF step ($\tilde{\mathcal W}_{it-2}$, `1517` functions) for the 19 non-selected industries (also all 28, for Appendix E).
- [x] P2 `1532-stage2-data-final.R` (`1503` untouched): `evader` flag; pooled $\beta,\mathcal V,\tilde{\mathcal W},\varepsilon,\alpha$ for the 19; 1981 $\bar M$ leave-one-out + flag; `corner = (tau_P==0) | !evader`. Exports: design A, and "all corrected" (for S1). **Done 2026-09-28:** allcorr n=31,246 (28,123 interior); designA n=31,327 (12,050 interior, 19,277 corner of which 16,154 not corrected); 1981 LOO rows 5,689; P1 uncorrected $\beta$ above corrected in all 9 evaders (+0.015 to +0.08). Flag: 369 interior mean $\mathcal V$ 0.361 vs test $\hat\mu$ 0.193 (known 369 level gap).

**Ladder:**
- [x] S0 old input `1260`, `grid3d` at $(\lambda,\delta_0,\delta_1,\delta_2)=(5.427\times10^{-7},3.464,4.3,0.54)$ must reproduce $\hat L_n=0.000215$ (code-base check). **Done 2026-09-28: 0.000214998, exact** (`1540-s0-check.csv`).
- [x] S1 new data, all corrected, linear $q$ — isolates the data change. **Done 2026-09-28** (`1541-s1-lambdagrid-allcorr.csv`): upper bound holds ($\lambda\ge6\times10^{-6}$ rejects, TS 856–9,374); **no lower bound** (good basin at every small λ, TS 18.7–64, three points unconverged); old lower bound partly a basin artifact ($\hat\delta_1\approx40$). Expected (Hans): linear $q$ with $e$ in levels does not identify λ ex ante — the fix is S3. Log 2026-09-28 "S1". Lesson: ≤4 points per 12-thread `lambdagrid` process.
- [x] S2 design A (corner flag only, no C++ change). **Done 2026-09-28** (3 λ's per design, common seed; `1542-s2-*.csv`): all-corrected flat in λ ($TS\approx3.5$, identical $\hat\delta$'s at all 3 λ's); design A flat in λ too ($TS$ 1.1–8.0) and the $\hat\delta$'s drift along a ridge with $\hat\omega^*\approx3.4$–3.8 (one point wrong-signed). Log 2026-09-28 "S1".
- [~] S3 new $q$ (C++: new $h$, fixed support $e<\bar M_{j,t-1}$, $\lambda_1$ score row); $\lambda_1$ grid. **Code done and checked 2026-09-28, not run:** `qform=exp_scale` CLI switch (default `linear`), header `h_of_e_exp_scale`/`h_prime_bounded_exp_scale`/`draw_from_rho_fixed_scale`, `firm_chain_A` branch; `Mbar` read from the input CSV. Checks: `qform=linear` gives estimates identical to 15 digits to the old binary; $B=1-d(qe)/de$ to 4e-11; score vs numerical derivative 6e-10; exp_scale smoke run finite. Built as `Code/C-estimator/grid_estimator_s3` (the running S1 uses the old binary). The linear optimum may be a poor seed under the new $q$ (smoke run hit the $\delta$ box with tiny chains): plan a center refit before the $\lambda_1$ grid.
- [x] **BLOCKER resolved 2026-09-28:** the eigen-cut kept 2–3 of 9–10 directions (row $\varepsilon\cdot e$ in pesos); fixed by row 6 = $\varepsilon\cdot\psi$ (`row6=eps_psi`), all directions kept. Old ch. 8 $\hat\theta$/CIs/counterfactual are artifacts. Log 2026-09-28.
- [~] S4 year intercepts: built (`YEAR_FE`, `grid_estimator_yfe`), checked; with full rank they barely help (TS 1,694–2,043 vs 1,825–2,118); NM in 33 dims does not reach the minimum (year rows still violated); simulated annealing (`sa_time=`) gave little. Parked until detection is settled.
- [~] **Detection, full rank (2026-09-28/29):** smooth forms all rejected (linear in levels 1,531–1,597; exp scaled 2,181; power 2,435–2,862). **Kinked power** (`KINK`, `qform=power_kink`: $q=(e/(\kappa\bar M))^k$ to the FOC ceiling, beyond it only $\varepsilon$ rows, share $s$ fixed): 3×3 best $\kappa$=0.5, $s$=0.2, $\hat k$≈0.95, TS 726 (crit 19.7); running overnight: `run-1559-kink-refine.sh` then `run-1560-kink-dense.sh` ($k\le2$, starts 0.2/0.9/1.3). **2026-09-29:** $k$ fixed at 0.3 and 1, $s$ estimated, score row for $\kappa$ (`run-1563-kinks-grid.sh`, 10 fits). **1563 done** (best $k$=0.3, κ=0.5, $\hat s$=0.219, TS 588.8; κ-score row ≈ duplicate of the $k$-score row). **Running:** fine κ grid at $k$=0.3 (`run-1565-kappa-fine.sh`), then fine $s$ grid at the profile-minimum κ (`s_fixed`). **Then (Hans): drop moments one at a time and see what breaks** (Nail's build-up, run backwards), starting with rows that carry no information or that we could forgo; keep $\varepsilon$ and $\varepsilon\ln M$. Earlier note — Next session:** rank all fits (watch $\hat k$ at bounds; optimizer noise ~±100 TS), soft test as best-fit region + conservative test, then decide year FE and move to the counterfactual (fix the units bug first).
- [ ] $\delta$ grid → cube → fix $\hat\theta$ (target Oct 2–3).
- [ ] Counterfactual, $e'(\Delta)$ by bisection; report share of firms past $x=1$ (target Oct 4–6). **Fix the units bug first** (found 2026-09-28, log): $M^*$ is real but `t1` nominal, and the C++ deflates the whole of `R` by `pgdp`, so Claims was deflated twice; use `R_real = t1/pgdp - tau*(M+(1-q)e')`. Old ch. 8 revenue/Claims levels are affected; $\hat\theta$ is not.
- [ ] Robustness: drop 323/356/384; drop all 19; drop 1981; 7 industries; industry intercepts (only if needed).

**RESTART (Hans, 2026-09-30).** The objective's eigen-cut ($w_k>10^{-8}\max$) was a porting error; AK2020 keep $\Lambda>0$, Schennach uses an exact inverse. Fixed as `cut=ak` (use it for every run from now on). Everything estimated under the old cut is discarded, including the choice of $q$ form, scaling and kink. Walk backwards from today's system (AK cut, EPSVAR 13 rows, row 6 dropped, kinked power $q$, design A): best point → drop fixes one at a time, keep a drop only if fit improves; if nothing improves, add $\omega$ targets. Log: `Research-log/log.md`, 2026-09-29/30.
- [x] **Noise at three points** (`1578`): MC noise at fixed parameters 1–3% of $TS$; refit noise 700–2,500.
- [x] **$k$ grid, κ estimated** (`1579`, `grid_estimator_kf`): $TS$ 4,653–5,240 over $k\in[0.25,2]$, inside refit noise; rows 1 and 12 dominate everywhere.
- [~] **B. Optimizer vs noise** (`run-1580-optimizer-noise.sh`, Mac mini): 3 NM / NM+BOBYQA (`algo2`) / NM+SA+NM at $k$=0.5 g0, seeds 29–31, common-seed re-evaluation (seeds 40, 41, n_keep 10,000).
- [~] **A. Chain length vs noise** (`run-1581-chain-noise.sh`, MacBook, screen `chain1581`, files `*-macbook`): n_keep 1,000 / 10,000, same point and seeds.
- [ ] Pick optimizer + chain length → rerun $k$ grid if needed → best point → drop fixes one at a time → $\omega$ targets if nothing improves.

**STATUS 2026-10-02 (supersedes the open items above; details in `Research-log/log.md`, 2026-10-01/02).**

Settled estimator:
- **Sampling and measure:** IS with the mixture proposal; ρ = Proposition 2.1 with a fixed D; bounded indicator rows left out of ρ.
- **Objective:** plant-clustered Ω; correlation-scaled CUE with a null-direction floor penalty.
- **Optimizer:** γ warm start (`gamma_init=solve`), then joint NM with 2 passes; seed 30.
- **Detection:** `qform=power_nokink`.
- **Lead design:** design i (ε by industry, rows 0, 2–4, 6, 8, 9, 11 + 13–21, interior firms).
- **Robustness design:** design iib (deconvolution medians).

Results and next steps:
- [x] **Coarse k grid** (design i): flat over 0.5–0.8, rising at 0.3/0.4 and 1.0. ω* = δ1/2δ2 is tightly identified (3.4–3.55), and the δ's are stable. κ is a flat direction (it mostly sets the detection level; relative risk is nearly κ-free).
- [x] **k × κ grid** (κ pinned, 1616): best point **k = 0.75, κ = 0.5, TS 23.7 < χ²₁₇ = 27.6** (first pass; 4R 24.7, ESS p10 2.7). Runner-up k = 0.7, κ = 9, TS 29.5.
- [~] **Fine grid** around (0.75, 0.5) (1619, Mac mini): k ∈ {0.725, 0.75, 0.775} × κ ∈ {0.4, 0.5, 0.6}. Then step outward only in the direction that improves.
- [~] **iib at design i's best point** (1620, MacBook, queued behind the 1617 κ profile).
- [ ] **Fix the operating point** (one passing point is enough, Hans). Second seed if it is irregular.
- [ ] **Counterfactual:** port the counterfactual modes to the new estimator (IS, prop21, guard, power_nokink); fix the revenue units bug (`R_real = t1/pgdp − τ(M + (1−q)e')`); e'(Δ) by bisection.
- [ ] **Ch. 8 text** with the new numbers; report detection risk in relative terms.
- [ ] **Only if no point passes:** share-of-overreporters rows (IND5P build `grid_estimator_ind5p`, inputs `1618-*`, ready and tested). κ_j by industry is **dropped** (Hans, 2026-10-02): the level of κ is weakly identified, and nine κ's would multiply the problem.
- **κ (the detection level) in the JMP (Hans, 2026-10-02):** the counterfactual uses one passing operating point with a non-stupid detection level, stated honestly as one of a range. No test of κ, no external anchor.

**POST-JMP robustness (after the Oct 13 draft; Hans, 2026-10-02):**
- [ ] **κ sensitivity analysis** (not a test): rerun the counterfactual at a more conservative and a more aggressive detection level along the κ ridge (passing or near-passing fits, e.g. κ = 9 with mean E[q] ≈ 2.7%, and κ = 0.5 with ≈ 8.5%). Report: "this is a non-stupid probability of detection; here are a conservative and an aggressive alternative; anything in between and beyond can happen." Relative detection risk is reported as κ-free.
- [ ] **D robustness at the operating point:** refit with the continuous rows' D × 0.5, × 2 and D = 1. Show that θ̂, the industry means and TS are stable. This answers "is D biasing the estimator". For the bounded rows D is an exact γ reparametrization (already shown).
- [ ] Second seed at irregular grid points (k = 0.71, 0.725, 0.73; 0.65 in another basin).
- [ ] Within-industry D for the ε rows. Auxiliary moment rows for every reported number (industry E[u], E[q], revenue).
- [ ] First-stage error in Ω, or a statement that TS is conditional on stage 1.
- [ ] Optional nested-solver re-test; PSIS k̂ diagnostic; `MIX_EDGE_EPS`.

**Remaining estimation sequence (agreed 2026-09-29; SUPERSEDED 2026-09-30 by the restart above — all under the wrong eigen-cut):**
- [~] **1. Chain length.** Best point ($k$=0.3, κ=0.556, $s$=0.20), $n_{\text{keep}}$ = 3,000 / 10,000 / 30,000, $n_{\text{burn}}$ 1,000, two NM passes, then re-evaluation at two other seeds (`run-1568-chain-length.sh`). **Stopping rule:** shortest length with seed spread < ~10 in $TS$ (below the κ/$k$ gaps we need to resolve). If even 30,000 doesn't get there, the noise isn't mainly simulation → evidence for step 2, not longer chains.
- [ ] **2. Drop moments one at a time, chain fixed.** Order fixed ex ante: (a) κ-score row 11 (duplicates the $k$-score row, corr 0.95–0.99); (b) one of $\psi\omega$ / $\varepsilon\psi$ (corr −0.99); (c) any row whose eigen-direction contributes ~nothing. Keep $\varepsilon$ and $\varepsilon\ln M$ (Hans). Compare on a fair basis: $TS$ against its own $\chi^2_{d_g}$ (p-value or ratio to critical value) plus stability of $\hat\delta$, $\hat\omega^*$ — never raw $TS$ (falls mechanically with fewer rows). `adiag` rank check after each change.
- [ ] **3. Grid over $k$, profiling κ.** A handful of $k$ in [0.2, 1] ($k$=0.3 vs 1 was ~25 apart).
- [ ] **4. Grid $\delta$'s, profiling everything else.**
- [ ] **5. Cube ($\delta_1$, $\delta_2$, $k$).**
- **Throughout:** best of two starts wherever a decision rests on a point; polish the final operating point (polishing moved $TS$ by 5–19).
- [ ] **6. Year intercepts once on the final kinked spec** (supervisors asked; barely helped with smooth forms).
- **Timeline:** steps 1–5 by ~Oct 3 (cap step 2 at about a day); counterfactual Oct 4–7 (units fix first; new $q$ needs $e'(\Delta)$ by bisection); ch. 8 Oct 8–11; send Oct 13. Hans prepares the text in parallel so results only need plugging in.

**Writing window (Oct 7–10):** ch. 8 (scope paragraph, new numbers), Appendix E corrected/uncorrected, productivity (`1523`/`1524`) for the 19, ch. 5 top-5 rule, refund source in ch. 3, intro/abstract/conclusion.

## 9. JMP (`JMP/`) — the Friday deliverable

**One prose source, two documents.** `JMP/paper.qmd` is a standalone Quarto `default`-type project that `{{< include >}}`s the exact same `Thesis/chapters/*.qmd` and `Thesis/appendices/*.qmd` files this book uses -- no forked/duplicate chapter text. `JMP/figures`, `JMP/tables`, `JMP/biblio` are symlinks to the `Thesis/` versions, so `Code/Thesis/*.R` output shows up in both renders automatically.

- **Root-relative image paths, required for this to work:** every `![...](...)` in `Thesis/chapters/*.qmd` and `appendices/*.qmd` uses `/figures/...` / `/tables/...` (project-root-relative), not `../figures/...`. Quarto resolves `{{< include >}}`d relative paths against the *including* document, not the included file, so a `../`-relative path breaks the moment the same chapter is included from a project at a different depth (verified: the book resolves `/figures/x.png` to `../figures/x.png` in `_book/chapters/`; the JMP resolves the identical source to `./figures/x.png` from `JMP/`). Keep using root-relative paths in any new chapter content.
- **Trimming content out of the JMP only, without forking the file:** wrap the passage in the shared chapter file with `::: {.content-hidden when-meta="jmp"} ... :::`. `JMP/paper.qmd` sets `jmp: true` in its YAML; `Thesis/_quarto.yml` does not set `jmp`, so the same text still renders in the book. Reverse (JMP-only tighter restatement) uses `.content-visible when-meta="jmp"`.
- **Format, standard econ-JMP look, not the thesis's `scrreport`:** plain `article`, 12pt, 1in margins, `mathptmx` (Times), `linestretch: 1.15`, `toc: false`, no separate title page -- title/author/abstract/JEL/keywords all land on page 1, straight into "1 Introduction" beneath, verified in the first real render (25pp with all chapters still at outline/stub length). JEL codes + keywords block injected via `include-before-body` (plain quarto `keywords:`/custom `jel:` YAML fields aren't typeset by the default pdf template on their own). Appendices via `\appendix` + an unnumbered "Online Appendix" heading -- letters (A/B/C) confirmed correct in the rendered PDF.
- **Abstract: separate by design** (YAML field in `JMP/paper.qmd` vs. `Thesis/index.qmd`); since 2026-09-25 the thesis abstract is the JMP abstract reordered to chapter order. **Introduction: split again since 2026-09-26** -- `Thesis/chapters/01-intro.qmd` is a placeholder copy of `JMP/sections/01-intro.qmd` (thesis wording resolved, numbered so it is Chapter 1 on page 1 under the Western template), to be rewritten around chapters rather than contributions. Edit each file separately; the `when-meta="jmp"` gates left in the JMP intro are now inert.
- **Tim Conley's intro feedback** (hidden in `Paper/sections/010-intro.qmd`: paragraph purposes, revenue-losses note, "focus on tax evasion, leave identification in the Appendix"): **resolved** (Hans, 2026-09-25) -- the RAP 2 rewrite of `JMP/sections/01-intro.qmd` addresses it.
- **New shared chapter added:** `Thesis/chapters/10-conclusion.qmd` (stub) -- the thesis chapter list had no conclusion; both documents need one. Added to `Thesis/_quarto.yml`'s chapter list and to `JMP/paper.qmd`'s include sequence, same position (end of body, before appendices).
- **Not yet decided:** whether "Related Literature" (ch. 9) should move earlier for the JMP (many JMPs fold it into/near the intro rather than placing it last, which is more of a thesis convention) -- currently both documents use the same order (…counterfactual → literature → conclusion → appendix). Revisit once ch. 9 has real content; reordering the JMP's own include sequence in `paper.qmd` doesn't require touching the shared chapter files.
- **Still placeholder in `JMP/paper.qmd`'s YAML, fill in before submitting anywhere:** `thanks:` (seminar/committee acknowledgements), JEL codes (guessed at H26/D22/L60/O47, confirm), and the abstract itself.
- Build: `cd JMP && quarto render paper.qmd --to pdf` -> `JMP/paper.pdf` (gitignored, like `Thesis/_book/`, along with `paper.tex`/`.quarto/` -- `keep-tex: true` is on for inspecting the compiled LaTeX when debugging layout, not for tracking).

## 10. JMP outline — argument-first (agreed 2026-09-24)

**Main message:** firms overreport input costs to evade taxes; this can be measured from production data alone; it responds to tax rates; and that response makes the revenue effect of rate changes asymmetric. PF parameters and productivity are secondary (a by-product of the method, and an input to the counterfactual).

| § | Section (source chapters) | What the reader must take away |
|---|---|---|
| 1 | Introduction (JMP intro; ch. 9 literature folded in) | Question, approach, findings with numbers, positioning (evasion / rates vs. enforcement / PF) |
| 2 | Setting and data (ch. 3) | Why corporations are truth-reporters; sales-tax credit = VAT in substance; 1983 reform facts |
| 3 | Model (ch. 2) | Materials FOC → log share; evasion FOC; linear $q$, convex $\kappa$. State the conditions below. |
| 4 | Identifying evasion (ch. 4 + 5, **merged in the JMP**) | Test + deconvolution without audit data; which industries, how much |
| 5 | PF parameters and productivity (ch. 6) | Corrected vs. naive elasticities and productivity; these feed §7 |
| 6 | Evasion responds to taxes (ch. 7) | Overreporting rose where net incentives rose (ST-liable; LLCs vs. proprietorships) |
| 7 | Structural estimation and counterfactual (ch. 8) | Increases raise claims immediately; cuts distinguishable only at about 8% |
| 8 | Conclusion (ch. 10) | — |
| App. | A (ELVIS), D (SOC), institutional tables | Drop C (MSL) from the JMP |

**Robustness of identification to the model's functional forms (state in §3/§4):**
- **Testing** needs only $q(0,Z)=0$ and $\kappa(0,Z)=0$: the evasion terms of expected profit then vanish identically in $(K,L,M)$ at $e=0$, so non-evaders' materials FOC is the standard one under the null, for any $q,\kappa$. Technology need not be Cobb-Douglas, only common within industry: the share residual is $\ln(\rho M^*/PY)-\ln D(K,L,M^*)=[u-(\ln D(M^*)-\ln D(M))]-\varepsilon$, which is $-\varepsilon$ under the null. Direction is always overreporting: the bracket is $>0$ because $d\ln D/d\ln M=1-D+MY_{MM}/Y_M<1$ when $D>0$, $Y_{MM}\le0$; there is no incentive to underreport deductible costs (maintained: corporations $e=0$, common technology within industry, $\varepsilon$ independent of JO).
- **Deconvolution** additionally needs $\partial q/\partial M=\partial\kappa/\partial M=0$ (evasion separable from true materials), so evaders' materials FOC is also undistorted, and a constant materials elasticity (CD in $M$) so the $\ln D$ difference drops out and $\mathcal V=u-\varepsilon$ exactly. With general technology, deconvolution needs a second flexible, non-deductible input (labour share), as in `56-id-evasion.qmd` ("Identification with two Flexible Inputs", translog). Counterexample: multiplicative evasion ($q(u),\kappa(u)$ with $e=M(\textbf{e}^u-1)$) distorts evaders' materials FOC — it agrees with the additive model at $u=0$ (so testing is unaffected) but not for evaders (slides `200-model.qmd`, "Robustness: Multiplicative Evasion").

**§4.4 overreporting ratio — DONE 2026-09-24 (moved up from post-deadline):** results are reported for the overreporting ratio $x=e/M$, not $u=\ln(1+e/M)$. $f_x(y)=f_u(\ln(1+y))/(1+y)$ from the fitted $f_u$ (slides `700`, Hogg et al. 2019 Thm 1.7.1), no re-estimation. **Sample fixed the same day:** `291-bs-deconv.R` deconvolved $\mathcal V$ pooled over ALL firms (corporations included, $u=0$ by assumption), so its $f_u$ was diluted toward zero and matched the one-sample test, not the preferred two-sample test. New `Code/Deconvolution/292-np-deconv-unincorp.R` reruns the same estimator (same corporate $f_\varepsilon$, knots rule, $\lambda$) on unincorporated firms only → `Code/Products/np_deconv_unincorp.RData`; 291's output left untouched. Asset: `Code/Thesis/ch05-overreporting-ratio.R` (reads 292) → `tables/`+`figures/ch05-overreporting-ratio.png`. Mean $x$ (unincorporated): 331 24.1%, 313 25.2%, 322 20.9%, 321 17.2%, 369 11.3%. Mean $u$ vs. mean $\mathcal V$ (test): 331 0.213/0.244, 322 0.190/0.201, 321 0.158/0.135, 313 0.215/0.176, 369 0.106/0.196 (0.186 after the 10-obs share≥0.75 trim) — 369's gap unexplained, see ch. 5 draft note. Intro/abstract numbers should use $x$.

**§4.3 to-do, AFTER the deadline:** test-inversion CI for mean overreporting, same convention as ch. 6/8. Take $\ln\hat D$ (i.e. $\hat\beta$) from corporations as the truth. Grid a candidate mean $\mu$ of $\mathcal V$ among unincorporated firms ($\mu=E[u]$ since $E[\varepsilon]=0$): 0, 0.01, ..., 1. Moments $g=\big(s-\ln\hat D\;\text{(corporations)},\;s-\ln\hat D-\mu\;\text{(unincorporated)}\big)$, efficient-GMM weighting with plant-clustered covariance, conservative test $2n\hat L_n$ against $\chi^2_{2,.95}$ (nothing profiled). The passing set is a CI for mean overreporting; evasion is detected when it excludes 0. For Friday: report the existing preferred bootstrap test (`pref_tax_ev_test_tbl` in `boot_test_comp_tbl.RData`: plants resampled separately within corporations and within unincorporated firms, $\beta$ from corporations, test on unincorporated only).

**§5 to-do, AFTER the deadline:** (1) conditional deconvolution of $\omega$ by group (exporters, importers, advertisers, wages above the industry median) to report GNR (2020) Table 3's productivity premia; the deconvolution gives only the distribution of $\omega$, not firm-level $\omega$, and firm-level $\widetilde{\mathcal W}$ still contains $(1-\beta)\varepsilon$. (2) Redo $\omega$ and the productivity comparison with the joint efficient-GMM estimates (the final-version specification). Done for Friday: percentile ratios 75/25, 80/20, 90/10, 95/5, skewness, and persistence $\hat\gamma_1$ (`294-omega-persistence.R`: IV at the PF point estimates vs. OLS on GNR's firm-level $\omega$, same row-based lags).

**Mechanics:** JMP-only merge of ch. 4/5 via `when-meta="jmp"` headings (thesis keeps two chapters); include order in `JMP/paper.qmd` already matches (PF before fiscal and counterfactual), no reorder needed; hide ch. 9 in the JMP.

### 10a. JMP intro — RAP 2 tracker (started 2026-09-24)

RAP 2 (tax rates) chosen; options and reasoning in `Thesis/feedback/jmp-intro-rap-options.md`; proof-read reports in `JMP/sections/01-intro-pr-240926.md`; literature map in the draft-only table of `chapters/09-literature.qmd` and the draft-only outline at the end of `JMP/sections/01-intro.qmd`.

- [x] 1 Hook: fake invoices, losses as % of tax revenue, gains at the top
- [x] 2 P: reported-income responses documented, firms' input overreporting not; not observed (proof-read)
- [x] 3 R + A: 1983 reform (~9% by 1987, ST-liable), claims +16–28% for +0.5%, 3% loss, −20 revenue elasticity holding sales tax fixed (proof-read)
- [x] 4 How: PF + corporations + deconvolution + reform + structural model (proof-read)
- [x] 5 Supporting findings: test introduced with rejection wording, PF + dispersion/persistence (OLS and capital/labour dropped; dispersion caveat stays in ch. 6 discussion)
- [x] Test robustness sentence inserted in intro para. 5 (2026-09-25); backed by ch. 4 draft note
- [x] Contribution 1: tax rates and revenue (signpost "three strands… and one of productivity measurement"; knowledge-first style)
- [x] Contribution 2: measuring firm evasion (absorbed old "Despite its relevance…", superseded)
- [x] Contribution 3: validation samples (PF as the indirect measure; corporations as reference group; groups need not be alike in size or productivity)
- [x] Production functions (secondary) paragraph -- final after Hans edit + second proof-read
- [x] Roadmap (follows the include order; REORDERED 2026-10-05 by Hans: setting (file `03-setting-data.qmd`) now precedes the model (file `02-model.qmd`), because ch. 3 is what convinces the reader that corporations are truth-reporters, which the model only assumes; filenames and `Code/Thesis/chNN-*` prefixes kept, so file numbers no longer match chapter numbers; matches §10's table)
- [x] Abstract (`JMP/paper.qmd`) rewritten to RAP 2 (176 words; old testing-first version saved in `JMP/sections/01-intro-pr-240926.md`); Hans editing
- [x] Para. 1 hook reframed around the rate-dependent incentive (fake invoices kept as one example), 2026-09-25; Hans to edit
- [x] Clean up stale draft blocks in the intro file (2026-09-25): intro is now final prose only (23 lines); superseded drafts and notes moved to `Thesis/feedback/jmp-intro-archive-250926.md`; visible prose verified identical, JMP re-rendered
- [x] Leak check on the rendered JMP (2026-09-25, non-draft render): one leak fixed (appendix A comment attached to a list item rendered as text; needs a blank line before `<!--`), plus a literal `{,}` in A.5 and a capital "Define" in ch. 8. Clean after re-render. Thesis book scanned too (2026-09-25): no leaks; thesis-only parts still stubs (ch. 1 intro is the old one-paragraph opening, ch. 9 and 10 have headings only, abstract in `index.qmd` predates RAP 2).
- [x] Intro para. 1 framing settled (Hans, 2026-09-25)
- [x] Conclusion (ch. 10) outline realigned to RAP 2 (2026-09-25); old RAP 3 outline in `Thesis/feedback/conclusion-rap3-outline-archive-250926.md`. Prose is Hans's to write

## 8. Log

- 2026-09-21: plan agreed; decisions above; tracker created.
- 2026-09-21: scaffold created (`_quarto.yml`, `index.qmd`, 9 chapter files, 3 appendix stubs); bib merged; ch. 7 and 8 written as outlines with key values and live figure/table calls; HTML render clean, all cross-refs resolve. Stubs carry the labels other chapters cite (`sec-model`, `sec-setting`, `sec-deconvolution`, `sec-pf`, `sec-app-*`).
- 2026-09-22: `Code/Thesis/` scaffolded (`001-setup.R`, `000-build-all.R`, `manifest.csv`). Ch. 3 (setting and data) built end to end: outline written, all 4 tables ported and rendering clean. Found Hans's own `ch06-pf-comparison.R`/`ch06-pf-testinv-regions.R` already in `Code/Thesis/` (new joint-GMM PF results, industries 331/322/369/313/321) and aligned them to the shared setup. Standardized on tinytable+etable (tables) / ggplot2+base-persp (figures) per chat. Rebuilt `ch03-summary-stats-table.R` from scratch without `modelsummary` (not installed); split its one table into two (numeric skim + J.Org. composition) per Hans's call in chat. Two real bugs caught and fixed in that process: unescaped `%`/`&` in tinytable cells/labels silently corrupts the render, and `style_tt(i=...)` after `group_tt()` needs post-insertion row indices -- both now documented in §1 as standing conventions. Full book HTML render clean throughout.
- 2026-09-22: full PDF render checked for the first time (`Thesis/_book/Tax-Evasion-and-Productivity.pdf`, 38pp) -- found and fixed the actual cause of inconsistent table sizing (untagged PNG resolution -> pdflatex assumes 72dpi -> Quarto's `\pandocbounded` clamps everything to full page width regardless of content) by adding `-units PixelsPerInch` to `050-render-tbls.R`'s `magick` call. Added deliberate size control on top: `render_png_tt_tbl()`/`render_png_etbl()` now take `linewidth_pt`, pinning each table's `\linewidth` to the book's real text width (446.76pt/6.18in) so `tt(width=<fraction>)` is meaningful; project default `width=1` for every table. Caught the equal-column-split side effect (a long-text column wrapping 7 lines next to short numeric ones) and fixed with `width=`'s per-column-vector form. Dropped `caption=` from every `tt()` call per Hans's call (Quarto's own `![cap](path){#tbl-x}` is now the single caption source; real notes moved to `notes=`), and moved captions into the ch. 3 qmd accordingly. Also caught a 3rd/4th unescaped-`%` instance and a new bug (literal `_` in a raw column name reads as a LaTeX subscript trigger and garbles) while fixing ch03-top-industries/corps-by-industry. All fixes documented as standing conventions in §1. Full PDF re-render clean.
- 2026-09-22: M/M* notation-flip request raised (project convention has the star backwards vs. the literature -- want $M^*$=true/unobserved, $M$=observed). Scoped (Quarto-Slides/ + Thesis/ in, Paper/ out, CLAUDE.md and code/data names still open) then explicitly deferred to after the Friday deadline -- see §7a. No files touched for this item.
- 2026-09-22: **priority flip -- Fri 2026-09-25 is the JMP deadline, not the thesis** (thesis submits next summer). Scaffolded `JMP/` as described in §9: standalone `default`-type Quarto project, `paper.qmd` includes the same `Thesis/chapters/*.qmd`/`appendices/*.qmd` files (no fork), `figures`/`tables`/`biblio` symlinked to `Thesis/`. Rewrote every chapter's image path from `../figures|tables/` to `/figures|tables/` (root-relative) so the same file resolves correctly from both projects -- verified with a minimal two-project mock before touching real files, then confirmed on the real book (`quarto render --to html`, clean, images present in `_book/`) and the real JMP (`quarto render paper.qmd --to pdf`, clean, 25pp at current stub length). Added `Thesis/chapters/10-conclusion.qmd` (new stub, neither document had one) to both `_quarto.yml` and `paper.qmd`. First real PDF confirms the target layout: no title page, title/author/abstract/JEL/keywords/Introduction all flow from page 1, appendix letters (A/B/C) correct. Still open: abstract rewrite (shared stale TODO text), `thanks`/JEL placeholders in `JMP/paper.qmd` YAML, and whether ch. 9 (literature) should move earlier in the JMP's own include order.
- 2026-09-22 (later): **ch. 8's remaining `[~]` tables and figures moved to `[x]` done** -- all 3 tables and 6 figures now have real `Code/Thesis/ch08-*.R` scripts. Hit the magick/pandoc PNG-sizing bug again (this time in `save_thesis_base_plot()` itself, previously undiscovered) plus a related-but-different device-canvas-vs-native-size layout bug -- **both now written up as a standing, consolidated reference in §1** ("PNG asset sizing in the Quarto/LaTeX pipeline") rather than left as a one-off fix buried in this log entry; check there first if a rendered asset ever looks the wrong size again. Also fixed a small cosmetic glyph bug (a base-R title with a literal `"β̂"` string mis-rendered; switched to `bquote(hat(beta))` plotmath). Full book and JMP PDF re-renders both clean, no broken glyphs anywhere in either document.
- 2026-09-23/24: **ch. 8 proof-read and revised; ch. 7 drafted from existing write-ups; ch. 3 extended.**
  - **Ch. 8:** report `chapters/08-counterfactual-pr-230926.md` (3 revisions). Dropped the CV-adjusted revenue and the soft test (appendix B out of both builds); the conservative test is now the only test named; Claims is defined in the design section; single-tax story in visible text (two-tax draft-only, §7b); densities as $f_X$.
  - **Ch. 7:** prose pasted from `965-DiD` + slides (source map in `REUSE-LOG.md`), then rewritten. Outcome is the log share $s$ with $\ln\beta_j$ industry FE (text matches code). It is not a traditional DiD: identifying assumptions (i)–(iii), no parallel trends. The 1983 income tax was CUT (Perry & Cárdenas 1986; the old "+8%" was a misreading). Results lead with ST-liable industries; the exempt-proprietorship rise is flagged as unexpected, with two explanations. SE levels vs. differences are explained mechanically (Salvador). New intro P1–P2 (sources verified against Slemrod 2019 WP). Takeaways written; four coefficient tables and figure captions added; "CIT" became "income tax/IT" thesis-wide.
  - **Ch. 3:** EAM timing (activity year, DANE 1995 p. 40); reform dates (Decreto 3541, 1 Apr 1984; Ley 49/1990); `tbl-st-by-year`, `tbl-inc-tax-1983`, `tbl-marg-tax-1983`.
  - **Conventions added to §1:** conservative test, Canadian spelling, notation. **Parked:** 1986 reform, cutting tables at 1985, JMP font (renders in Latin Modern, not Times; `mathptmx` is pdflatex-only). **Next:** Hans re-reads, then the intro/abstract consistency pass for ch. 7.
- 2026-09-24: internal notes hidden (draft-only) across chapters, appendix A, JMP intro. Ch. 4–6 ported from the approved paper and older write-ups; new assets `ch04-evasion-test`, `ch05-overreporting-ratio`, `ch06-pf-comparison` (rebuilt), `ch06-productivity-comparison`; `292`–`294` scripts (unincorporated-only deconvolution, ω with current PF, persistence). Ch. 8 drops the untilded-𝒲 ELVIS column. Ch. 2 model-only; ch. 10 outline (RAP 3). Ch. 7 figures rebuilt. Abstract/intro: 10% coverage wording, 11–25% range, inconclusive negatives, net-incentive reform result. **Next:** Hans's prose in draft notes (ch. 2, 4–6, 10); ch. 8 consistency pass (29-industry scope, ε as measurement error); intro vs. conclusion argument; `thanks:` placeholder; abstract typos.
- 2026-09-25: JMP intro + abstract rebuilt on RAP 2 (tax rates); all visible intro paragraphs drafted and proof-read (§10a); Appendix E (all-industry PF estimates) added; ch. 4 test-robustness note, ch. 6 half-lives + third caveat, ch. 8 all-industries scope; literature table in ch. 9 (draft-only); 12 bib entries added. Deadline may slip to Mon 2026-09-28. **Next:** intro draft-block cleanup, leak check on non-draft render, ch. 10 → RAP 2, intro para. 1 fake-invoice framing.
- 2026-09-27/28: **ch. 3 §3.3–3.4 proof-read and redrafted** (report `chapters/03-setting-data-pr-270926.md`). §3.3: corporations-as-truth-reporters paragraph rebuilt on two reasons (government and market scrutiny; dividend margin dropped), Carrillo 1.7 vs 11.5 percent is corps vs *sole proprietorships*, auditor from the Fiscal Survey (not "on-site"; "Superintendent" is the book's term), bias direction = lower bound; definitions paragraph with DANE (2018, p. 12) cites, partnerships' liability and the 1995 *empresa unipersonal* footnote (Ley 222); income-tax paragraph reordered (corporations last, scrutiny answers their 40 percent rate). New `tbl-jo-rules` (`Code/Thesis/ch03-jo-rules-table.R`, replaces the markdown table). §3.4: takeaway paragraph first (across industries via sales tax, within via income tax by JO); Bird–Wiesner debate moved to ch. 7 only (recommendations footnote, Bolaños p. 723). Bib: McLure, De Loecker, Orozco de Triana names; Spanish titles `langid`; bad DOIs; Perry–Cárdenas locators as `1:23`; `Ley2221995`; year-only cites after naming the Fiscal Survey/DANE. **Convention: "percent" in running text (AEA), "%" only in tables/figures/math** — converted thesis + JMP. Kept `.tex` now git-ignored. New skills: `draft-prose` (drafting, self-checks with `proof-read`); `proof-read` gained a target-audience section. Test results now reported by sales-tax liability: 9 of 18 liable industries, neither exempt one (exempt = 311, 312, verified in the data); abstracts, intros, ch. 4 notes, ch. 10 updated.
